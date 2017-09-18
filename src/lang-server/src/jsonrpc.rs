use std::collections::HashMap;
use languageserver_types as lst;
use serde::Serialize;
use serde::de::DeserializeOwned;
use serde_json::{self, Value as JsonValue};


#[derive(Deserialize)]
struct Request {
    jsonrpc: String,
    id: Option<lst::NumberOrString>,
    method: String,
    params: JsonValue,
}

#[derive(Serialize)]
pub struct ErrorResponse<T> {
    code: i64,
    message: String,
    #[serde(skip_serializing_if="Option::is_none")]
    data: Option<T>,
}

impl<T> ErrorResponse<T> {
    fn parse_error() -> Self {
        ErrorResponse {
            code: -32_700,
            message: "Parse error".into(),
            data: None,
        }
    }

    fn invalid_request() -> Self {
        ErrorResponse {
            code: -32_600,
            message: "Invalid request".into(),
            data: None,
        }
    }

    fn method_not_found() -> Self {
        ErrorResponse {
            code: -32_601,
            message: "Method not found".into(),
            data: None,
        }
    }

    fn invalid_params() -> Self {
        ErrorResponse {
            code: -32_602,
            message: "Invalid params".into(),
            data: None,
        }
    }

    fn internal_error() -> Self {
        ErrorResponse {
            code: -32_603,
            message: "Internal error".into(),
            data: None,
        }
    }

    pub fn new<S: Into<String>>(code: i64, msg: S, data: Option<T>) -> Self {
        ErrorResponse {
            code,
            message: msg.into(),
            data,
        }
    }
}

impl<T: Serialize> ErrorResponse<T> {
    fn serialize_error(self) -> ErrorResponse<JsonValue> {
        let ErrorResponse { code, message, data } = self;
        let data = if let Some(value) = data {
            match serde_json::to_value(&value) {
                Ok(value) => Some(value),
                Err(_) => {
                    // failed to serialize error value,
                    // just respond with internal JSON-RPC error
                    return ErrorResponse::internal_error();
                }
            }
        } else {
            None
        };
        ErrorResponse {
            code,
            message,
            data,
        }
    }
}

#[derive(Serialize)]
struct RawResponse<T, E> {
    jsonrpc: &'static str,
    #[serde(skip_serializing_if="Option::is_none")]
    result: Option<T>,
    #[serde(skip_serializing_if="Option::is_none")]
    error: Option<ErrorResponse<E>>,
    id: Option<lst::NumberOrString>,
}

impl<T: Serialize, E: Serialize> RawResponse<T, E> {
    fn ok(result: T, id: Option<lst::NumberOrString>) -> Self {
        RawResponse {
            jsonrpc: "2.0",
            result: Some(result),
            error: None,
            id,
        }
    }

    fn err(error: ErrorResponse<E>, id: Option<lst::NumberOrString>) -> Self {
        RawResponse {
            jsonrpc: "2.0",
            result: None,
            error: Some(error),
            id,
        }
    }

    fn serialize(&self) -> JsonValue {
        serde_json::to_value(&self).unwrap_or_else(|_| {
            let err = RawResponse::<(), ()>::err(ErrorResponse::internal_error(), None);
            serde_json::to_value(&err)
                .expect("failed to serialize basic response")
        })
    }
}

pub enum Response<T, E> {
    Success(T),
    Error(ErrorResponse<E>),
}

impl<T, E> Response<T, E> {
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Response<U, E> {
        match self {
            Response::Success(value) => Response::Success(f(value)),
            Response::Error(err) => Response::Error(err),
        }
    }
}

impl<T: Serialize, E: Serialize> Response<T, E> {
    fn into_raw(self, id: Option<lst::NumberOrString>) -> RawResponse<T, E> {
        match self {
            Response::Success(value) => {
                RawResponse::ok(value, id)
            }
            Response::Error(err) => {
                RawResponse::err(err, id)
            }
        }
    }
}

type CallHandler<'a, T, E> = Box<Fn(JsonValue) -> Option<Response<T, E>> + 'a>;

#[derive(Default)]
pub struct RpcHandler<'a> {
    handlers: HashMap<String, CallHandler<'a, JsonValue, JsonValue>>,
}

impl<'a> RpcHandler<'a> {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn add_method<S, F, T, U, E>(&mut self, name: S, handle: F)
        where
            F: Fn(T) -> Response<U, E> + 'a,
            T: DeserializeOwned,
            U: Serialize,
            E: Serialize,
            S: Into<String>
    {
        self.add_handler(name, move |input| {
            handle(input).map(Some)
        });
    }

    pub fn add_notification<S, F, T>(&mut self, name: S, handle: F)
        where
            F: Fn(T) + 'a,
            T: DeserializeOwned,
            S: Into<String>
    {
        self.add_handler::<_, _, _, (), ()>(name, move |input| {
            handle(input);
            Response::Success(None)
        });
    }

    fn add_handler<S, F, T, U, E>(&mut self, name: S, handle: F)
        where
            F: Fn(T) -> Response<Option<U>, E> + 'a,
            T: DeserializeOwned,
            U: Serialize,
            E: Serialize,
            S: Into<String>
    {
        let handle = move |params| {
            let params = match serde_json::from_value::<T>(params) {
                Ok(params) => params,
                Err(_) => {
                    let response = ErrorResponse::invalid_params();
                    return Some(Response::Error(response));
                }
            };
            match handle(params) {
                Response::Success(Some(response)) => {
                    let json = serde_json::to_value(&response).unwrap();
                    Some(Response::Success(json))
                }
                Response::Success(None) => {
                    None
                }
                Response::Error(err) => {
                    Some(Response::Error(err.serialize_error()))
                }
            }
        };
        self.handlers.insert(name.into(), Box::new(handle));
    }

    pub fn handle_call(&mut self, input: &str) -> Option<String> {
        let value = match serde_json::from_str::<JsonValue>(input) {
            Ok(value) => value,
            Err(_) => return basic_error(&ErrorResponse::parse_error()),
        };

        let response = match value {
            JsonValue::Array(values) => {
                // batch request
                if values.is_empty() {
                    return basic_error(&ErrorResponse::invalid_request());
                }
                let mut responses = Vec::new();
                for request in values {
                    if let Some(response) = self.handle_request(request) {
                        responses.push(response.serialize());
                    }
                }
                if responses.is_empty() {
                    None
                } else {
                    Some(JsonValue::Array(responses))
                }
            }
            value => {
                // single request
                self.handle_request(value).map(|r| r.serialize())
            }
        };
        response.map(|value| serde_json::to_string(&value)
            .expect("failed to serialize json value to string"))
    }

    fn handle_request(&mut self, request: JsonValue) -> Option<RawResponse<JsonValue, JsonValue>> {
        let request = match serde_json::from_value::<Request>(request) {
            Ok(request) => request,
            Err(_) => return Some(RawResponse::err(ErrorResponse::invalid_request(), None)),
        };
        if request.id.is_some() {
            debug!("handling method: '{}'", request.method);
        } else {
            debug!("handling notification: '{}'", request.method);
        }
        match self.handlers.get_mut(&request.method) {
            Some(handler) => {
                let id = request.id;
                handler(request.params).map(|resp| resp.into_raw(id))
            }
            None => {
                error!("handler not found for: '{}'", request.method);
                // only reply to requests, not notifications
                if request.id.is_some() {
                    Some(RawResponse::err(ErrorResponse::method_not_found(), request.id))
                } else {
                    None
                }
            }
        }
    }
}

fn basic_error(err: &ErrorResponse<()>) -> Option<String> {
    Some(serde_json::to_string(err).expect("failed to serialize basic error"))
}
