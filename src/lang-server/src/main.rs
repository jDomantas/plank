#[macro_use]
extern crate serde_derive;
extern crate serde;
extern crate serde_json;
extern crate languageserver_types;
#[macro_use]
extern crate log;
extern crate simple_logging;
extern crate url;
extern crate syntax;
extern crate errors;

mod transport;
mod jsonrpc;

use std::cell::RefCell;
use std::io;
use std::io::{BufRead, Write};
use std::ops::DerefMut;
use languageserver_types as lst;
use url::Url;
use jsonrpc::{RpcHandler, Response};
use transport::Transport;


fn main() {
    simple_logging::log_to_stderr(log::LogLevelFilter::Trace)
        .expect("failed to initialize logging");
    
    let stdin = io::stdin();
    let stdin = stdin.lock();
    let stdout = io::stdout();
    let stdout = stdout.lock();

    let transport = RefCell::new(Transport::new(stdin, stdout));
    let mut rpc = RpcHandler::new();

    rpc.add_method("initialize", |_: lst::InitializeParams| {
        Response::Success::<_, ()>(lst::InitializeResult {
            capabilities: lst::ServerCapabilities {
                text_document_sync: Some(lst::TextDocumentSyncKind::Full),
                .. Default::default()
            },
        })
    });

    rpc.add_notification("initialized", |_: serde_json::Value| {
    });

    rpc.add_notification("textDocument/didOpen", |params: lst::DidOpenTextDocumentParams| {
        publish_diagnostics(
            &params.text_document.text,
            params.text_document.uri,
            transport.borrow_mut().deref_mut(),
        );
    });

    rpc.add_notification("textDocument/didSave", |_: lst::DidSaveTextDocumentParams| {
    });

    rpc.add_notification("textDocument/didClose", |_: lst::DidCloseTextDocumentParams| {
    });

    rpc.add_notification("textDocument/didChange", |params: lst::DidChangeTextDocumentParams| {
        if params.content_changes.len() != 1 ||
            params.content_changes[0].range.is_some() ||
            params.content_changes[0].range_length.is_some()
        {
            debug!("got bad edit with `didChange` event");
        } else {
            publish_diagnostics(
                &params.content_changes[0].text,
                params.text_document.uri,
                transport.borrow_mut().deref_mut(),
            );
        }
    });

    loop {
        let response = {
            let msg = transport
                .borrow_mut()
                .read_message()
                .expect("failed to read message");
            rpc.handle_call(&msg)
        };
        if let Some(response) = response {
            transport
                .borrow_mut()
                .send_message(&response)
                .expect("failed to send");
        }
    }
}

fn publish_diagnostics<R, W>(source: &str, doc: Url, transport: &mut Transport<R, W>)
    where R: BufRead, W: Write
{
    let diagnostics = make_diagnostics(source);
    let params = lst::PublishDiagnosticsParams {
        uri: doc,
        diagnostics,
    };
    #[derive(Serialize)]
    struct Notification {
        jsonrpc: &'static str,
        method: &'static str,
        params: lst::PublishDiagnosticsParams,
    }

    let notification = Notification {
        jsonrpc: "2.0",
        method: "textDocument/publishDiagnostics",
        params,
    };
    let string = serde_json::to_string(&notification)
        .expect("failed to serialize diagnostics");
    transport.send_message(&string).expect("failed to write message");
}

fn make_diagnostics(source: &str) -> Vec<lst::Diagnostic> {
    let reporter = errors::Reporter::new();
    let tokens = syntax::lex(source, reporter.clone());
    let _ = syntax::parse(tokens, reporter.clone());
    reporter
        .get_diagnostics()
        .into_iter()
        .map(|d| note_to_diagnostic(&d.notes[0], d.message))
        .collect()
}

fn note_to_diagnostic(note: &errors::reporter::Note, msg: String) -> lst::Diagnostic {
    fn convert_pos(pos: errors::position::Position) -> lst::Position {
        lst::Position {
            line: (pos.line - 1) as u64,
            character: (pos.column - 1) as u64,
        }
    }
    fn convert_range(range: errors::position::Span) -> lst::Range {
        lst::Range {
            start: convert_pos(range.start),
            end: convert_pos(range.end),
        }
    }
    lst::Diagnostic {
        range: convert_range(note.span),
        severity: Some(lst::DiagnosticSeverity::Error),
        code: None,
        source: Some("plank".into()),
        message: msg,
    }
}
