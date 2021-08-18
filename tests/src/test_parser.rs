#[derive(Debug)]
pub enum ParseError {
    MalformedBinary(&'static str),
    DuplicateBinary(&'static str),
    NoOutput,
    ErrorsAndIo,
}

impl ::std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        use self::ParseError::*;
        match *self {
            MalformedBinary(name) => write!(f, "annotation `{}` is malformed", name),
            DuplicateBinary(name) => write!(f, "annotation `{}` appears multiple times", name),
            NoOutput => write!(f, "input is provided but not output"),
            ErrorsAndIo => write!(f, "test provides both build errors and io"),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub line: u32,
    pub message: String,
}

#[derive(Debug)]
pub enum Expectation {
    /// Build should fail, and all given errors must be present.
    BuildErrors(Vec<Error>),
    /// Build should succeed, and when ran with given
    /// input program should produce given output.
    Io { input: Vec<u8>, output: Vec<u8> },
    /// Build should succeed, but program execution is not tested.
    BuildSuccess,
}

fn get_errors(source: &str) -> Vec<Error> {
    const ANNOTATION: &'static str = "// ERROR: ";
    let mut errors = Vec::new();
    for (line_num, line) in source.lines().enumerate() {
        if let Some((index, _)) = line.match_indices(ANNOTATION).next() {
            let message_from = index + ANNOTATION.len();
            let message = line[message_from..].into();
            errors.push(Error {
                line: line_num as u32,
                message,
            })
        }
    }
    errors
}

fn decode_bytes(mut from: &str) -> Result<Vec<u8>, ()> {
    let mut result = Vec::new();
    while let Some(ch) = from.chars().next() {
        match ch {
            '\\' => {
                if from.chars().skip(1).next() != Some('x') {
                    return Err(());
                }
                let b1 = from
                    .chars()
                    .skip(2)
                    .next()
                    .ok_or(())?
                    .to_digit(16)
                    .ok_or(())? as u8;
                let b2 = from
                    .chars()
                    .skip(3)
                    .next()
                    .ok_or(())?
                    .to_digit(16)
                    .ok_or(())? as u8;
                result.push((b1 << 4) + b2);
                from = &from[4..];
            }
            _ => {
                if ch as u32 >= 127 || (ch as u32) < 32 {
                    return Err(());
                }
                result.push(ch as u8);
                from = &from[1..];
            }
        }
    }
    Ok(result)
}

fn get_io(source: &str, name: &'static str) -> Result<Option<Vec<u8>>, ParseError> {
    let mut annotation = None;
    let pattern = format!("// {}: ", name);
    for line in source.lines() {
        if let Some((index, _)) = line.match_indices(&pattern).next() {
            let from = index + pattern.len();
            let bytes =
                decode_bytes(&line[from..]).map_err(|()| ParseError::MalformedBinary(name))?;
            if annotation.is_some() {
                return Err(ParseError::DuplicateBinary(name));
            }
            annotation = Some(bytes);
        }
    }
    Ok(annotation)
}

pub fn parse_test(source: &str) -> Result<Expectation, ParseError> {
    let errors = get_errors(source);
    let input = get_io(source, "INPUT")?;
    let output = get_io(source, "OUTPUT")?;
    if output.is_none() && input.is_some() {
        return Err(ParseError::NoOutput);
    }
    if (output.is_some() || input.is_some()) && !errors.is_empty() {
        return Err(ParseError::ErrorsAndIo);
    }
    Ok(if !errors.is_empty() {
        Expectation::BuildErrors(errors)
    } else if input.is_some() || output.is_some() {
        Expectation::Io {
            input: input.unwrap_or_else(Vec::new),
            output: output.unwrap_or_else(Vec::new),
        }
    } else {
        Expectation::BuildSuccess
    })
}
