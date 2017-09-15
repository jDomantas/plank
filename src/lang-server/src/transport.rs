use std::convert::From;
use std::io;
use std::io::prelude::*;


#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    BadInput(&'static str),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Error::Io(err)
    }
}

const LENGTH_HEADER: &'static str = "Content-Length:";

#[derive(Debug)]
pub struct Transport<R, W> {
    read_buffer: String,
    reader: R,
    writer: W,
}

impl<R: BufRead, W: Write> Transport<R, W> {
    pub fn new(reader: R, writer: W) -> Self {
        Transport {
            read_buffer: String::new(),
            reader,
            writer,
        }
    }

    pub fn read_message(&mut self) -> Result<String, Error> {
        let mut content_length = None;
        loop {
            self.read_buffer.clear();
            self.reader.read_line(&mut self.read_buffer)?;
            if self.read_buffer.starts_with(LENGTH_HEADER) {
                let len_str = self.read_buffer.get(LENGTH_HEADER.len()..).unwrap().trim();
                let length = str::parse::<u64>(&len_str)
                    .map_err(|_| Error::BadInput("bad length"))?;
                content_length = Some(length);
            } else if self.read_buffer == "\r\n" {
                break;
            } else if self.read_buffer.is_empty() {
                return Err(Error::BadInput("unexpected eof"));
            }
        }
        let content_length = content_length
            .ok_or(Error::BadInput("no content length"))?;
        self.read_buffer.clear();
        (&mut self.reader).take(content_length).read_line(&mut self.read_buffer)?;
        Ok(self.read_buffer.clone())
    }

    pub fn send_message(&mut self, msg: &str) -> Result<(), Error> {
        debug!("sending: {}", msg);
        write!(self.writer, "{} {}\r\n\r\n", LENGTH_HEADER, msg.len())?;
        self.writer.write_all(msg.as_bytes())?;
        self.writer.flush()?;
        Ok(())
    }
}

