use lalrpop_util;

use std::convert::AsRef;
use std::collections::HashMap;
use std::fs::OpenOptions;
use std::hash::Hash;
use std::io;
use std::io::Read;
use std::path::{Path, PathBuf};

use self::token::{Tokenizer, Tok};
use super::term::*;

mod arvo;
mod token;

type InternalError<'input> = lalrpop_util::ParseError<usize, Tok<'input>, token::Error>;

#[derive(Debug)]
pub struct Error {
    message: String,
}

pub struct Parser {
    source: String,
    // only present if code is parsed from file
    file: Option<PathBuf>
}

// fn group_by<K: Hash + Eq, V, I: Iterator<Item=(K, V)>>(iter: I) -> HashMap<K, Vec<V>> {
//     let mut map = HashMap::new();
//
//     for (k, v) in iter {
//         let mut vec = match map.remove(&k) {
//             None => vec![],
//             Some(vec) => vec,
//         };
//
//         vec.push(v);
//         map.insert(k, vec);
//     }
//
//     return map;
// }
//
// fn convert_to_path_buf(ast_path: Vec<Atom>) -> PathBuf {
//     ast_path.into_iter()
//             .map(|a| a.repr)
//             .fold(PathBuf::new(), |path, seg| path.join(seg))
// }

impl Parser {
    pub fn new(source: String) -> Parser {
        Parser {
            source: source,
            file: None,
        }
    }

    pub fn parse_program(&self) -> Result<Program, Error> {
        let tokenizer = Tokenizer::new(&self.source[..]);
        arvo::parse_Program(&self.source[..], tokenizer)
            .map_err(|e| self.construct_error(e))
    }

    pub fn parse_command(&self) -> Result<Command, Error> {
        let tokenizer = Tokenizer::new(&self.source[..]);
        arvo::parse_Command(&self.source[..], tokenizer)
            .map_err(|e| self.construct_error(e))
    }

    pub fn from_file<P: AsRef<Path>>(path: P) -> io::Result<Parser> {
        let file_path = path.as_ref().to_owned();

        let mut file = try!(OpenOptions::new()
                                .read(true)
                                .open(path));

        let mut buffer = String::new();
        try!(file.read_to_string(&mut buffer));

        let mut p = Parser::new(buffer);
        p.file = Some(file_path);
        Ok(p)
    }

    // This is some crappy code ripped from something else I'm working on.
    // We need to fully span-ify the AST and build an AST map allowing us
    // to map from indicies into a source map giving a line number and
    // column number enabling better error reporting.
    fn construct_error(&self, err: InternalError) -> Error {
        use lalrpop_util::ParseError::*;

        match err {
            UnrecognizedToken { token, expected } => {
                let file = self.file.clone().map(|p| p.display()
                                    .to_string())
                                    .unwrap_or("interactive".to_owned());

                let (lo, tok, hi) = token.unwrap();
                // Should do something better here, fine for now.
                // This doesn't work because indexing splits the String
                // in between character points. 
                let start = if lo <= 10 {
                    0
                } else {
                    lo - 10
                };

                let end = hi + 10;

                let body = &self.source[lo..hi];
                let message = format!("{}:{}:{}: {}", file, lo, hi, body);
                Error { message: message }
            }
            e => Error { message: format!("{:?}", e) }
        }
    }
}
