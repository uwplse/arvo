//! A tokenizer twice adapted for Arvo
use std::str::CharIndices;

use self::Error::*;
use self::Tok::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Error {
    UnrecognizedToken(usize),
    UnterminatedEscape(usize),
    UnterminatedStringLiteral(usize),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Tok<'input> {
    // Keywords;
    Def,
    Axiom,
    Import,
    Print,
    Check,
    Simpl,
    Data,

    // Identifiers of various kinds:
    Escape(&'input str),
    Atom(&'input str),
    Var(&'input str),
    MacroId(&'input str), // identifier followed immediately by `<`
    Lifetime(&'input str), // includes the `'`
    StringLiteral(&'input str), // excludes the `"`

    // Symbols:
    Ampersand,
    BangEquals,
    BangTilde,
    Bar,
    Backslash,
    Colon,
    ColonColon,
    ColonEquals,
    Comma,
    Dot,
    Equals,
    EqualsEquals,
    GreaterThan,
    LeftBrace,
    LeftBracket,
    LeftParen,
    LessThan,
    Lookahead, // @L
    Lookbehind, // @R
    Plus,
    Question,
    RightArrow,
    RightBrace,
    RightBracket,
    RightParen,
    Semi,
    Star,
    TildeTilde,
    Type,
    Underscore,
}

pub struct Tokenizer<'input> {
    text: &'input str,
    chars: CharIndices<'input>,
    lookahead: Option<(usize, char)>,
}

macro_rules! eof {
    ($x:expr) => {
        match $x { Some(v) => v, None => { return None; } }
    }
}

pub type Spanned<T> = (usize, T, usize);

const KEYWORDS: &'static [(&'static str, Tok<'static>)] = &[
    ("def", Def),
    ("axiom", Axiom),
    ("import", Import),
    ("print", Print),
    ("check", Check),
    ("simpl", Simpl),
    ("data", Data),
    ("Type", Type),
    ];

impl<'input> Tokenizer<'input> {
    pub fn new(text: &'input str) -> Tokenizer<'input> {
        let mut t = Tokenizer {
            text: text,
            chars: text.char_indices(),
            lookahead: None
        };
        t.bump();
        t
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        self.lookahead = self.chars.next();
        self.lookahead
    }

    fn escape(&mut self, idx0: usize) -> Result<Spanned<Tok<'input>>, Error> {
        match self.take_until(|c| c == '`') {
            Some(idx1) => {
                self.bump(); // consume the '`'
                let text: &'input str = &self.text[idx0+1..idx1]; // do not include the `` in the str
                Ok((idx0, Escape(text), idx1+1))
            }
            None => {
                Err(UnterminatedEscape(idx0))
            }
        }
    }

    fn string_literal(&mut self, idx0: usize) -> Result<Spanned<Tok<'input>>, Error> {
        let mut escape = false;
        let terminate = |c: char| {
            if escape {
                escape = false;
                false
            } else if c == '\\' {
                escape = true;
                false
            } else if c == '"' {
                true
            } else {
                false
            }
        };
        match self.take_until(terminate) {
            Some(idx1) => {
                self.bump(); // consume the '"'
                let text = &self.text[idx0+1..idx1]; // do not include the "" in the str
                Ok((idx0, StringLiteral(text), idx1+1))
            }
            None => {
                Err(UnterminatedStringLiteral(idx0))
            }
        }
    }

    fn lifetime(&mut self, idx0: usize) -> Spanned<Tok<'input>> {
        let (start, word, end) = self.word(idx0);
        (start, Lifetime(word), end)
    }

    fn identifierish(&mut self, idx0: usize) -> Spanned<Tok<'input>> {
        let (start, word, end) = self.word(idx0);

        let tok =
            // search for a keyword first; if none are found, this is
            // either a MacroId or an Id, depending on whether there
            // is a `<` immediately afterwards
            KEYWORDS.iter()
                    .filter(|&&(w, _)| w == word)
                    .map(|&(_, ref t)| t.clone())
                    .next()
                    .unwrap_or_else(|| {
                        match self.lookahead {
                            Some((_, '<')) => MacroId(word),
                            _ => {
                                // check if atom or variable
                                let first = word.chars().next().unwrap();
                                if ('A' <= first) && (first <= 'Z') {
                                    Var(word)
                                } else {
                                    Atom(word)
                                }
                            }
                        }
                    });

        (start, tok, end)
    }

    fn word(&mut self, idx0: usize) -> Spanned<&'input str> {
        match self.take_while(is_identifier_continue) {
            Some(end) => (idx0, &self.text[idx0..end], end),
            None => (idx0, &self.text[idx0..], self.text.len()),
        }
    }

    fn take_while<F>(&mut self, mut keep_going: F) -> Option<usize>
        where F: FnMut(char) -> bool
    {
        self.take_until(|c| !keep_going(c))
    }

    fn take_until<F>(&mut self, mut terminate: F) -> Option<usize>
        where F: FnMut(char) -> bool
    {
        loop {
            match self.lookahead {
                None => {
                    return None;
                }
                Some((idx1, c)) => {
                    if terminate(c) {
                        return Some(idx1);
                    } else {
                        self.bump();
                    }
                }
            }
        }
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<Spanned<Tok<'input>>, Error>;

    fn next(&mut self) -> Option<Result<Spanned<Tok<'input>>, Error>> {
        loop {
            return match self.lookahead {
                Some((idx0, '&')) => {
                    self.bump();
                    Some(Ok((idx0, Ampersand, idx0+1)))
                }
                Some((idx0, '|')) => {
                    self.bump();
                    Some(Ok((idx0, Bar, idx0+1)))
                }
                Some((idx0, '\\')) => {
                    self.bump();
                    Some(Ok((idx0, Backslash, idx0+1)))
                }
                Some((idx0, '!')) => {
                    match self.bump() {
                        Some((idx1, '=')) => {
                            self.bump();
                            Some(Ok((idx0, BangEquals, idx1+1)))
                        }
                        Some((idx1, '~')) => {
                            self.bump();
                            Some(Ok((idx0, BangTilde, idx1+1)))
                        }
                        _ => {
                            Some(Err(UnrecognizedToken(idx0)))
                        }
                    }
                }
                Some((idx0, ':')) => {
                    match self.bump() {
                        Some((idx1, ':')) => {
                            self.bump();
                            Some(Ok((idx0, ColonColon, idx1+1)))
                        }
                        Some((idx1, '=')) => {
                            self.bump();
                            Some(Ok((idx0, ColonEquals, idx1+1)))
                        }
                        _ => {
                            Some(Ok((idx0, Colon, idx0+1)))
                        }
                    }
                }
                Some((idx0, ',')) => {
                    self.bump();
                    Some(Ok((idx0, Comma, idx0+1)))
                }
                Some((idx0, '.')) => {
                    self.bump();
                    Some(Ok((idx0, Dot, idx0+1)))
                }
                Some((idx0, '=')) => {
                    match self.bump() {
                        Some((idx1, '=')) => {
                            self.bump();
                            Some(Ok((idx0, EqualsEquals, idx1+1)))
                        }
                        _ => {
                            Some(Ok((idx0, Equals, idx0+1)))
                        }
                    }
                }
                Some((idx0, '>')) => {
                    self.bump();
                    Some(Ok((idx0, GreaterThan, idx0+1)))
                }
                Some((idx0, '{')) => {
                    self.bump();
                    Some(Ok((idx0, LeftBrace, idx0+1)))
                }
                Some((idx0, '[')) => {
                    self.bump();
                    Some(Ok((idx0, LeftBracket, idx0+1)))
                }
                Some((idx0, '(')) => {
                    self.bump();
                    Some(Ok((idx0, LeftParen, idx0+1)))
                }
                Some((idx0, '<')) => {
                    self.bump();
                    Some(Ok((idx0, LessThan, idx0+1)))
                }
                Some((idx0, '@')) => {
                    match self.bump() {
                        Some((idx1, '<')) => {
                            self.bump();
                            Some(Ok((idx0, Lookahead, idx1+1)))
                        }
                        Some((idx1, '>')) => {
                            self.bump();
                            Some(Ok((idx0, Lookbehind, idx1+1)))
                        }
                        _ => {
                            Some(Err(UnrecognizedToken(idx0)))
                        }
                    }
                }
                Some((idx0, '+')) => {
                    self.bump();
                    Some(Ok((idx0, Plus, idx0+1)))
                }
                Some((idx0, '-')) => {
                    match self.bump() {
                        Some((idx1, '>')) => {
                            self.bump();
                            Some(Ok((idx0, RightArrow, idx1+1)))
                        }
                        _ => {
                            Some(Err(UnrecognizedToken(idx0)))
                        }
                    }
                }
                Some((idx0, '?')) => {
                    self.bump();
                    Some(Ok((idx0, Question, idx0+1)))
                }
                Some((idx0, '}')) => {
                    self.bump();
                    Some(Ok((idx0, RightBrace, idx0+1)))
                }
                Some((idx0, ']')) => {
                    self.bump();
                    Some(Ok((idx0, RightBracket, idx0+1)))
                }
                Some((idx0, ')')) => {
                    self.bump();
                    Some(Ok((idx0, RightParen, idx0+1)))
                }
                Some((idx0, ';')) => {
                    self.bump();
                    Some(Ok((idx0, Semi, idx0+1)))
                }
                Some((idx0, '*')) => {
                    self.bump();
                    Some(Ok((idx0, Star, idx0+1)))
                }
                Some((idx0, '~')) => {
                    match self.bump() {
                        Some((idx1, '~')) => {
                            self.bump();
                            Some(Ok((idx0, TildeTilde, idx1+1)))
                        }
                        _ => {
                            Some(Err(UnrecognizedToken(idx0)))
                        }
                    }
                }
                Some((idx0, '_')) => {
                    self.bump();
                    Some(Ok((idx0, Underscore, idx0+1)))
                }
                Some((idx0, '`')) => {
                    self.bump();
                    Some(self.escape(idx0))
                }
                Some((idx0, '\'')) => {
                    self.bump();
                    Some(Ok(self.lifetime(idx0)))
                }
                Some((idx0, '"')) => {
                    self.bump();
                    Some(self.string_literal(idx0))
                }
                // This handles the comments
                Some((idx0, '/')) => {
                    match self.bump() {
                        Some((_, '/')) => {
                            self.take_until(|c| c == '\n');
                            continue;
                        }
                        _ => {
                            Some(Err(UnrecognizedToken(idx0)))
                        }
                    }
                }
                Some((idx0, c)) if is_identifier_start(c) => {
                    Some(Ok(self.identifierish(idx0)))
                }
                Some((_, c)) if c.is_whitespace() => {
                    self.bump();
                    continue;
                }
                Some((idx, _)) => {
                    Some(Err(UnrecognizedToken(idx)))
                }
                None => {
                    None
                }
            };
        }
    }
}

fn is_identifier_start(c: char) -> bool {
    // for some reason c.is_xid_start() is not stable :(
    c.is_alphabetic() || c == '_'
}

fn is_identifier_continue(c: char) -> bool {
    // for some reason c.is_xid_continue() is not stable :(
    c.is_alphabetic() || c == '_' || c.is_digit(10)
}
