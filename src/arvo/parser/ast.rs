// The parse tree which is then post-processed into a term.

enum #[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String
}

#[derive(Debug, Clone, PartialEq)]
pub enum AST {
    Type,
    Hole,
    Var(Variable),
    Lambda(Variable, Option<Box<Term>>, Box<Term>),
    App(Box<Term>, Box<Term>),
    Data(Variable, Vec<Term>),
    // Borrowed from https://github.com/jroesch/hubris inspiration is discussed in write-up
    // which was last minute assemblage of english from the code, so please excuse any
    // mistakes.
    //
    // Right now to make parsing less crazy we can parse any term of the form `t : T` as an
    // ascription, we can then transform terms of the form (t : T) -> R into a terms of the
    // form \Pi t : T -> R[t], and terms of the form T -> R into Pi _ : T -> R since the bound
    // variable is not free in the term R.
    Ascribe(Box<Term>, Box<Term>),
}
