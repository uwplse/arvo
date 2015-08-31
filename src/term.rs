#[derive(Debug)]
pub struct Variable {
    pub name: String
}

#[derive(Debug)]
pub enum Term {
    Type, 
    Hole,
    Var(Variable),
    Lambda(Variable, Option<Box<Term>>, Box<Term>),
    Pi(Variable, Box<Term>, Box<Term>),
    App(Box<Term>, Box<Term>),
    Intro(Variable, Box<Term>, Vec<Term>, Vec<Term>, Vec<Term>),
    Elim(Variable, Vec<Term>, Vec<Term>, Vec<Term>),
    Data(Variable, Vec<Term>, Vec<Term>)
}
