#[derive(Debug)]
pub struct Variable {
    pub name: String
}

#[derive(Debug)]
pub enum Term {
    Type, 
    Hole,
    Var(Variable),
    Lambda(Variable, Box<Term>, Box<Term>),
    Pi(Variable, Box<Term>, Box<Term>),
    App(Box<Term>, Box<Term>),
    Intro(Variable, Box<Term>, Vec<Term>),
    Elim(Variable, Vec<Term>),
    Data(Variable)
}
