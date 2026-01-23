use std::{fmt, ops::Deref};

use pest::iterators::Pair;
use ref_cast::RefCast;

use crate::{
    gmpl::expr::{Expr, LogicExpr},
    grammar::Rule,
};

// ==============================
// CHILD STRUCTS
// ==============================

/// Set val (identifier or positive integer)
#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum SetVal {
    Str(String),
    Int(u64),
    Vec(Vec<SetValTerminal>),
}

#[derive(Hash, Eq, PartialEq, Clone, Debug)]
pub enum SetValTerminal {
    Str(String),
    Int(u64),
}

impl SetVal {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        let inner = entry.into_inner().next().unwrap();
        match inner.as_rule() {
            Rule::id => SetVal::Str(inner.as_str().to_string()),
            Rule::int => SetVal::Int(inner.as_str().parse().unwrap_or(0)),
            _ => SetVal::Str(inner.as_str().to_string()),
        }
    }
}

impl fmt::Display for SetVal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SetVal::Str(s) => write!(f, "{}", s),
            SetVal::Int(n) => write!(f, "{}", n),
            SetVal::Vec(v) => {
                let items: Vec<String> = v.iter().map(|s| s.to_string()).collect();
                write!(f, "{}", items.join(","))
            }
        }
    }
}

impl fmt::Display for SetValTerminal {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SetValTerminal::Str(s) => write!(f, "{}", s),
            SetValTerminal::Int(n) => write!(f, "{}", n),
        }
    }
}

/// Domain specification
#[derive(Clone, Debug)]
pub struct Domain {
    pub parts: Vec<DomainPart>,
    pub condition: Option<LogicExpr>,
}

impl Domain {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        let mut parts = Vec::new();
        let mut condition = None;

        for pair in entry.into_inner() {
            match pair.as_rule() {
                Rule::domain_part => parts.push(DomainPart::from_entry(pair)),
                Rule::logic_expr => condition = Some(LogicExpr::from_entry(pair)),
                _ => {}
            }
        }

        Self { parts, condition }
    }
}

impl fmt::Display for Domain {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        let vars: Vec<_> = self.parts.iter().map(|p| p.var.to_string()).collect();
        write!(f, "{}", vars.join(", "))?;
        if self.condition.is_some() {
            write!(f, ": <condition>")?;
        }
        write!(f, "}}")
    }
}

/// Single domain part (e.g., "r in REGION")
#[derive(Clone, Debug)]
pub struct DomainPart {
    pub var: DomainPartVar,
    pub set: String,
    pub subscript: Subscript,
}

impl DomainPart {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        let mut var = DomainPartVar::Single(String::new());
        let mut subscript = Subscript::default();
        let mut set = String::new();

        for pair in entry.into_inner() {
            match pair.as_rule() {
                Rule::domain_var => {
                    let inner = pair.into_inner().next().unwrap();
                    var = match inner.as_rule() {
                        Rule::domain_var_single => {
                            DomainPartVar::Single(inner.as_str().to_string())
                        }
                        Rule::domain_var_tuple => {
                            let ids: Vec<String> = inner
                                .into_inner()
                                .filter(|p| p.as_rule() == Rule::id)
                                .map(|p| p.as_str().to_string())
                                .collect();
                            DomainPartVar::Tuple(ids)
                        }
                        _ => unreachable!(),
                    };
                }
                Rule::subscript => {
                    subscript = Subscript::from_entry(pair);
                }
                Rule::domain_set => set = pair.as_str().to_string(),
                _ => {}
            }
        }

        Self {
            var,
            subscript,
            set,
        }
    }
}

impl fmt::Display for DomainPart {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} in {}", self.var, self.set)
    }
}

#[derive(Clone, Debug)]
pub enum DomainPartVar {
    Single(String),
    Tuple(Vec<String>),
}

impl fmt::Display for DomainPartVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            DomainPartVar::Single(s) => write!(f, "{}", s),
            DomainPartVar::Tuple(v) => write!(f, "({})", v.join(", ")),
        }
    }
}

// ==============================
// ENUMS AND OPERATORS
// ==============================

/// Relational operator
#[derive(Clone, Copy, Debug)]
pub enum RelOp {
    Lt,
    Le,
    Eq,
    EqEq,
    Ne,
    Ne2,
    Ge,
    Gt,
}

impl RelOp {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        match entry.as_str() {
            "<" => RelOp::Lt,
            "<=" => RelOp::Le,
            "=" => RelOp::Eq,
            "==" => RelOp::EqEq,
            "<>" => RelOp::Ne,
            "!=" => RelOp::Ne2,
            ">=" => RelOp::Ge,
            ">" => RelOp::Gt,
            _ => RelOp::Eq,
        }
    }
}

impl fmt::Display for RelOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RelOp::Lt => write!(f, "<"),
            RelOp::Le => write!(f, "<="),
            RelOp::Eq => write!(f, "="),
            RelOp::EqEq => write!(f, "=="),
            RelOp::Ne => write!(f, "<>"),
            RelOp::Ne2 => write!(f, "!="),
            RelOp::Ge => write!(f, ">="),
            RelOp::Gt => write!(f, ">"),
        }
    }
}

/// Mathematical operator
#[derive(Clone, Copy, Debug)]
pub enum MathOp {
    Add,
    Sub,
    Mul,
    Div,
    Pow,
}

impl MathOp {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        match entry.as_str() {
            "+" => MathOp::Add,
            "-" => MathOp::Sub,
            "*" => MathOp::Mul,
            "/" => MathOp::Div,
            "^" => MathOp::Pow,
            _ => MathOp::Add,
        }
    }
}

impl fmt::Display for MathOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MathOp::Add => write!(f, "+"),
            MathOp::Sub => write!(f, "-"),
            MathOp::Mul => write!(f, "*"),
            MathOp::Div => write!(f, "/"),
            MathOp::Pow => write!(f, "^"),
        }
    }
}

/// Boolean operator
#[derive(Clone, Debug)]
pub enum BoolOp {
    And,
    Or,
}

impl BoolOp {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        match entry.as_str() {
            "and" | "&&" => BoolOp::And,
            "or" | "||" => BoolOp::Or,
            _ => BoolOp::And,
        }
    }
}

impl fmt::Display for BoolOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BoolOp::And => write!(f, "and"),
            BoolOp::Or => write!(f, "or"),
        }
    }
}

/// Variable with optional subscript
#[derive(Clone, Debug)]
pub struct VarSubscripted {
    pub var: String,
    pub subscript: Subscript,
}

impl VarSubscripted {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        let mut var = String::new();
        let mut subscript = Subscript::default();

        for pair in entry.into_inner() {
            match pair.as_rule() {
                Rule::var_ref => var = pair.as_str().to_string(),
                Rule::subscript => subscript = Subscript::from_entry(pair),
                _ => {}
            }
        }

        Self { var, subscript }
    }
}

impl fmt::Display for VarSubscripted {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.var)?;
        if !self.subscript.is_empty() {
            write!(f, "[...]")?;
        }
        Ok(())
    }
}

/// SetVals
#[derive(Clone, Debug, Eq, PartialEq, Hash, Default, RefCast)]
#[repr(transparent)]
pub struct SetVals(pub Vec<SetVal>);

impl Deref for SetVals {
    type Target = Vec<SetVal>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Vec<SetVal>> for SetVals {
    fn from(inner: Vec<SetVal>) -> Self {
        SetVals(inner)
    }
}

/// Index
#[derive(Clone, Debug, Eq, PartialEq, Hash, Default, RefCast)]
#[repr(transparent)]
pub struct Index(pub Vec<SetVal>);

impl Deref for Index {
    type Target = Vec<SetVal>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl From<Vec<SetVal>> for Index {
    fn from(inner: Vec<SetVal>) -> Self {
        Index(inner)
    }
}

/// Subscript (array indexing with optional shifts)
#[derive(Clone, Debug, Default)]
pub struct Subscript(pub Vec<SubscriptPart>);

impl Subscript {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        let parts = entry
            .into_inner()
            .filter(|p| p.as_rule() == Rule::subscript_part)
            .map(SubscriptPart::from_entry)
            .collect();
        Self(parts)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &SubscriptPart> {
        self.0.iter()
    }
}

#[derive(Clone, Debug)]
pub struct SubscriptPart {
    pub var: String,
    pub shift: Option<SubscriptShift>,
}

impl SubscriptPart {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        let mut var = String::new();
        let mut shift = None;

        for pair in entry.into_inner() {
            match pair.as_rule() {
                Rule::id => var = pair.as_str().to_string(),
                Rule::subscript_shift => shift = Some(SubscriptShift::from_entry(pair)),
                _ => {}
            }
        }

        Self { var, shift }
    }
}

impl fmt::Display for SubscriptPart {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.var)?;
        if let Some(shift) = &self.shift {
            write!(f, "{}", shift)?;
        }
        Ok(())
    }
}

/// Subscript shift (+1 or -1)
#[derive(Clone, Debug)]
pub enum SubscriptShift {
    Plus,
    Minus,
}

impl SubscriptShift {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        let s = entry.as_str();
        if s.starts_with('+') {
            SubscriptShift::Plus
        } else {
            SubscriptShift::Minus
        }
    }
}

impl fmt::Display for SubscriptShift {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            SubscriptShift::Plus => write!(f, "+1"),
            SubscriptShift::Minus => write!(f, "-1"),
        }
    }
}

/// Sum function (iterated sum over a domain)
#[derive(Clone, Debug)]
pub struct FuncSum {
    pub domain: Domain,
    pub operand: Box<Expr>,
}

impl fmt::Display for FuncSum {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "sum {} {}", self.domain, self.operand)
    }
}

/// Min function
#[derive(Clone, Debug)]
pub struct FuncMin {
    pub domain: Domain,
    pub var: String,
}

impl FuncMin {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        let mut domain = None;
        let mut var = String::new();

        for pair in entry.into_inner() {
            match pair.as_rule() {
                Rule::domain => domain = Some(Domain::from_entry(pair)),
                Rule::func_var => var = pair.as_str().to_string(),
                _ => {}
            }
        }
        let domain = domain.unwrap();

        Self { domain, var }
    }
}

impl fmt::Display for FuncMin {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "min <domain> min(<var>)")
    }
}

/// Max function
#[derive(Clone, Debug)]
pub struct FuncMax {
    pub domain: Domain,
    pub var: String,
}

impl FuncMax {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        let mut domain = None;
        let mut var = String::new();

        for pair in entry.into_inner() {
            match pair.as_rule() {
                Rule::domain => domain = Some(Domain::from_entry(pair)),
                Rule::func_var => var = pair.as_str().to_string(),
                _ => {}
            }
        }
        let domain = domain.unwrap();

        Self { domain, var }
    }
}

impl fmt::Display for FuncMax {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "max <domain> max(<var>)")
    }
}
