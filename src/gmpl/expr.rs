use std::fmt;

use pest::iterators::Pair;

use crate::{
    gmpl::atoms::{BoolOp, FuncMax, FuncMin, FuncSum, MathOp, RelOp, VarSubscripted},
    grammar::Rule,
};

/// Expression
#[derive(Clone, Debug)]
pub struct Expr {
    pub negated: bool,
    pub terms: Vec<ExprTerm>,
    pub operators: Vec<MathOp>,
}

impl Expr {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        let mut negated = false;
        let mut terms = Vec::new();
        let mut operators = Vec::new();

        // Check if first term is negative before consuming entry
        let expr_str = entry.as_str().trim();
        if expr_str.starts_with('-') {
            negated = true;
        }

        for pair in entry.into_inner() {
            match pair.as_rule() {
                Rule::number => {
                    let val = pair.as_str().parse().unwrap_or(0.0);
                    terms.push(ExprTerm::Number(val));
                }
                Rule::var_subscripted => {
                    terms.push(ExprTerm::VarSubscripted(VarSubscripted::from_entry(pair)))
                }
                Rule::func_sum => terms.push(ExprTerm::FuncSum(FuncSum::from_entry(pair))),
                Rule::func_min => terms.push(ExprTerm::FuncMin(FuncMin::from_entry(pair))),
                Rule::func_max => terms.push(ExprTerm::FuncMax(FuncMax::from_entry(pair))),
                Rule::conditional => {
                    terms.push(ExprTerm::Conditional(Conditional::from_entry(pair)))
                }
                Rule::expr => terms.push(ExprTerm::Expr(Box::new(Expr::from_entry(pair)))),
                Rule::math_op => operators.push(MathOp::from_entry(pair)),
                _ => {}
            }
        }

        Self {
            negated,
            terms,
            operators,
        }
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<expr>")
    }
}

/// Expression term
#[derive(Clone, Debug)]
pub enum ExprTerm {
    Number(f64),
    VarSubscripted(VarSubscripted),
    FuncSum(FuncSum),
    FuncMin(FuncMin),
    FuncMax(FuncMax),
    Conditional(Conditional),
    Expr(Box<Expr>),
}

impl fmt::Display for ExprTerm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprTerm::Number(n) => write!(f, "{}", n),
            ExprTerm::VarSubscripted(v) => write!(f, "{}", v),
            ExprTerm::FuncSum(_) => write!(f, "<sum>"),
            ExprTerm::FuncMin(_) => write!(f, "<min>"),
            ExprTerm::FuncMax(_) => write!(f, "<max>"),
            ExprTerm::Conditional(_) => write!(f, "<if-then-else>"),
            ExprTerm::Expr(_) => write!(f, "(<expr>)"),
        }
    }
}

/// Logical expression
#[derive(Clone, Debug)]
pub struct LogicExpr {
    pub parts: Vec<LogicExprPart>,
    pub operators: Vec<BoolOp>,
}

impl LogicExpr {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        let mut parts = Vec::new();
        let mut operators = Vec::new();

        let inner: Vec<_> = entry.into_inner().collect();
        for (i, pair) in inner.iter().enumerate() {
            match pair.as_rule() {
                Rule::logic_expr_compound => {
                    // Extract inner logic_expr from compound
                    let inner_logic = pair.clone().into_inner().next().unwrap();
                    parts.push(LogicExprPart::Compound(Box::new(LogicExpr::from_entry(
                        inner_logic,
                    ))));
                }
                Rule::expr => {
                    // This is the start of a comparison (expr rel_op expr)
                    if i + 2 < inner.len()
                        && let (Some(op_pair), Some(rhs_pair)) =
                            (inner.get(i + 1), inner.get(i + 2))
                        && op_pair.as_rule() == Rule::rel_op
                    {
                        let lhs = ExprOrLiteral::Expr(Expr::from_entry(pair.clone()));
                        let op = RelOp::from_entry(op_pair.clone());
                        let rhs = ExprOrLiteral::Expr(Expr::from_entry(rhs_pair.clone()));
                        parts.push(LogicExprPart::Comparison { lhs, op, rhs });
                    }
                }
                Rule::string_literal => {
                    // Part of a comparison with string literal
                    if i > 0
                        && i + 1 < inner.len()
                        && let Some(prev) = inner.get(i - 1)
                        && prev.as_rule() == Rule::rel_op
                    {
                        // Already handled in previous iteration
                        continue;
                    }
                }
                Rule::bool => operators.push(BoolOp::from_entry(pair.clone())),
                Rule::rel_op => {
                    // Skip, handled with expr
                }
                _ => {}
            }
        }

        Self { parts, operators }
    }
}

impl fmt::Display for LogicExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<logic expr>")
    }
}

/// Part of a logical expression
#[derive(Clone, Debug)]
pub enum LogicExprPart {
    Comparison {
        lhs: ExprOrLiteral,
        op: RelOp,
        rhs: ExprOrLiteral,
    },
    Compound(Box<LogicExpr>),
}

impl fmt::Display for LogicExprPart {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<logic part>")
    }
}

/// Expression or string literal
#[derive(Clone, Debug)]
pub enum ExprOrLiteral {
    Expr(Expr),
    StringLiteral(String),
}

impl ExprOrLiteral {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        match entry.as_rule() {
            Rule::expr => ExprOrLiteral::Expr(Expr::from_entry(entry)),
            Rule::string_literal => ExprOrLiteral::StringLiteral(entry.as_str().to_string()),
            _ => {
                // For cases where we get the inner directly
                if let Some(inner) = entry.into_inner().next() {
                    ExprOrLiteral::from_entry(inner)
                } else {
                    ExprOrLiteral::StringLiteral(String::new())
                }
            }
        }
    }
}

impl fmt::Display for ExprOrLiteral {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprOrLiteral::Expr(_) => write!(f, "<expr>"),
            ExprOrLiteral::StringLiteral(s) => write!(f, "{}", s),
        }
    }
}

/// Conditional expression (if-then-else)
#[derive(Clone, Debug)]
pub struct Conditional {
    pub condition: LogicExpr,
    pub then_expr: Box<ExprOrLiteral>,
    pub else_expr: Option<Box<ExprOrLiteral>>,
}

impl Conditional {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        let mut condition = None;
        let mut then_expr = None;
        let mut else_expr = None;

        let inner: Vec<_> = entry.into_inner().collect();
        let mut i = 0;
        while i < inner.len() {
            let pair = &inner[i];
            match pair.as_rule() {
                Rule::logic_expr => condition = Some(LogicExpr::from_entry(pair.clone())),
                Rule::expr => {
                    if then_expr.is_none() {
                        then_expr = Some(Box::new(ExprOrLiteral::Expr(Expr::from_entry(
                            pair.clone(),
                        ))));
                    } else {
                        else_expr = Some(Box::new(ExprOrLiteral::Expr(Expr::from_entry(
                            pair.clone(),
                        ))));
                    }
                }
                Rule::string_literal => {
                    if then_expr.is_none() {
                        then_expr = Some(Box::new(ExprOrLiteral::StringLiteral(
                            pair.as_str().to_string(),
                        )));
                    } else {
                        else_expr = Some(Box::new(ExprOrLiteral::StringLiteral(
                            pair.as_str().to_string(),
                        )));
                    }
                }
                _ => {}
            }
            i += 1;
        }

        Self {
            condition: condition.unwrap(),
            then_expr: then_expr.unwrap(),
            else_expr,
        }
    }
}

impl fmt::Display for Conditional {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "if <condition> then <expr>")?;
        if self.else_expr.is_some() {
            write!(f, " else <expr>")?;
        }
        Ok(())
    }
}
