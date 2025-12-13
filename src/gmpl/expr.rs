use std::fmt;
use std::sync::LazyLock;

use pest::iterators::{Pair, Pairs};
use pest::pratt_parser::{Assoc::*, Op, PrattParser};

use crate::{
    gmpl::atoms::{BoolOp, Domain, FuncMax, FuncMin, FuncSum, MathOp, RelOp, VarSubscripted},
    grammar::Rule,
};

static PRATT_PARSER: LazyLock<PrattParser<Rule>> = LazyLock::new(|| {
    PrattParser::new()
        // Precedence lowest to highest (per GMPL spec)
        .op(Op::infix(Rule::add, Left) | Op::infix(Rule::sub, Left))
        .op(Op::prefix(Rule::sum_prefix)) // iterated ops: between add/sub and mul/div
        .op(Op::infix(Rule::mul, Left) | Op::infix(Rule::div, Left))
        .op(Op::prefix(Rule::neg))
        .op(Op::infix(Rule::pow, Right))
});

/// Expression - recursive tree structure with proper operator precedence
#[derive(Clone, Debug)]
pub enum Expr {
    Number(f64),
    VarSubscripted(VarSubscripted),
    FuncSum(Box<FuncSum>),
    FuncMin(Box<FuncMin>),
    FuncMax(Box<FuncMax>),
    Conditional(Box<Conditional>),
    UnaryNeg(Box<Expr>),
    BinOp {
        lhs: Box<Expr>,
        op: MathOp,
        rhs: Box<Expr>,
    },
}

impl Expr {
    pub fn from_entry(entry: Pair<Rule>) -> Self {
        parse_expr(entry.into_inner())
    }
}

/// Parse expression using Pratt parser for correct precedence
pub fn parse_expr(pairs: Pairs<Rule>) -> Expr {
    PRATT_PARSER
        .map_primary(|primary| match primary.as_rule() {
            Rule::number => Expr::Number(primary.as_str().parse().unwrap_or(0.0)),
            Rule::var_subscripted => Expr::VarSubscripted(VarSubscripted::from_entry(primary)),
            Rule::func_min => Expr::FuncMin(Box::new(FuncMin::from_entry(primary))),
            Rule::func_max => Expr::FuncMax(Box::new(FuncMax::from_entry(primary))),
            Rule::conditional => Expr::Conditional(Box::new(Conditional::from_entry(primary))),
            Rule::expr => parse_expr(primary.into_inner()),
            rule => unreachable!("Expected primary, found {:?}", rule),
        })
        .map_prefix(|op, rhs| match op.as_rule() {
            Rule::neg => Expr::UnaryNeg(Box::new(rhs)),
            Rule::sum_prefix => {
                // Extract domain from sum_prefix
                let domain = op
                    .into_inner()
                    .find(|p| p.as_rule() == Rule::domain)
                    .map(Domain::from_entry)
                    .expect("sum_prefix must have domain");
                Expr::FuncSum(Box::new(FuncSum {
                    domain,
                    operand: Box::new(rhs),
                }))
            }
            rule => unreachable!("Expected prefix op, found {:?}", rule),
        })
        .map_infix(|lhs, op, rhs| {
            let op = match op.as_rule() {
                Rule::add => MathOp::Add,
                Rule::sub => MathOp::Sub,
                Rule::mul => MathOp::Mul,
                Rule::div => MathOp::Div,
                Rule::pow => MathOp::Pow,
                rule => unreachable!("Expected infix op, found {:?}", rule),
            };
            Expr::BinOp {
                lhs: Box::new(lhs),
                op,
                rhs: Box::new(rhs),
            }
        })
        .parse(pairs)
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Number(n) => write!(f, "{}", n),
            Expr::VarSubscripted(v) => write!(f, "{}", v),
            Expr::FuncSum(func) => write!(f, "{}", **func),
            Expr::FuncMin(func) => write!(f, "{}", **func),
            Expr::FuncMax(func) => write!(f, "{}", **func),
            Expr::Conditional(cond) => write!(f, "{}", **cond),
            Expr::UnaryNeg(e) => write!(f, "-{}", **e),
            Expr::BinOp { lhs, op, rhs } => write!(f, "({} {} {})", **lhs, op, **rhs),
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
            ExprOrLiteral::Expr(e) => write!(f, "{}", e),
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
        write!(f, "if {} then {}", self.condition, self.then_expr)?;
        if let Some(else_expr) = &self.else_expr {
            write!(f, " else {}", else_expr)?;
        }
        Ok(())
    }
}
