//! These operators are slightly further generalised from
//! the structs in ir/mod.rs.

use std::fmt;

use crate::ir::{self, RelOp};

#[derive(Copy, Clone, Debug)]
pub struct Bounds {
    pub op: BoundsOp,
    pub val: Option<f64>,
}

impl Bounds {
    pub fn from_gmpl_bounds(bounds: Option<ir::VarBounds>) -> Self {
        match bounds {
            Some(bounds) => Bounds {
                op: BoundsOp::from_rel_op(&bounds.op),
                val: Some(bounds.value),
            },
            None => Bounds {
                op: BoundsOp::Free,
                val: None,
            },
        }
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum BoundsOp {
    Free,
    Lower,
    Upper,
    Fixed,
}

impl BoundsOp {
    pub fn from_rel_op(op: &ir::RelOp) -> Self {
        match op {
            ir::RelOp::Lt => panic!("Less than not supported"),
            ir::RelOp::Le => BoundsOp::Upper,
            ir::RelOp::Eq => BoundsOp::Fixed,
            ir::RelOp::EqEq => BoundsOp::Fixed,
            ir::RelOp::Ne => panic!("Not equal not supported"),
            ir::RelOp::Ne2 => panic!("Not equal not supported"),
            ir::RelOp::Ge => BoundsOp::Lower,
            ir::RelOp::Gt => panic!("Greater than not supported"),
        }
    }
}

impl fmt::Display for BoundsOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BoundsOp::Free => write!(f, "FR"),
            BoundsOp::Lower => write!(f, "LO"),
            BoundsOp::Upper => write!(f, "UP"),
            BoundsOp::Fixed => write!(f, "FX"),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum RowType {
    LessThanOrEqual,
    Equal,
    GreaterThanOrEqual,
    /// Used for the objective function
    Unconstrained,
}

impl fmt::Display for RowType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RowType::LessThanOrEqual => write!(f, "L"),
            RowType::Equal => write!(f, "E"),
            RowType::GreaterThanOrEqual => write!(f, "G"),
            RowType::Unconstrained => write!(f, "N"),
        }
    }
}

impl RowType {
    pub fn from_rel_op(op: &RelOp) -> Self {
        match op {
            RelOp::Lt => panic!("Less than not supported"),
            RelOp::Le => RowType::LessThanOrEqual,
            RelOp::Eq => RowType::Equal,
            RelOp::EqEq => RowType::Equal,
            RelOp::Ne => panic!("Not equal not supported"),
            RelOp::Ne2 => panic!("Not equal not supported"),
            RelOp::Ge => RowType::GreaterThanOrEqual,
            RelOp::Gt => panic!("Greater than not supported"),
        }
    }
}
