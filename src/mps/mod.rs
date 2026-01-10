use std::collections::HashMap;

use indexmap::IndexMap;

use crate::gmpl::atoms::{Domain, RelOp, VarSubscripted};
use crate::gmpl::{ParamDataBody, SetIndex};
use crate::model::ParamWithData;
use crate::{
    gmpl::{Constraint, Expr, atoms::MathOp},
    model::ModelWithData,
};
use itertools::Itertools;

type SetMap = IndexMap<String, Vec<SetIndex>>;
type SetArr = Vec<Vec<SetIndex>>;
type VarMap = HashMap<String, bool>;
type ParamMap = HashMap<String, ParamArr>;
enum ParamArr {
    Arr(HashMap<Vec<SetIndex>, f64>),
    Scalar(f64),
}
//                  var       set_index        constraint    val
type Cols = IndexMap<(String, Vec<SetIndex>), IndexMap<String, f64>>;

// const SENTINEL: SetIndex = SetIndex::Int(u64::MAX);

pub fn compile_mps(model: ModelWithData) {
    let set_map: SetMap = model
        .sets
        .clone()
        .into_iter()
        .map(|set| (set.decl.name, set.data.unwrap().values))
        .collect();

    // let set_indexes: SetArr = set_map.into_values().multi_cartesian_product().collect();

    // let set_indexes: SetArr = {
    //     let result: SetArr = set_map
    //         .clone()
    //         .into_values()
    //         .multi_cartesian_product()
    //         .collect();
    //     if result.is_empty() {
    //         vec![vec![]]
    //     } else {
    //         result
    //     }
    // };

    let var_map: VarMap = model
        .vars
        .clone()
        .into_iter()
        .map(|var| (var.name, true))
        .collect();

    let param_map: ParamMap = model
        .params
        .clone()
        .into_iter()
        .map(|param| (param.decl.name.clone(), resolve_param(param)))
        .collect();

    // Constraints
    let mut cols: Cols = IndexMap::new();
    let mut rhs: IndexMap<String, f64> = IndexMap::new();

    // First the objective alone
    {
        // Objective is always "singular"
        // it has no domain
        let index = vec![];

        let pairs = recurse(
            model.objective.expr.clone(),
            &var_map,
            &param_map,
            &set_map,
            &index,
        );
        let pairs = match pairs {
            RecurseResult::Pairs(pairs) => pairs,
            _ => panic!("unhandled OBJ"),
        };
        for pair in &pairs {
            cols.entry((
                pair.var.clone(),
                pair.index.clone().unwrap_or_else(Vec::new),
            ))
            .or_default()
            .insert(model.objective.name.clone(), pair.coeff);
        }
    }

    // Then all the actual constraints
    for constraint in &model.constraints {
        let con_indexes = domain_to_indexes(&constraint.domain, &set_map);
        for index in con_indexes {
            let built = build_constraint(constraint, &var_map, &param_map, &set_map, &index);
            if let Some(num) = built.rhs {
                rhs.insert(constraint.name.clone(), num);
            }
            for pair in &built.pairs {
                cols.entry((
                    pair.var.clone(),
                    pair.index.clone().unwrap_or_else(Vec::new),
                ))
                .or_default()
                .insert(constraint.name.clone(), pair.coeff);
            }
        }
    }

    print_name();
    print_rows(&model);
    print_cols(cols);
    print_rhs(rhs);
    print_bounds(&model, &set_map);
    println!("ENDATA");
}

fn resolve_param(param: ParamWithData) -> ParamArr {
    let data = param.data.unwrap();
    if let Some(body) = data.body {
        match body {
            ParamDataBody::Num(num) => ParamArr::Scalar(num),
            ParamDataBody::List(pairs) => {
                let mut arr: HashMap<Vec<SetIndex>, f64> = HashMap::new();
                for pair in pairs {
                    arr.insert(vec![pair.key], pair.value);
                }
                ParamArr::Arr(arr)
            }
            ParamDataBody::Tables(_) => panic!("param data tables not implemented"),
        }
    } else if let Some(default) = data.default {
        ParamArr::Scalar(default)
    } else {
        panic!("param data body is incomplete");
    }
}

fn build_constraint(
    constraint: &Constraint,
    var_map: &VarMap,
    param_map: &ParamMap,
    set_map: &SetMap,
    index: &Vec<SetIndex>,
) -> BuiltConstraint {
    let lhs = recurse(
        constraint.constraint_expr.lhs.clone(),
        var_map,
        param_map,
        set_map,
        index,
    );
    let rhs = recurse(
        constraint.constraint_expr.rhs.clone(),
        var_map,
        param_map,
        set_map,
        index,
    );

    let pairs = match lhs {
        RecurseResult::Pairs(pairs) => pairs,
        _ => panic!("unhandled outer LHS"),
    };

    let rhs = match rhs {
        RecurseResult::Number(num) => num,
        _ => panic!("unhandled outer RHS"),
    };

    BuiltConstraint {
        rhs: Some(rhs),
        pairs,
    }
}

fn recurse(
    expr: Expr,
    var_map: &VarMap,
    param_map: &ParamMap,
    set_map: &SetMap,
    _index: &Vec<SetIndex>,
) -> RecurseResult {
    match expr {
        Expr::Number(num) => RecurseResult::Number(num),
        Expr::VarSubscripted(var_or_param) => {
            let name = var_or_param.var;
            let concrete: Option<Vec<SetIndex>> = var_or_param
                .concrete
                .map(|concrete| concrete.iter().map(|s| SetIndex::Str(s.clone())).collect());
            if var_map.get(&name).is_some() {
                return RecurseResult::Pairs(vec![Pair {
                    coeff: 1.0,
                    index: concrete,
                    var: name,
                }]);
            }

            if let Some(param) = param_map.get(&name) {
                return match param {
                    ParamArr::Scalar(num) => RecurseResult::Number(*num),
                    // TODO need to merge concrete and index somehow
                    ParamArr::Arr(arr) => {
                        RecurseResult::Number(*arr.get(&concrete.unwrap()).unwrap())
                    }
                };
            }

            panic!("symbol {} does not point to a valid var or param", &name);
        }
        Expr::FuncSum(func) => {
            let domain = func.domain;
            let operand = *func.operand;
            let indexes = domain_to_indexes(&Some(domain.clone()), set_map);
            let domain_vars: Vec<String> = domain.parts.iter().map(|d| d.var.clone()).collect();

            let new_expr = expand_sum(&operand, &domain_vars, &indexes);
            recurse(new_expr, var_map, param_map, set_map, _index)
        }
        Expr::FuncMin(_) => panic!("not implemented: FuncMin"),
        Expr::FuncMax(_) => panic!("not implemented: FuncMax"),
        Expr::Conditional(_) => panic!("not implemented: Conditional"),
        Expr::UnaryNeg(_) => panic!("not implemented: UnaryNeg"),
        Expr::BinOp { lhs, op, rhs } => {
            let lhs = recurse(*lhs, var_map, param_map, set_map, _index);
            let rhs = recurse(*rhs, var_map, param_map, set_map, _index);

            let debug_msg = format!("unhandled case: lhs:{:?} rhs:{:?} op:{}", &lhs, &rhs, op);

            match (lhs, rhs, op) {
                (RecurseResult::Number(l), RecurseResult::Number(r), MathOp::Add) => {
                    RecurseResult::Number(l + r)
                }
                (RecurseResult::Number(l), RecurseResult::Number(r), MathOp::Sub) => {
                    RecurseResult::Number(l - r)
                }
                (RecurseResult::Number(l), RecurseResult::Pairs(pairs), MathOp::Mul) => {
                    let res: Vec<Pair> = pairs
                        .iter()
                        .map(|p| Pair {
                            coeff: l * p.coeff,
                            index: p.index.clone(),
                            var: p.var.clone(),
                        })
                        .collect();
                    RecurseResult::Pairs(res)
                }
                (RecurseResult::Pairs(l_pairs), RecurseResult::Pairs(r_pairs), MathOp::Add) => {
                    RecurseResult::Pairs([l_pairs, r_pairs].concat())
                }
                _ => unreachable!("{}", debug_msg),
            }
        }
    }
}

// Structs
pub struct BuiltConstraint {
    rhs: Option<f64>,
    pairs: Vec<Pair>,
}

#[derive(Clone, Debug)]
pub struct Pair {
    var: String,
    index: Option<Vec<SetIndex>>,
    coeff: f64,
}

#[derive(Clone, Debug)]
pub enum RecurseResult {
    Number(f64),
    Pairs(Vec<Pair>),
}

// Utils
fn print_name() {
    println!("NAME          noname");
}

fn rel_op_to_row_type(op: &RelOp) -> String {
    match op {
        RelOp::Lt => panic!("Less than not supported"),
        RelOp::Le => "L".to_string(),
        RelOp::Eq => "E".to_string(),
        RelOp::EqEq => "E".to_string(),
        RelOp::Ne => panic!("Not equal not supported"),
        RelOp::Ne2 => panic!("Not equal not supported"),
        RelOp::Ge => "G".to_string(),
        RelOp::Gt => panic!("Greater than not supported"),
    }
}

fn rel_op_to_bounds(op: &RelOp) -> String {
    match op {
        RelOp::Lt => panic!("Less than not supported"),
        RelOp::Le => "UP".to_string(),
        RelOp::Eq => "FX".to_string(),
        RelOp::EqEq => "FX".to_string(),
        RelOp::Ne => panic!("Not equal not supported"),
        RelOp::Ne2 => panic!("Not equal not supported"),
        RelOp::Ge => "LO".to_string(),
        RelOp::Gt => panic!("Greater than not supported"),
    }
}

fn print_rows(model: &ModelWithData) {
    println!("ROWS");

    // Objective
    {
        let dir = "N";
        let name = model.objective.name.clone();
        println!(" {dir}  {name}");
    }

    for constraint in &model.constraints {
        let dir = rel_op_to_row_type(&constraint.constraint_expr.op);
        let name = constraint.name.clone();
        println!(" {dir}  {name}")
    }
}

fn print_cols(cols: Cols) {
    println!("COLUMNS");
    for ((var_name, set_index), con_map) in cols {
        for (con_name, val) in con_map {
            let idx = format_set_index(&set_index);
            println!("    {}{}      {}         {}", var_name, idx, con_name, val);
        }
    }
}

fn print_rhs(rhs: IndexMap<String, f64>) {
    println!("RHS");

    for (con_name, val) in rhs {
        println!("    RHS1      {}       {}", con_name, val);
    }
}

fn print_bounds(model: &ModelWithData, set_map: &SetMap) {
    println!("BOUNDS");

    for var in &model.vars {
        if let Some(bounds) = var.bounds.clone() {
            let indexes = domain_to_indexes(&var.domain, set_map);
            let dir = rel_op_to_bounds(&bounds.op);
            let name = var.name.clone();
            let val = bounds.value;
            for index in indexes {
                let si = format_set_index(&index);
                println!(" {} BND1     {}{}         {}", dir, name, si, val);
            }
        }
    }
}

fn format_set_index(v: &[SetIndex]) -> String {
    if v.is_empty() {
        String::new()
    } else {
        let items: Vec<String> = v.iter().map(|s| s.to_string()).collect();
        format!("[{}]", items.join(","))
    }
}

fn domain_to_indexes(domain: &Option<Domain>, set_map: &SetMap) -> SetArr {
    match domain {
        None => vec![vec![]],
        Some(dom) => {
            let Domain { parts, condition } = dom;
            if condition.is_some() {
                panic!("domain conditions unhandled");
            }
            let sets: Vec<Vec<SetIndex>> = parts
                .iter()
                .map(|p| set_map.get(&p.set).unwrap().clone())
                .multi_cartesian_product()
                .collect();
            sets
        }
    }
}

fn expand_sum(operand: &Expr, domain_vars: &[String], indexes: &[Vec<SetIndex>]) -> Expr {
    let substituted: Vec<Expr> = indexes
        .iter()
        .map(|idx_combo| substitute_index(operand, domain_vars, idx_combo))
        .collect();

    substituted
        .into_iter()
        .reduce(|acc, expr| Expr::BinOp {
            lhs: Box::new(acc),
            op: MathOp::Add,
            rhs: Box::new(expr),
        })
        .unwrap_or(Expr::Number(0.0))
}

fn substitute_index(expr: &Expr, domain_vars: &[String], idx_combo: &[SetIndex]) -> Expr {
    match expr {
        Expr::VarSubscripted(vs) => {
            if let Some(subscript) = &vs.subscript {
                let concrete: Vec<String> = subscript
                    .indices
                    .iter()
                    .map(|i| {
                        if let Some(pos) = domain_vars.iter().position(|dv| dv == &i.var) {
                            match &idx_combo[pos] {
                                SetIndex::Str(s) => s.clone(),
                                SetIndex::Int(n) => n.to_string(),
                            }
                        } else {
                            i.var.clone() // not a domain var, keep as-is
                        }
                    })
                    .collect();

                return Expr::VarSubscripted(VarSubscripted {
                    var: vs.var.clone(),
                    subscript: None,
                    concrete: Some(concrete),
                });
            }
            Expr::VarSubscripted(vs.clone())
        }
        Expr::BinOp { lhs, op, rhs } => Expr::BinOp {
            lhs: Box::new(substitute_index(lhs, domain_vars, idx_combo)),
            op: op.clone(),
            rhs: Box::new(substitute_index(rhs, domain_vars, idx_combo)),
        },
        Expr::Number(n) => Expr::Number(*n),
        Expr::UnaryNeg(inner) => {
            Expr::UnaryNeg(Box::new(substitute_index(inner, domain_vars, idx_combo)))
        }
        _ => todo!("handle other variants"),
    }
}
