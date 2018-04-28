use std::mem;
use std::collections::{
    HashMap,
    HashSet,
    VecDeque,
};
use syntax::ast::*;
use super::{
    Cfg,
    CfgNode,
    EdgeData,
};

enum VariableOperations {
    SetLiveVariables(CfgNode, HashSet<String>),
}

pub struct LiveVariableAnalyzer<'a> {
    cfg: &'a Cfg<'a>,
    pub alive: HashMap<CfgNode, HashSet<String>>,
}

impl<'a> LiveVariableAnalyzer<'a> {
    pub fn new(cfg: &'a Cfg<'a>) -> LiveVariableAnalyzer<'a> {
        LiveVariableAnalyzer {
            cfg,
            alive: HashMap::new(),
        }
    }

    pub fn analyze(&mut self) {
        use self::VariableOperations::*;
        let mut ops = VecDeque::<VariableOperations>::new();
        loop {
            for origin_node in self.cfg.iter() {
                let outgoing_edges = self.cfg.graph.get(&origin_node);
                if outgoing_edges.is_none() { continue; }
                let edges = outgoing_edges.unwrap();

                let mut variables_after = HashSet::new();
                let mut live_origin = HashSet::new();

                for (successor, edge) in edges.iter() {
                    // If this successor has no live variable set, create one for it.
                    if !self.alive.contains_key(successor) {
                        self.alive.insert(*successor, HashSet::new());
                    }

                    // This node's live variables are the union of the successor's live variables.
                    let successor_variables = self.alive.get(successor).expect("Successors should have variable sets");
                    variables_after = &variables_after | successor_variables;

                    debug!("Successors of {}: {:#?}", origin_node, successor_variables);

                    // Identify the variables defined and used by the statement on this edge.
                    let defined = defined_by_edge(&edge);
                    let used = used_by_edge(&edge);
                    debug!("Variables used by {}: {:#?}", origin_node, used);

                    // Calculate the live variables at the origin node.
                    debug!("Variables after {}: {:#?}", origin_node, variables_after);
                    debug!("Variables defined on {} -> {}: {:#?}", origin_node, successor, defined);
                    debug!("Variables used by {} -> {}: {:#?}", origin_node, successor, used);

                    if let Some(defined) = defined {
                        variables_after.remove(&defined);
                    }
                    live_origin = &live_origin | &(&variables_after | (&used));
                }

                debug!("Live variables at node {}: {:#?}", origin_node, live_origin);

                // If the variable data for this node has not changed, don't do any operations.
                match self.alive.get(&origin_node) {
                    None => (),
                    Some(variables) => {
                        if variables == &live_origin {
                            debug!("NO OPERATIONS FOR NODE {}", origin_node);
                            continue;
                        }
                    }
                }
                ops.push_back(SetLiveVariables(origin_node, live_origin));
            }

            if ops.len() == 0 {
                debug!("FINISHED ANALYSIS");
                break;
            }

            while let Some(op) = ops.pop_front() {
                match op {
                    SetLiveVariables(node, vars) => {
                        self.alive.insert(node, vars);
                    }
                }
            }
        }

        println!("Live variables: {:#?}", self.alive);
    }

    pub fn list_dead_vars(&self) -> HashSet<String> {
        let mut dead = HashSet::new();

        for node in self.cfg.iter() {
            let outgoing_edges = self.cfg.graph.get(&node);
            if outgoing_edges.is_none() { continue; }
            let edges = outgoing_edges.unwrap();

            let live_in_origin = self.alive.get(&node)
                .expect("Each node should have a live variable set");

            for (successor, edge) in edges.iter() {
                let live_in_successor = self.alive.get(&successor)
                    .expect("Each node should have a live variable set");

                if let Some(ref defined_variable) = defined_by_edge(edge) {

                    debug!("{:?} -> {} -> {:?}", live_in_origin, defined_variable, live_in_successor);

                    if !live_in_successor.contains(defined_variable) {
                        dead.insert(defined_variable.clone());
                        debug!("FOUND DEAD VARIABLE {}", defined_variable);
                    }
                }
            }
        }
        dead
    }

    pub fn optimized_cfg<'b>(&self) -> Cfg<'b> {
        unimplemented!()
    }
}

fn defined_by_edge(edge: &EdgeData) -> Option<String> {
    match edge {
        EdgeData::Stmt(ref statement) => {
            defined_by(statement)
        }
        EdgeData::Var(ref variable) => {
            Some(String::from(&variable.name.text))
        }
        _ => None,
    }
}

fn used_by_edge(edge: &EdgeData) -> HashSet<String> {
    let mut used = HashSet::new();
    match edge {
        EdgeData::Stmt(ref statement) => {
            used_by_statement(&mut used, statement);
        }
        EdgeData::Expr(ref expression) |
        EdgeData::ExprNot(ref expression) |
        EdgeData::Return(ref expression) => {
            used_by_expression(&mut used, expression);
        }
        _ => (),
    }
    used
}

/// Returns a list of all variables defined by the given statement, if any.
fn defined_by(statement: &Statement) -> Option<String> {
    match statement.stmt {
        Stmt::Assign { ref lhs, .. } => {
            Some(String::from(&lhs.text))
        }
        Stmt::AssignArray { ref lhs, .. } => {
            Some(String::from(&lhs.text))
        }
        Stmt::If { .. } |
        Stmt::While { .. } |
        Stmt::Block { .. } => panic!("Control flow graph shouldn't contain compound statements"),
        _ => None,
    }
}

fn used_by_statement(vars: &mut HashSet<String>, statement: &Statement) {
    match statement.stmt {
        Stmt::Assign { ref rhs, .. } => {
            used_by_expression(vars, rhs);
        }
        Stmt::AssignArray { ref index, ref rhs, .. } => {
            used_by_expression(vars, index);
            used_by_expression(vars, rhs);
        }
        Stmt::SideEffect { ref expression, .. } => {
            used_by_expression(vars, expression);
        }
        Stmt::Print { ref expression, .. } => {
            used_by_expression(vars, expression);
        }
        _ => panic!("Control flow graph shouldn't contain compound statements"),
    }
}

fn used_by_expression(vars: &mut HashSet<String>, expression: &Expression) {
    match expression.expr {
        Expr::Identifier(ref id) => {
            vars.insert(String::from(&id.text));
        }
        Expr::Unary(ref unary) => {
            match unary {
                UnaryExpression::NewArray(ref expr) => {
                    used_by_expression(vars, expr);
                }
                UnaryExpression::Not(ref expr) => {
                    used_by_expression(vars, expr);
                }
                UnaryExpression::Parentheses(ref expr) => {
                    used_by_expression(vars, expr);
                }
                UnaryExpression::Length(ref expr) => {
                    used_by_expression(vars, expr);
                }
                UnaryExpression::ArrayLookup { ref lhs, ref index, .. } => {
                    used_by_expression(vars, lhs);
                    used_by_expression(vars, index);
                }
                UnaryExpression::Application { ref expression, ref id, ref list, .. } => {
                    used_by_expression(vars, expression);
                    for arg in list.iter() {
                        used_by_expression(vars, arg);
                    }
                }
            }
        }
        Expr::Binary(ref binary) => {
            used_by_expression(vars, &binary.lhs);
            used_by_expression(vars, &binary.rhs);
        }
        _ => (),
    }
}