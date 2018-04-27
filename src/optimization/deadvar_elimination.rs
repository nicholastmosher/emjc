use std::mem;
use std::collections::{
    HashMap,
    HashSet,
};
use syntax::ast::*;
use super::{
    Cfg,
    CfgNode,
    EdgeData,
};

pub struct NodeState {
    incoming: HashSet<String>, // in (S)
    outgoing: HashSet<String>,// out (S)
}

impl NodeState {
    fn new() -> NodeState {
        NodeState {
            incoming: HashSet::new(),
            outgoing: HashSet::new(),
        }
    }
}

enum VariableOperations {
    UpdateIncoming {
        node: CfgNode,
        outgoing: HashSet<String>,
        defined: HashSet<String>,
        used: HashSet<String>,
    },
    UpdateOutgoing {
        node: CfgNode,
    }
}

pub struct LiveVariableAnalyzer<'a> {
    cfg: &'a Cfg<'a>,
    alive: HashMap<CfgNode, HashSet<String>>,
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
        loop {
            let mut ops = Vec::<VariableOperations>::new();

            for node in self.cfg.iter() {
                let outgoing_edges = self.cfg.graph.get(&node);
                if outgoing_edges.is_none() { continue; }
                let edges = outgoing_edges.unwrap();

                for (end, edge) in edges.iter() {
                    let mut defined = HashSet::new();
                    let mut used = HashSet::new();

                    match edge {
                        EdgeData::Stmt(ref statement) => {
                            defined_by(&mut defined, statement);
                            used_by_statement(&mut used, statement);
                        }
                        EdgeData::Expr(ref expression) |
                        EdgeData::ExprNot(ref expression) => {
                            used_by_expression(&mut used, expression);
                        }
                    }


                    // This node's live variables are the union of the successor's live variables.
                }
            }

            let done = !self.apply_operations(ops);
            if done { break; }
        }
    }

    /// Applies all of the given operations to the variable data.
    /// Returns true if any changes occurred in the data (indicating more
    /// iterations may be needed), or false if there were no changes
    /// (indicating that there is no more work to be done).
    fn apply_operations(&mut self, ops: Vec<VariableOperations>) -> bool {
        use self::VariableOperations::*;

        let mut updated = false;
        for op in ops.iter() {
            match op {
                UpdateIncoming { ref node, ref outgoing, ref defined, ref used, .. } => {
                    if !self.alive.contains_key(node) {
                        self.alive.insert(*node, NodeState::new());
                        updated = true;
                    }

                    let node_state = self.alive.get_mut(node).unwrap();
                    let initial_size = node_state.incoming.len();

                    // in(S) = (out(S) \ def(S)) U use(S)
                    let incoming: HashSet<String> = &(outgoing - defined) & used;
                    let new_size = incoming.len();
                    mem::replace(&mut node_state.incoming, incoming);

                    if initial_size != new_size { updated = true; }
                }
                UpdateOutgoing { ref node, .. } => {
                    if !self.alive.contains_key(node) {
                        self.alive.insert(*node, NodeState::new());
                        updated = true;
                    }

                    let node_state = self.alive.get(node).unwrap();
                    let initial_size = node_state.outgoing.len();

                    // out(S) = U succ(S)
                    let successors = self.cfg.graph.get(node);
                    if successors.is_none() {
                        warn!("No successors to node {}", node);
                        continue;
                    }
                    let successors = successors.unwrap();

                    let mut outgoing = HashSet::<String>::new();
                    for (succ, edge) in successors.iter() {
                        if let Some(succ_node_state) = self.alive.get(succ) {
                            let succ_in = &succ_node_state.incoming;
                            outgoing = &outgoing & succ_in;
                            updated = true;
                        }
                    }
                }
            }
        }
        updated
    }
}

/// Returns a list of all variables defined by the given statement, if any.
fn defined_by(vars: &mut HashSet<String>, statement: &Statement) {
    match statement.stmt {
        Stmt::Assign { ref lhs, .. } => {
            vars.insert(String::from(&lhs.text));
        }
        Stmt::AssignArray { ref lhs, .. } => {
            vars.insert(String::from(&lhs.text));
        }
        Stmt::If { .. } |
        Stmt::While { .. } |
        Stmt::Block { .. } => panic!("Control flow graph shouldn't contain compound statements"),
        _ => (),
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