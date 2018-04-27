#![allow(warnings)]

pub mod graph_writer;
pub mod deadvar_elimination;

use std::fmt::{
    Display,
    Formatter,
    Result as fmtResult,
    Write,
};
use std::rc::Rc;
use std::collections::{
    HashMap,
    HashSet,
    VecDeque,
};

use uuid::Uuid;

use lexer::SourceMap;
use syntax::ast::*;

#[derive(Debug, Fail)]
enum _CfgError {
    #[fail(display = "attempted to create duplicate edge in control flow graph")]
    DuplicateEdge,
}

/// A CfgNode is just a uniquely identifiable struct.
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct CfgNode(Uuid);

impl CfgNode {
    fn new() -> CfgNode { CfgNode(Uuid::new_v4()) }
}

impl Display for CfgNode {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub enum EdgeData {
    Expr(Rc<Expression>),
    ExprNot(Rc<Expression>),
    Return(Rc<Expression>),
    Stmt(Rc<Statement>),
    Var(Rc<Variable>),
    Empty,
}

impl EdgeData {
    fn display(&self, map: &SourceMap) -> String {
        let mut string = String::new();
        match self {
            EdgeData::Expr(ref expr) => {
                let _ = write!(string, "[{}]", map.spanning(expr.span));
            }
            EdgeData::ExprNot(ref expr) => {
                let _ = write!(string, "![{}]", map.spanning(expr.span));
            }
            EdgeData::Return(ref expr) => {
                let _ = write!(string, "return {}", map.spanning(expr.span));
            }
            EdgeData::Stmt(ref statement) => {
                match statement.stmt {
                    Stmt::If { ref condition, .. } => {
                        let _ = write!(string, "if ({})", map.spanning(condition.span));
                    }
                    Stmt::While { ref expression, .. } => {
                        let _ = write!(string, "while ({})", map.spanning(expression.span));
                    }
                    Stmt::Assign { .. } |
                    Stmt::AssignArray { .. } |
                    Stmt::Print { .. } |
                    Stmt::SideEffect { .. } => {
                        let _ = write!(string, "{}", map.spanning(statement.span));
                    }
                    Stmt::Block { .. } => {
                        let _ = write!(string, "{{ }}");
                    }
                    _ => (),
                }
            }
            EdgeData::Var(ref var) => {
                let _ = write!(string, "{} {};", var.kind, &var.name.text);
            }
            EdgeData::Empty => (),
        }
        string
    }
}

type CfgEdges = HashMap<CfgNode, EdgeData>;
type CfgMap = HashMap<CfgNode, CfgEdges>;

pub struct Cfg<'a> {
    pub function: Rc<Function>,
    graph: CfgMap,
    start: CfgNode,
    end: CfgNode,
    source_map: &'a SourceMap,
}

pub struct CfgIterator<'a> {
    cfg: &'a Cfg<'a>,
    queue: VecDeque<CfgNode>,
    visited: HashSet<CfgNode>,
}

impl<'a> Iterator for CfgIterator<'a> {
    type Item = CfgNode;

    fn next(&mut self) -> Option<<Self as Iterator>::Item> {
        loop {
            match self.queue.pop_front() {
                None => return None,
                Some(from) => {
                    if self.visited.contains(&from) { continue; } else {
                        self.visited.insert(from);
                        match self.cfg.graph.get(&from) {
                            None => (),
                            Some(outgoing_edges) => {
                                for (end, _) in outgoing_edges.iter() {
                                    self.queue.push_back(*end);
                                }
                            }
                        }
                        return Some(from);
                    }
                }
            }
        }
    }
}

enum GraphOperation {
    AddEdge(CfgNode, CfgNode, EdgeData),
    RemoveEdge(CfgNode, CfgNode),
}

impl<'a> Cfg<'a> {
    pub fn new(source_map: &'a SourceMap, function: &Rc<Function>) -> Cfg<'a> {
        let mut cfg = Cfg {
            function: function.clone(),
            graph: CfgMap::new(),
            start: CfgNode::new(),
            end: CfgNode::new(),
            source_map,
        };

        cfg.generate_function_graph();
        cfg
    }

    fn generate_function_graph(&mut self) {
        use self::GraphOperation::*;
        let mut ops = Vec::<GraphOperation>::new();
        let mut from = self.start;

        for var in self.function.variables.iter() {
            let to = CfgNode::new();
            let edge = EdgeData::Var(var.clone());
            ops.push(AddEdge(from, to, edge));
            from = to;
        }

        for stmt in self.function.statements.iter() {
            let to = CfgNode::new();
            let edge = EdgeData::Stmt(stmt.clone());
            ops.push(AddEdge(from, to, edge));
            from = to;
        }

        if let Some(ref return_expression) = self.function.expression {
            let return_node = self.end;
            let return_edge = EdgeData::Return(return_expression.clone());
            ops.push(AddEdge(from, return_node, return_edge));
        }

        self.apply_operations(ops);

        // Expand the edges until there are no more to expand.
        loop {
            let ops = self.expand_edges(self.start);
            if ops.len() == 0 { break; }
            self.apply_operations(ops);
        }
    }

    /// Creates an edge from the 'from' node to the 'to' node with the given data
    /// attached to the edge. If an edge already existed between the two nodes,
    /// give an error.
    fn add_edge(&mut self, from: CfgNode, to: CfgNode, edge: EdgeData) {
        match self.graph.get_mut(&from) {
            Some(edges) => {
                edges.insert(to, edge);
            }
            None => {
                let mut edges = CfgEdges::new();
                edges.insert(to, edge);
                self.graph.insert(from, edges);
            }
        }
    }

    /// Given a node in the graph, checks whether any of the outgoing edges from
    /// that node can be expanded and expands them.
    fn expand_edges(&mut self, start: CfgNode) -> Vec<GraphOperation> {
        use self::GraphOperation::*;
        let mut ops = vec![];

        for from in self.iter() {
            let from = from.clone();
            let outgoing_edges = self.graph.get(&from);
            if outgoing_edges.is_none() { continue; }
            let outgoing_edges = outgoing_edges.unwrap();

            for (to, edge) in outgoing_edges.iter() {
                match edge {
                    EdgeData::Stmt(ref statement) => {
                        match statement.stmt {
                            Stmt::If { ref condition, ref statement, ref otherwise, .. } => {
                                // Remove the IF edge
                                ops.push(RemoveEdge(from, *to));

                                // Create edge START -> [condition] -> *
                                let condition_edge = EdgeData::Expr(condition.clone());
                                let condition_node = CfgNode::new();
                                ops.push(AddEdge(from, condition_node, condition_edge));

                                // Create edge * -> statement -> END
                                let statement_edge = EdgeData::Stmt(statement.clone());
                                ops.push(AddEdge(condition_node, *to, statement_edge));

                                // If there's an else statement, generate that branch
                                if let Some(ref otherwise) = otherwise {
                                    // Create edge START -> ![condition] -> *
                                    let condition_not_edge = EdgeData::ExprNot(condition.clone());
                                    let otherwise_node = CfgNode::new();
                                    ops.push(AddEdge(from, otherwise_node, condition_not_edge));

                                    // Create edge * -> otherwise -> END
                                    let otherwise_edge = EdgeData::Stmt(otherwise.clone());
                                    ops.push(AddEdge(otherwise_node, *to, otherwise_edge));
                                }
                            }
                            Stmt::While { ref expression, ref statement, .. } => {
                                // Remove the WHILE edge
                                ops.push(RemoveEdge(from, *to));

                                // Create the edge START -> [condition] -> *
                                let condition_edge = EdgeData::Expr(expression.clone());
                                let condition_node = CfgNode::new();
                                ops.push(AddEdge(from, condition_node, condition_edge));

                                // Create the edge * -> statement -> START
                                let statement_edge = EdgeData::Stmt(statement.clone());
                                ops.push(AddEdge(condition_node, from, statement_edge));

                                // Create the edge START -> ![condition] -> END
                                let condition_not_edge = EdgeData::ExprNot(expression.clone());
                                ops.push(AddEdge(from, *to, condition_not_edge));
                            }
                            Stmt::Block { ref statements, .. } => {
                                // Remove the BLOCK edge
                                ops.push(RemoveEdge(from, *to));

                                let mut next = from;
                                for (i, statement) in statements.iter().enumerate() {
                                    let to = if i >= statements.len() - 1 { *to } else { CfgNode::new() };
                                    let edge = EdgeData::Stmt(statement.clone());
                                    ops.push(AddEdge(next, to, edge));
                                    next = to;
                                }
                            }
                            _ => (),
                        }
                    }
                    _ => (),
                }
            }
        };

        ops
    }

    fn apply_operations(&mut self, operations: Vec<GraphOperation>) {
        let mut ops = operations;
        while ops.len() > 0 {
            let mut next_ops = vec![];
            for op in ops.iter() {
                match op {
                    GraphOperation::AddEdge(ref from, ref to, ref data) => {
                        self.add_edge(*from, *to, data.clone());
                    }
                    GraphOperation::RemoveEdge(ref from, ref to) => {
                        let edges = self.graph.get_mut(&from).expect("Edge to remove should exist");
                        edges.remove(&to);
                    }
                }
            }
            ops = next_ops;
        }
    }

    fn iter(&'a self) -> CfgIterator<'a> {
        let mut queue = VecDeque::new();
        queue.push_back(self.start);

        CfgIterator {
            cfg: self,
            queue,
            visited: HashSet::new(),
        }
    }

    fn predecessors_of(&self, node: CfgNode) -> Vec<CfgNode> {
        let mut predecessors = Vec::new();
        for item in self.iter() {
            let edges = self.graph.get(&item).expect("Items iterated from graph should exist in graph");
            for (end, _) in edges.iter() {
                if *end == node {
                    predecessors.push(item);
                }
            }
        }
        predecessors
    }
}
