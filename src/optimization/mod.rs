#![allow(warnings)]

pub mod graph_writer;

use std::fmt::{
    Display,
    Formatter,
    Result as fmtResult,
    Write,
};
use std::rc::Rc;
use std::collections::HashMap;

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
struct CfgNode(Uuid);

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
    function: Rc<Function>,
    graph: CfgMap,
    start: CfgNode,
    source_map: &'a SourceMap,
}

impl<'a> Display for Cfg<'a> {
    fn fmt(&self, f: &mut Formatter) -> fmtResult {
        writeln!(f, "Start: {}", self.start)?;
        for (from, edges) in self.graph.iter() {
            for (to, edge) in edges.iter() {
                write!(f, "{} -> ", from)?;
                match edge {
                    EdgeData::Expr(ref expr) |
                    EdgeData::ExprNot(ref expr) =>  write!(f, "{} -> ", self.source_map.spanning(expr.span))?,
                    EdgeData::Stmt(ref stmt) => write!(f, "{} -> ", self.source_map.spanning(stmt.span))?,
                    _ => (),
                }
                writeln!(f, "{}", to)?;
            }
        }
        Ok(())
    }
}

enum GraphOperation {
    AddEdge(CfgNode, CfgNode, EdgeData),
    Expand(CfgNode),
    Remove(CfgNode, CfgNode),
}

impl<'a> Cfg<'a> {
    pub fn new(source_map: &'a SourceMap, function: &Rc<Function>) -> Cfg<'a> {
        let mut cfg = Cfg {
            function: function.clone(),
            graph: CfgMap::new(),
            start: CfgNode::new(),
            source_map,
        };

        cfg.generate_function_graph();
        cfg
    }

//    /// Given a CfgNode, return a list of all CfgNodes which are immediate successors.
//    fn next_nodes(&self, from: CfgNode) -> Vec<CfgNode> {
//        let edges = self.graph.get(&from);
//        match edges {
//            None => vec![],
//            Some(edges) => {
//                let mut nodes = vec![];
//                for (node, _) in edges.iter() {
//                    nodes.push(node.clone());
//                }
//                nodes
//            }
//        }
//    }

    fn generate_function_graph(&mut self) {
        let mut from = self.start.clone();
        let mut ops = Vec::<GraphOperation>::new();
        for statement in self.function.statements.iter() {
            let to = CfgNode::new();
            let data = EdgeData::Stmt(statement.clone());
            ops.push(GraphOperation::AddEdge(from, to, data));
            ops.push(GraphOperation::Expand(from));
            from = to;
        }
        if let Some(ref return_expression) = self.function.expression {
            let ret = CfgNode::new();
            let ret_edge = EdgeData::Return(return_expression.clone());
            ops.push(GraphOperation::AddEdge(from, ret, ret_edge));
        }
        self.apply_operations(ops);
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
        let mut ops = Vec::<GraphOperation>::new();

        let outgoing_edges = self.graph.get(&start).expect("Nodes to expand should have edges");
        debug!("Expanding node {}", start);
        for (end, edge) in outgoing_edges.iter() {
            match edge {
                EdgeData::Stmt(ref statement) => {
                    match statement.stmt {
                        // * Start
                        // |------------ ![condition]-|
                        // |                          *
                        // |-[condition]    otherwise-|
                        // *                          |
                        // |-statement                |
                        // * End --------------------/
                        Stmt::If { ref condition, ref statement, ref otherwise, .. } => {
                            debug!("Expaning IF {}", self.source_map.spanning(statement.span));
                            ops.push(Remove(start, *end));

                            // Create edge * Start -> [condition] -> *
                            let true_node = CfgNode::new();
                            let true_edge = EdgeData::Expr(condition.clone());
                            ops.push(AddEdge(start, true_node, true_edge));

                            // Create edge * -> [condition] -> * End with statement
                            let true_edge = EdgeData::Stmt(statement.clone());
                            ops.push(AddEdge(true_node, *end, true_edge));

                            // Recursively expand the inner true branch
                            ops.push(Expand(true_node));

                            if let Some(otherwise) = otherwise {
                                // Create edge Start -> ![condition] if there's an else statement.
                                let false_node = CfgNode::new();
                                let false_edge = EdgeData::ExprNot(condition.clone());
                                ops.push(AddEdge(start, false_node, false_edge));

                                // Create ![condition] -> End with otherwise
                                let false_edge = EdgeData::Stmt(otherwise.clone());
                                ops.push(AddEdge(false_node, *end, false_edge));

                                // Recursively expand the inner else branch
                                ops.push(Expand(false_node));
                            }
                        }
                        // * Start-----------![condition]--> * End
                        // ^             |-[condition]
                        // |--statement--*
                        Stmt::While { ref expression, ref statement, .. } => {
                            debug!("Expanding {}", self.source_map.spanning(statement.span));
                            ops.push(Remove(start, *end));

                            // Create edge * Start -> [condition] -> *
                            let true_node = CfgNode::new();
                            let true_edge = EdgeData::Expr(expression.clone());
                            ops.push(AddEdge(start, true_node, true_edge));

                            // Create edge * -> statement -> * Start
                            let loop_edge = EdgeData::Stmt(statement.clone());
                            ops.push(AddEdge(true_node, start, loop_edge));

                            // Create edge * Start -> ![condition] -> * End
                            ops.push(AddEdge(start, *end, EdgeData::ExprNot(expression.clone())));

                            // Recursively expand the inner statement
                            ops.push(Expand(true_node));
                        }
                        Stmt::Block { ref statements, .. } => {
                            debug!("Expanding {} statements in BLOCK", statements.len());
                            let mut from = start;
                            for (i, statement) in statements.iter().enumerate() {
                                let to = CfgNode::new();
                                let data = EdgeData::Stmt(statement.clone());
                                ops.push(AddEdge(from, to, data));
                                if i != 0 { ops.push(Expand(from)); }
                                from = to;
                            }
                        }
                        _ => (),
                    }
                }
                _ => (),
            }
        }
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
                    GraphOperation::Expand(ref from) => {
                        let nexts = self.expand_edges(*from);
                        debug!("Added {} ops", nexts.len());
                        next_ops.extend(nexts);
                    }
                    GraphOperation::Remove(ref from, ref to) => {
                        let edges = self.graph.get_mut(&from).expect("Edge to remove should exist");
                        edges.remove(&to);
                    }
                }
            }
            ops = next_ops;
        }
    }
}
