use std::str;
use std::collections::{
    HashMap,
    HashSet,
};
use std::io::{
    Write,
    Result as ioResult,
};

use super::{
    Cfg,
    CfgNode,
};

pub struct GraphWriter {
    next_id: usize,
    node_map: HashMap<CfgNode, usize>,
}

impl GraphWriter {
    pub fn new() -> GraphWriter {
        GraphWriter {
            next_id: 0,
            node_map: HashMap::new(),
        }
    }

    pub fn write_to<W: Write>(&mut self, w: &mut W, cfg: &Cfg) -> ioResult<()> {
        let _ = writeln!(w, "digraph {{");

        for from in cfg.iter() {
            let outgoing_edges = cfg.graph.get(&from);
            if outgoing_edges.is_none() { continue; }
            let outgoing_edges = outgoing_edges.unwrap();

            let start_id = self.get_id(from);
            for (end, edge) in outgoing_edges.iter() {
                let end_id = self.get_id(*end);
                let edge_label = edge.display(&cfg.source_map);
                let _ = writeln!(w, r#"  {} -> {}[label="{}"];"#, start_id, end_id, edge_label);
            }
        }

        let _ = writeln!(w, "}}");
        Ok(())
    }

    pub fn write_annotated<F, W: Write>(&mut self, w: &mut W, cfg: &Cfg, annotation: F) -> ioResult<()>
        where F: Fn(CfgNode) -> Option<String>
    {
        let _ = writeln!(w, "digraph {{");

        for from in cfg.iter() {
            let outgoing_edges = cfg.graph.get(&from);
            if outgoing_edges.is_none() { continue; }
            let outgoing_edges = outgoing_edges.unwrap();

            let start_id = self.get_id(from);

            // Write node label, if any.
            if let Some(node_label) = annotation(from) {
                let _ = writeln!(w, r#"  {} [label="{}"]"#, start_id, str::replace(&node_label, r#"""#, r#"'"#));
            }

            for (end, edge) in outgoing_edges.iter() {
                let end_id = self.get_id(*end);
                let edge_label = edge.display(&cfg.source_map);
                let _ = writeln!(w, r#"  {} -> {}[label="{}"];"#, start_id, end_id, str::replace(&edge_label, r#"""#, r#"'"#));
            }
        }

        let _ = writeln!(w, "}}");
        Ok(())
    }

    fn get_id(&mut self, node: CfgNode) -> usize {
        match self.node_map.get(&node) {
            Some(id) => *id,
            None => {
                let id = self.next_id;
                self.node_map.insert(node, id);
                self.next_id += 1;
                id
            }
        }
    }
}