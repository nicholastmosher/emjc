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

        let mut queue = vec![cfg.start];
        let mut visited = HashSet::<CfgNode>::new();
        let mut i = 0;
        while queue.len() > i {
            let from = queue.get(i);
            i += 1;
            if from.is_none() {
                warn!("Could not get item from non-empty queue");
                break;
            }
            let from = *from.unwrap();
            if visited.contains(&from) { continue; }
            visited.insert(from);

            let outgoing_edges = cfg.graph.get(&from);
            if outgoing_edges.is_none() { continue; }
            let outgoing_edges = outgoing_edges.unwrap();

            let start_id = self.get_id(from);
            for (end, edge) in outgoing_edges.iter() {
                queue.push(*end);
                let end_id = self.get_id(*end);
                let edge_label = edge.display(&cfg.source_map);
                let _ = writeln!(w, r#"  {} -> {}[label="{}"];"#, start_id, end_id, edge_label);
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