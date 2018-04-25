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
            let start: CfgNode = *queue.get(i).expect("Should fetch node from queue");
            if let Some(edges) = cfg.graph.get(&start) {
                let start_id = self.get_id(start);
                for (end, edge) in edges.iter() {
                    let end_id = self.get_id(*end);
                    let edge_label = edge.display(&cfg.source_map);
                    let _ = writeln!(w, r#"  {} -> {}[label="{}"];"#, start_id, end_id, edge_label);
                    if !visited.contains(end) {
                        queue.push(*end);
                    }
                }
            }

            i += 1;
            visited.insert(start);
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