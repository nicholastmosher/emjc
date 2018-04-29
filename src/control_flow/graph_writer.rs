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

pub fn write_to<W: Write>(w: &mut W, cfg: &Cfg) -> ioResult<()> {
    let _ = writeln!(w, "digraph {{");

    for from in cfg.iter() {
        let outgoing_edges = cfg.graph.get(&from);
        if outgoing_edges.is_none() { continue; }
        let outgoing_edges = outgoing_edges.unwrap();

        let start_id = from.0;
        for (end, edge) in outgoing_edges.iter() {
            let end_id = end.0;
            let edge_label = edge.display(&cfg.source_map);
            let _ = writeln!(w, r#"  {} -> {}[label="{}"];"#, start_id, end_id, edge_label);
        }
    }

    let _ = writeln!(w, "}}");
    Ok(())
}

pub fn write_annotated<F, W: Write>(w: &mut W, cfg: &Cfg, annotation: F) -> ioResult<()>
    where F: Fn(CfgNode) -> Option<String>
{
    let _ = writeln!(w, "digraph {{");

    for from in cfg.iter() {
        let outgoing_edges = cfg.graph.get(&from);
        if outgoing_edges.is_none() { continue; }
        let outgoing_edges = outgoing_edges.unwrap();

        let start_id = from.0;

        // Write node label, if any.
        if let Some(node_label) = annotation(from) {
            let _ = writeln!(w, r#"  {} [label="{}: {}"]"#, start_id, from.0, str::replace(&node_label, r#"""#, r#"'"#));
        }

        for (end, edge) in outgoing_edges.iter() {
            let end_id = end.0;
            let edge_label = edge.display(&cfg.source_map);
            let _ = writeln!(w, r#"  {} -> {}[label="{}"];"#, start_id, end_id, str::replace(&edge_label, r#"""#, r#"'"#));
        }
    }

    let _ = writeln!(w, "}}");
    Ok(())
}
