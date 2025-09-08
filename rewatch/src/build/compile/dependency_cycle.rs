use super::super::build_types::*;
use crate::helpers;
use ahash::AHashSet;
use std::collections::{HashMap, HashSet, VecDeque};

pub fn find(modules: &Vec<(&String, &Module)>) -> Vec<String> {
    find_shortest_cycle(modules)
}

fn find_shortest_cycle(modules: &Vec<(&String, &Module)>) -> Vec<String> {
    let mut shortest_cycle: Vec<String> = Vec::new();

    // Build a graph representation for easier traversal

    let mut graph: HashMap<&String, &AHashSet<String>> = HashMap::new();
    let mut in_degrees: HashMap<&String, usize> = HashMap::new();

    let empty = AHashSet::new();
    // First pass: collect all nodes and initialize in-degrees
    for (name, _) in modules {
        graph.insert(name, &empty);
        in_degrees.insert(name, 0);
    }

    // Second pass: build the graph and count in-degrees
    for (name, module) in modules {
        // Update in-degrees
        for dep in module.deps.iter() {
            if let Some(count) = in_degrees.get_mut(dep) {
                *count += 1;
            }
        }

        // Update the graph
        *graph.get_mut(*name).unwrap() = &module.deps;
    }
    // Remove all nodes in the graph that have no incoming edges
    graph.retain(|_, deps| !deps.is_empty());

    // OPTIMIZATION 1: Start with nodes that are more likely to be in cycles
    // Sort nodes by their connectivity (in-degree + out-degree)
    let mut start_nodes: Vec<&String> = graph.keys().cloned().collect();
    start_nodes.sort_by(|a, b| {
        let a_connectivity = in_degrees.get(a).unwrap_or(&0) + graph.get(a).map_or(0, |v| v.len());
        let b_connectivity = in_degrees.get(b).unwrap_or(&0) + graph.get(b).map_or(0, |v| v.len());
        b_connectivity.cmp(&a_connectivity) // Sort in descending order
    });

    // OPTIMIZATION 2: Keep track of the current shortest cycle length for early termination
    let mut current_shortest_length = usize::MAX;

    // OPTIMIZATION 3: Cache nodes that have been checked and don't have cycles
    let mut no_cycle_cache: HashSet<String> = HashSet::new();

    // Try BFS from each node to find the shortest cycle
    for start_node in start_nodes {
        // Skip nodes that we know don't have cycles
        if no_cycle_cache.contains(start_node) {
            continue;
        }

        // Skip nodes with no incoming edges
        if in_degrees.get(&start_node).is_none_or(|&d| d == 0) {
            no_cycle_cache.insert(start_node.clone());
            continue;
        }

        if let Some(cycle) = find_cycle_bfs(start_node, &graph, current_shortest_length) {
            if shortest_cycle.is_empty() || cycle.len() < shortest_cycle.len() {
                shortest_cycle = cycle.clone();
                current_shortest_length = cycle.len();

                // OPTIMIZATION 4: If we find a very short cycle (length <= 3), we can stop early
                // as it's unlikely to find a shorter one
                if cycle.len() <= 3 {
                    break;
                }
            }
        } else {
            // Cache this node as not having a cycle
            no_cycle_cache.insert(start_node.to_string());
        }
    }

    shortest_cycle
}

fn find_cycle_bfs(
    start: &String,
    graph: &HashMap<&String, &AHashSet<String>>,
    max_length: usize,
) -> Option<Vec<String>> {
    // Use a BFS to find the shortest cycle
    let mut queue = VecDeque::new();
    // Store node -> (distance, parent)
    let mut visited: HashMap<String, (usize, Option<String>)> = HashMap::new();

    // Initialize with start node
    visited.insert(start.clone(), (0, None));
    queue.push_back(start.clone());

    while let Some(current) = queue.pop_front() {
        let (dist, _) = *visited.get(&current).unwrap();

        // OPTIMIZATION: Early termination if we've gone too far
        // If we're already at max_length, we won't find a shorter cycle from here
        if dist >= max_length - 1 {
            continue;
        }

        // Check all neighbors
        if let Some(neighbors) = graph.get(&current) {
            for neighbor in neighbors.iter() {
                // If we found the start node again, we have a cycle
                if neighbor == start {
                    // Reconstruct the cycle
                    let mut path = Vec::new();
                    path.push(start.clone());

                    // Backtrack from current to start using parent pointers
                    let mut curr = current.clone();
                    while curr != *start {
                        path.push(curr.clone());
                        curr = visited.get(&curr).unwrap().1.clone().unwrap();
                    }

                    return Some(path);
                }

                // If not visited, add to queue
                if !visited.contains_key(neighbor) {
                    visited.insert(neighbor.clone(), (dist + 1, Some(current.clone())));
                    queue.push_back(neighbor.clone());
                }
            }
        }
    }

    None
}

pub fn format(cycle: &[String]) -> String {
    let mut cycle = cycle.to_vec();
    cycle.reverse();
    // add the first module to the end of the cycle
    cycle.push(cycle[0].clone());

    cycle
        .iter()
        .map(|s| helpers::format_namespaced_module_name(s))
        .collect::<Vec<String>>()
        .join("\n → ")
}
