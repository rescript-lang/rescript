open Belt

type graph = list<(string, string)>

let graph: graph = list{
  ("a", "b"),
  ("a", "c"),
  ("a", "d"),
  ("b", "e"),
  ("c", "f"),
  ("d", "e"),
  ("e", "f"),
  ("e", "g"),
}

let nexts = (x: string, g: graph): list<string> =>
  g->List.reduce(list{}, (acc, (a, b)) =>
    if a == x {
      list{b, ...acc}
    } else {
      acc
    }
  )

let rec dfs1 = (nodes, graph, visited) =>
  switch nodes {
  | list{} => List.reverse(visited)
  | list{x, ...xs} =>
    if visited->List.has(x, \"==") {
      dfs1(xs, graph, visited)
    } else {
      Js.log(x)
      dfs1(\"@"(nexts(x, graph), xs), graph, list{x, ...visited})
    }
  }

let () = {
  assert(dfs1(list{"a"}, graph, list{}) == list{"a", "d", "e", "g", "f", "c", "b"})

  Js.log()
  assert(dfs1(list{"b"}, list{("f", "d"), ...graph}, list{}) == list{"b", "e", "g", "f", "d"})
}

let rec dfs2 = (nodes, graph, visited) => {
  let rec aux = (nodes, graph, visited) =>
    switch nodes {
    | list{} => visited
    | list{x, ...xs} =>
      if visited->List.has(x, \"==") {
        aux(xs, graph, visited)
      } else {
        aux(xs, graph, aux(nexts(x, graph), graph, list{x, ...visited}))
      }
    }
  aux(nodes, graph, visited)->List.reverse
}

let () = {
  let dfs1 = dfs2
  assert(dfs1(list{"a"}, graph, list{}) == list{"a", "d", "e", "g", "f", "c", "b"})
  assert(dfs1(list{"b"}, list{("f", "d"), ...graph}, list{}) == list{"b", "e", "g", "f", "d"})
}

let dfs3 = (nodes, graph) => {
  let visited = ref(list{})
  let rec aux = (node, graph) =>
    if !List.has(visited.contents, node, \"==") {
      visited := list{node, ...visited.contents}
      nexts(node, graph)->List.forEach(x => aux(x, graph))
    }
  nodes->List.forEach(node => aux(node, graph))
  visited.contents->List.reverse
}

let () = {
  let dfs1 = dfs3
  assert(dfs1(list{"a"}, graph) == list{"a", "d", "e", "g", "f", "c", "b"})
  assert(dfs1(list{"b"}, list{("f", "d"), ...graph}) == list{"b", "e", "g", "f", "d"})
}

/* since [x] is recorded before visiting its successors, so even with 
    cycles, it is still terminating
*/

let grwork = list{
  ("wake", "shower"),
  ("shower", "dress"),
  ("dress", "go"),
  ("wake", "eat"),
  ("eat", "washup"),
  ("washup", "go"),
}

let unsafe_topsort = graph => {
  let visited = ref(list{})
  let rec sort_nodes = nodes => nodes->List.forEach(node => sort_node(node))
  and sort_node = node =>
    if !List.has(visited.contents, node, \"==") {
      /* This check does not prevent cycle ,
           but it is still necessary? yes! 
           since a node can have multiple parents
 */

      sort_nodes(nexts(node, graph))
      /* different from dfs, recorded after its 
            successors have been searched for toplogoical
            sort, downside: can not detect cycles
 */
      visited := list{node, ...visited.contents}
    }
  graph->List.forEach(((x, _)) => sort_node(x))
  visited.contents
}

let () = assert(unsafe_topsort(grwork) == list{"wake", "shower", "dress", "eat", "washup", "go"})

module String_set = Belt.Set.String
exception Cycle(list<string>)
let pathsort = graph => {
  let visited = ref(list{})
  let empty_path = (String_set.empty, list{})
  let \"+>" = (node, (set, stack)) =>
    if String_set.has(set, node) {
      throw(Cycle(list{node, ...stack}))
    } else {
      (String_set.add(set, node), list{node, ...stack})
    }

  /* let check node (set,stack) = 
      if String_set.mem node set then 
        raise (Cycle (node::stack))  in */
  let rec sort_nodes = (path, nodes) => nodes->List.forEach(node => sort_node(path, node))
  and sort_node = (path, node) =>
    if !List.has(visited.contents, node, \"==") {
      /* check node path ; */
      sort_nodes(\"+>"(node, path), nexts(node, graph))
      /* different from dfs, recorded after its 
            successors have been searched for toplogoical
            sort, downside: can not detect cycles
 */
      visited := list{node, ...visited.contents}
    }
  graph->List.forEach(((x, _)) => sort_node(empty_path, x))
  visited.contents
}

let () = assert(pathsort(grwork) == list{"wake", "shower", "dress", "eat", "washup", "go"})

let () = try {
  pathsort(list{("go", "eat"), ...grwork})->ignore
  assert(false)
} catch {
| Cycle(list{"go", "washup", "eat", "go"}) => ()
| _ => assert(false)
}
