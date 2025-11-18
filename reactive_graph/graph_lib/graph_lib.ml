type node_id = int

type node = {
  id : node_id;
  marked : bool;
}

type edge = node_id * node_id

type file_graph = {
  file : string;
  nodes : node list;
  edges : edge list;
}

exception Duplicate_node_in_file of {
  file : string;
  node : node_id;
}

exception Duplicate_node_across_files of {
  node : node_id;
  first_file : string;
  second_file : string;
}

exception Edge_source_not_local of {
  file : string;
  src : node_id;
  dst : node_id;
}

exception Edge_target_undefined of {
  file : string;
  src : node_id;
  dst : node_id;
}

module Int = struct
  type t = int

  let compare = compare
end

module Int_set = Set.Make (Int)
module Int_map = Map.Make (Int)

let parse_error file line msg =
  invalid_arg (Printf.sprintf "%s:%d: %s" file (line + 1) msg)

let drop_comment line =
  match String.index_opt line '#' with
  | None -> line
  | Some idx -> String.sub line 0 idx

let normalize_whitespace line =
  let buf = Bytes.create (String.length line) in
  String.iteri
    (fun i ch ->
      match ch with
      | '\t' -> Bytes.set buf i ' '
      | _ -> Bytes.set buf i ch)
    line;
  Bytes.unsafe_to_string buf

let tokens line =
  normalize_whitespace line
  |> String.split_on_char ' '
  |> List.filter_map (fun token ->
         let trimmed = String.trim token in
         if trimmed = "" then None else Some trimmed)

let parse_node file line fields nodes_seen =
  match fields with
  | [ "node"; id ] ->
      let id =
        match int_of_string_opt id with
        | Some v -> v
        | None -> parse_error file line "node id must be an integer"
      in
      let nodes_seen =
        if Int_set.mem id nodes_seen then
          raise (Duplicate_node_in_file { file; node = id })
        else Int_set.add id nodes_seen
      in
      ({ id; marked = false }, nodes_seen)
  | [ "node"; id; "marked" ] ->
      let id =
        match int_of_string_opt id with
        | Some v -> v
        | None -> parse_error file line "node id must be an integer"
      in
      let nodes_seen =
        if Int_set.mem id nodes_seen then
          raise (Duplicate_node_in_file { file; node = id })
        else Int_set.add id nodes_seen
      in
      ({ id; marked = true }, nodes_seen)
  | _ -> parse_error file line "expected `node <int> [marked]`"

let parse_edge file line fields =
  match fields with
  | [ "edge"; src; dst ] ->
      let parse_int what value =
        match int_of_string_opt value with
        | Some v -> v
        | None -> parse_error file line (Printf.sprintf "%s must be an integer" what)
      in
      (parse_int "edge source" src, parse_int "edge target" dst)
  | _ -> parse_error file line "expected `edge <src> <dst>`"

let parse_file ~file contents =
  let lines = String.split_on_char '\n' contents in
  let nodes = ref [] in
  let edges = ref [] in
  let nodes_seen = ref Int_set.empty in
  List.iteri
    (fun idx raw_line ->
      let trimmed = raw_line |> drop_comment |> String.trim in
      if trimmed <> "" then
        match tokens trimmed with
        | [] -> ()
        | "node" :: _ as fields ->
            let node, updated = parse_node file idx fields !nodes_seen in
            nodes_seen := updated;
            nodes := node :: !nodes
        | "edge" :: _ as fields ->
            let edge = parse_edge file idx fields in
            edges := edge :: !edges
        | _ -> parse_error file idx "line must start with `node` or `edge`")
    lines;
  let nodes = List.rev !nodes in
  let edges = List.rev !edges in
  let node_ids = !nodes_seen in
  List.iter
    (fun (src, dst) ->
      if not (Int_set.mem src node_ids) then
        raise (Edge_source_not_local { file; src; dst }))
    edges;
  { file; nodes; edges }

let read_whole_file path =
  let ic = open_in path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let load_files paths =
  match paths with
  | [] -> invalid_arg "load_files: expected at least one file"
  | _ ->
      List.map
        (fun path ->
          let contents = read_whole_file path in
          parse_file ~file:path contents)
        paths

let unreachable_nodes files =
  let node_owner = Hashtbl.create 64 in
  let file_nodes = Hashtbl.create 16 in
  let all_nodes = ref Int_set.empty in
  let marked = ref Int_set.empty in
  let adjacency = ref Int_map.empty in
  List.iter
    (fun { file; nodes; _ } ->
      let locals =
        List.fold_left
          (fun acc node ->
            (match Hashtbl.find_opt node_owner node.id with
            | Some previous ->
                raise
                  (Duplicate_node_across_files
                     { node = node.id; first_file = previous; second_file = file })
            | None -> Hashtbl.add node_owner node.id file);
            all_nodes := Int_set.add node.id !all_nodes;
            if node.marked then marked := Int_set.add node.id !marked;
            Int_set.add node.id acc)
          Int_set.empty nodes
      in
      Hashtbl.replace file_nodes file locals)
    files;
  List.iter
    (fun { file; edges; _ } ->
      let locals =
        match Hashtbl.find_opt file_nodes file with
        | Some set -> set
        | None -> Int_set.empty
      in
      List.iter
        (fun (src, dst) ->
          if not (Int_set.mem src locals) then
            raise (Edge_source_not_local { file; src; dst });
          if not (Int_set.mem dst !all_nodes) then
            raise (Edge_target_undefined { file; src; dst });
          let neighbors =
            match Int_map.find_opt src !adjacency with
            | None -> Int_set.singleton dst
            | Some existing -> Int_set.add dst existing
          in
          adjacency := Int_map.add src neighbors !adjacency)
        edges)
    files;
  let rec bfs queue visited =
    match queue with
    | [] -> visited
    | node :: rest ->
        if Int_set.mem node visited then
          bfs rest visited
        else
          let visited = Int_set.add node visited in
          let neighbors =
            match Int_map.find_opt node !adjacency with
            | None -> Int_set.empty
            | Some ns -> ns
          in
          let queue = Int_set.fold (fun dst acc -> dst :: acc) neighbors rest in
          bfs queue visited
  in
  let reachable = bfs (Int_set.elements !marked) Int_set.empty in
  let unreachable = Int_set.diff !all_nodes reachable in
  Int_set.elements unreachable

