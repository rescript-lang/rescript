type entry = {
  module_name: string;
  package_root: string;
  path: string;
  output: string;
}

type t = (string, entry) Hashtbl.t

let create () = Hashtbl.create 16

let key ~package_root ~path = Filename.concat package_root path

let set state ~module_name ~package_root ~path ~output =
  Hashtbl.replace state (key ~package_root ~path)
    {module_name; package_root; path; output}

let remove state ~package_root ~path =
  Hashtbl.remove state (key ~package_root ~path)

let retain_paths state paths =
  let current = Hashtbl.create (List.length paths) in
  List.iter (fun path -> Hashtbl.replace current path ()) paths;
  Hashtbl.filter_map_inplace
    (fun path entry ->
      if Hashtbl.mem current path then Some entry else None)
    state

let entries state =
  Hashtbl.to_seq_values state |> List.of_seq
  |> List.sort (fun first second ->
       match String.compare first.module_name second.module_name with
       | 0 -> String.compare first.path second.path
       | order -> order)
