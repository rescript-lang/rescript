type item = SharedTypes.ScopeTypes.item

type t = item list

open SharedTypes.ScopeTypes

let item_to_string item =
  let str s = if s = "" then "\"\"" else s in
  let list l = "[" ^ (l |> List.map str |> String.concat ", ") ^ "]" in
  match item with
  | Constructor (s, loc) -> "Constructor " ^ s ^ " " ^ Loc.to_string loc
  | Field (s, loc) -> "Field " ^ s ^ " " ^ Loc.to_string loc
  | Open sl -> "Open " ^ list sl
  | Module (s, loc) -> "Module " ^ s ^ " " ^ Loc.to_string loc
  | Value (s, loc, _, _) -> "Value " ^ s ^ " " ^ Loc.to_string loc
  | Type (s, loc) -> "Type " ^ s ^ " " ^ Loc.to_string loc
  | Include (s, loc) -> "Include " ^ s ^ " " ^ Loc.to_string loc
[@@live]

let create () : t = []
let add_constructor ~name ~loc x = Constructor (name, loc) :: x
let add_field ~name ~loc x = Field (name, loc) :: x
let add_module ~name ~loc x = Module (name, loc) :: x
let add_open ~lid x =
  Open (Utils.flatten_long_ident lid @ ["place holder"]) :: x
let add_value ~name ~loc ?context_path x =
  let show_debug = !Cfg.debug_follow_ctx_path in
  (if show_debug then
     match context_path with
     | None -> Printf.printf "adding value '%s', no ctxPath\n" name
     | Some context_path ->
       if show_debug then
         Printf.printf "adding value '%s' with ctxPath: %s\n" name
           (SharedTypes.Completable.context_path_to_string context_path));
  Value (name, loc, context_path, x) :: x
let add_type ~name ~loc x = Type (name, loc) :: x
let add_include ~name ~loc x = Include (name, loc) :: x

let iter_values_before_first_open f x =
  let rec loop items =
    match items with
    | Value (s, loc, context_path, scope) :: rest ->
      f s loc context_path scope;
      loop rest
    | Open _ :: _ -> ()
    | _ :: rest -> loop rest
    | [] -> ()
  in
  loop x

let iter_values_after_first_open f x =
  let rec loop found_open items =
    match items with
    | Value (s, loc, context_path, scope) :: rest ->
      if found_open then f s loc context_path scope;
      loop found_open rest
    | Open _ :: rest -> loop true rest
    | _ :: rest -> loop found_open rest
    | [] -> ()
  in
  loop false x

let iter_constructors_before_first_open f x =
  let rec loop items =
    match items with
    | Constructor (s, loc) :: rest ->
      f s loc;
      loop rest
    | Open _ :: _ -> ()
    | _ :: rest -> loop rest
    | [] -> ()
  in
  loop x

let iter_constructors_after_first_open f x =
  let rec loop found_open items =
    match items with
    | Constructor (s, loc) :: rest ->
      if found_open then f s loc;
      loop found_open rest
    | Open _ :: rest -> loop true rest
    | _ :: rest -> loop found_open rest
    | [] -> ()
  in
  loop false x

let iter_types_before_first_open f x =
  let rec loop items =
    match items with
    | Type (s, loc) :: rest ->
      f s loc;
      loop rest
    | Open _ :: _ -> ()
    | _ :: rest -> loop rest
    | [] -> ()
  in
  loop x

let iter_types_after_first_open f x =
  let rec loop found_open items =
    match items with
    | Type (s, loc) :: rest ->
      if found_open then f s loc;
      loop found_open rest
    | Open _ :: rest -> loop true rest
    | _ :: rest -> loop found_open rest
    | [] -> ()
  in
  loop false x

let iter_modules_before_first_open f x =
  let rec loop items =
    match items with
    | Module (s, loc) :: rest ->
      f s loc;
      loop rest
    | Open _ :: _ -> ()
    | _ :: rest -> loop rest
    | [] -> ()
  in
  loop x

let iter_modules_after_first_open f x =
  let rec loop found_open items =
    match items with
    | Module (s, loc) :: rest ->
      if found_open then f s loc;
      loop found_open rest
    | Open _ :: rest -> loop true rest
    | _ :: rest -> loop found_open rest
    | [] -> ()
  in
  loop false x

let iter_includes f x =
  let rec loop items =
    match items with
    | [] -> ()
    | Include (s, loc) :: rest ->
      f s loc;
      loop rest
    | _ :: rest -> loop rest
  in
  loop x

let get_raw_opens x =
  x
  |> Utils.filter_map (function
       | Open path -> Some path
       | _ -> None)
