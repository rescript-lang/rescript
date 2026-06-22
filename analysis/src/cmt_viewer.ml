let filter_by_cursor cursor (loc : Warnings.loc) : bool =
  match cursor with
  | None -> true
  | Some (line, col) ->
    let start = loc.loc_start and end_ = loc.loc_end in
    let line_in = start.pos_lnum <= line && line <= end_.pos_lnum in
    let col_in =
      if start.pos_lnum = end_.pos_lnum then
        start.pos_cnum - start.pos_bol <= col
        && col <= end_.pos_cnum - end_.pos_bol
      else if line = start.pos_lnum then col >= start.pos_cnum - start.pos_bol
      else if line = end_.pos_lnum then col <= end_.pos_cnum - end_.pos_bol
      else true
    in
    line_in && col_in

let dump ~(filter_for_position : (int * int) option) ~full =
  let open Shared_types in
  let open Shared_types.Stamps in
  let buffer = Buffer.create 4096 in
  let printf fmt = Printf.bprintf buffer fmt in
  let apply_filter =
    match filter_for_position with
    | None -> fun _ -> true
    | Some cursor -> Loc.has_pos ~pos:cursor
  in
  (match filter_for_position with
  | None -> ()
  | Some (line, col) -> printf "Filtering by cursor %d,%d\n" line col);

  printf "file moduleName: %s\n\n" full.file.module_name;

  let stamps =
    full.file.stamps |> get_entries
    |> List.filter (fun (_, stamp) -> apply_filter (loc_of_kind stamp))
  in

  let total_stamps = List.length stamps in
  printf "Found %d stamps:\n%s" total_stamps
    (if total_stamps > 0 then "\n" else "");

  stamps
  |> List.sort (fun (_, a) (_, b) ->
         let a_loc = loc_of_kind a in
         let b_loc = loc_of_kind b in
         match compare a_loc.loc_start.pos_lnum b_loc.loc_start.pos_lnum with
         | 0 -> compare a_loc.loc_start.pos_cnum b_loc.loc_start.pos_cnum
         | c -> c)
  |> List.iter (fun (stamp, kind) ->
         match kind with
         | KType t ->
           printf "%d ktype        %s\n" stamp
             (Warnings.loc_to_string t.extent_loc)
         | KValue t ->
           printf "%d kvalue       %s\n" stamp
             (Warnings.loc_to_string t.extent_loc)
         | KModule t ->
           printf "%d kmodule      %s\n" stamp
             (Warnings.loc_to_string t.extent_loc)
         | KConstructor t ->
           printf "%d kconstructor %s\n" stamp
             (Warnings.loc_to_string t.extent_loc));

  (* dump the structure *)
  let rec dump_structure indent (structure : Module.structure) =
    if indent > 0 then printf "%s" (String.make indent ' ');
    printf "Structure %s:\n" structure.name;
    structure.items |> List.iter (dump_structure_item (indent + 2))
  and dump_structure_item indent item =
    if indent > 0 then printf "%s" (String.make indent ' ');
    let open Module in
    match item.kind with
    | Value _typedExpr ->
      printf "Value %s %s\n" item.name (Warnings.loc_to_string item.loc)
    | Type _ ->
      printf "Type %s %s\n" item.name (Warnings.loc_to_string item.loc)
    | Module {type_ = m} ->
      printf "Module %s %s\n" item.name (Warnings.loc_to_string item.loc);
      dump_module indent m
  and dump_module indent (module_ : Module.t) =
    match module_ with
    | Ident path -> printf "Module (Ident) %s\n" (Path.name path)
    | Structure structure -> dump_structure indent structure
    | Constraint (m1, m2) ->
      dump_module indent m1;
      dump_module indent m2
  in

  printf "\n";
  dump_structure 0 full.file.structure;

  (* Dump all locItems (typed nodes) *)
  let loc_items =
    match full.extra with
    | {loc_items} ->
      loc_items |> List.filter (fun loc_item -> apply_filter loc_item.loc)
  in

  printf "\nFound %d locItems (typed nodes):\n\n" (List.length loc_items);

  loc_items
  |> List.sort (fun a b ->
         let a_loc = a.loc.Location.loc_start in
         let b_loc = b.loc.Location.loc_start in
         match compare a_loc.pos_lnum b_loc.pos_lnum with
         | 0 -> compare a_loc.pos_cnum b_loc.pos_cnum
         | c -> c)
  |> List.iter (fun {loc; loc_type} ->
         let loc_str = Warnings.loc_to_string loc in
         let kind_str = Shared_types.loc_type_to_string loc_type in
         printf "%s %s\n" loc_str kind_str);
  Buffer.contents buffer
