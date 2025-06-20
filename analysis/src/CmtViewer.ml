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

type filter = Cursor of (int * int) | Loc of Loc.t

let dump ?filter rescript_json cmt_path =
  let uri = Uri.fromPath (Filename.remove_extension cmt_path ^ ".res") in
  let package =
    let uri = Uri.fromPath rescript_json in
    Packages.getPackage ~uri |> Option.get
  in
  let moduleName =
    BuildSystem.namespacedName package.namespace (FindFiles.getName cmt_path)
  in
  match Cmt.fullForCmt ~moduleName ~package ~uri cmt_path with
  | None -> failwith (Format.sprintf "Could not load cmt for %s" cmt_path)
  | Some full ->
    let open SharedTypes in
    let open SharedTypes.Stamps in
    let applyFilter =
      match filter with
      | None -> fun _ -> true
      | Some (Cursor cursor) -> Loc.hasPos ~pos:cursor
      | Some (Loc loc) -> Loc.isInside loc
    in
    (match filter with
    | None -> ()
    | Some (Cursor (line, col)) ->
      Printf.printf "Filtering by cursor %d,%d\n" line col
    | Some (Loc loc) -> Printf.printf "Filtering by loc %s\n" (Loc.toString loc));

    Printf.printf "file moduleName: %s\n\n" full.file.moduleName;

    let stamps =
      full.file.stamps |> getEntries
      |> List.filter (fun (_, stamp) -> applyFilter (locOfKind stamp))
    in

    let total_stamps = List.length stamps in
    Printf.printf "Found %d stamps:\n%s" total_stamps
      (if total_stamps > 0 then "\n" else "");

    stamps
    |> List.sort (fun (_, a) (_, b) ->
           let aLoc = locOfKind a in
           let bLoc = locOfKind b in
           match compare aLoc.loc_start.pos_lnum bLoc.loc_start.pos_lnum with
           | 0 -> compare aLoc.loc_start.pos_cnum bLoc.loc_start.pos_cnum
           | c -> c)
    |> List.iter (fun (stamp, kind) ->
           match kind with
           | KType t ->
             Printf.printf "%d ktype        %s\n" stamp
               (Warnings.loc_to_string t.extentLoc)
           | KValue t ->
             Printf.printf "%d kvalue       %s\n" stamp
               (Warnings.loc_to_string t.extentLoc)
           | KModule t ->
             Printf.printf "%d kmodule      %s\n" stamp
               (Warnings.loc_to_string t.extentLoc)
           | KConstructor t ->
             Printf.printf "%d kconstructor %s\n" stamp
               (Warnings.loc_to_string t.extentLoc));

    (* dump the structure *)
    let rec dump_structure indent (structure : Module.structure) =
      if indent > 0 then Printf.printf "%s" (String.make indent ' ');
      Printf.printf "Structure %s:\n" structure.name;
      structure.items |> List.iter (dump_structure_item (indent + 2))
    and dump_structure_item indent item =
      if indent > 0 then Printf.printf "%s" (String.make indent ' ');
      let open Module in
      match item.kind with
      | Value _typedExpr ->
        Printf.printf "Value %s %s\n" item.name
          (Warnings.loc_to_string item.loc)
      | Type _ ->
        Printf.printf "Type %s %s\n" item.name (Warnings.loc_to_string item.loc)
      | Module {type_ = m} ->
        Printf.printf "Module %s %s\n" item.name
          (Warnings.loc_to_string item.loc);
        dump_module indent m
    and dump_module indent (module_ : Module.t) =
      match module_ with
      | Ident path -> Printf.printf "Module (Ident) %s\n" (Path.name path)
      | Structure structure -> dump_structure indent structure
      | Constraint (m1, m2) ->
        dump_module indent m1;
        dump_module indent m2
    in

    print_newline ();
    dump_structure 0 full.file.structure;

    (* Dump all locItems (typed nodes) *)
    let locItems =
      match full.extra with
      | {locItems} ->
        locItems |> List.filter (fun locItem -> applyFilter locItem.loc)
    in

    Printf.printf "\nFound %d locItems (typed nodes):\n\n"
      (List.length locItems);

    locItems
    |> List.sort (fun a b ->
           let aLoc = a.loc.Location.loc_start in
           let bLoc = b.loc.Location.loc_start in
           match compare aLoc.pos_lnum bLoc.pos_lnum with
           | 0 -> compare aLoc.pos_cnum bLoc.pos_cnum
           | c -> c)
    |> List.iter (fun {loc; locType} ->
           let locStr = Warnings.loc_to_string loc in
           let kindStr = SharedTypes.locTypeToString locType in
           Printf.printf "%s %s\n" locStr kindStr)
