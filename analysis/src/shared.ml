let try_read_cmt cmt =
  if not (Files.exists cmt) then (
    Log.log ("Cmt file does not exist " ^ cmt);
    None)
  else
    match Cmt_format.read_cmt cmt with
    | exception Cmi_format.Error err ->
      let error_message =
        Cmi_format.report_error Format.str_formatter err;
        Format.flush_str_formatter ()
      in
      Log.log ("Invalid cmt format " ^ cmt ^ ": " ^ error_message);
      None
    | exception err ->
      Log.log ("Invalid cmt format " ^ cmt ^ ": " ^ Printexc.to_string err);
      None
    | x -> Some x

let try_read_cmi cmi =
  if not (Files.exists cmi) then None
  else
    match Cmt_format.read_cmi cmi with
    | exception _ ->
      Log.log ("Failed to load " ^ cmi);
      None
    | x -> Some x

let rec dig (te : Types.type_expr) =
  match te.desc with
  | Tlink inner -> dig inner
  | Tsubst inner -> dig inner
  | Tpoly (inner, _) -> dig inner
  | _ -> te

let dig_constructor te =
  match (dig te).desc with
  | Tconstr (path, _args, _memo) -> Some path
  | _ -> None

let find_type_constructors (tel : Types.type_expr list) =
  let paths = ref [] in
  let add_path path =
    if not (List.exists (Path.same path) !paths) then paths := path :: !paths
  in
  let rec loop (te : Types.type_expr) =
    match te.desc with
    | Tlink te1 | Tsubst te1 | Tpoly (te1, _) -> loop te1
    | Tconstr (path, args, _) ->
      add_path path;
      args |> List.iter loop
    | Tarrow (arg, ret, _, _) ->
      loop arg.typ;
      loop ret
    | Ttuple tel -> tel |> List.iter loop
    | Tnil | Tvar _ | Tobject _ | Tfield _ | Tvariant _ | Tunivar _ | Tpackage _
      ->
      ()
  in
  tel |> List.iter loop;
  !paths |> List.rev

let decl_to_string ?print_name_as_is ?(rec_status = Types.Trec_not) name t =
  Print_type.print_decl ?print_name_as_is ~rec_status name t

let cache_type_to_string = ref false
let type_tbl = Hashtbl.create 1

let type_to_string ?line_width (t : Types.type_expr) =
  match
    if !cache_type_to_string then Hashtbl.find_opt type_tbl (t.id, t) else None
  with
  | None ->
    let s = Print_type.print_expr ?line_width t in
    Hashtbl.replace type_tbl (t.id, t) s;
    s
  | Some s -> s
