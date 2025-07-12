let tryReadCmt cmt =
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

let tryReadCmi cmi =
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

let digConstructor te =
  match (dig te).desc with
  | Tconstr (path, _args, _memo) -> Some path
  | _ -> None

let findTypeConstructors (tel : Types.type_expr list) =
  let paths = ref [] in
  let addPath path =
    if not (List.exists (Path.same path) !paths) then paths := path :: !paths
  in
  let rec loop (te : Types.type_expr) =
    match te.desc with
    | Tlink te1 | Tsubst te1 | Tpoly (te1, _) -> loop te1
    | Tconstr (path, args, _) ->
      addPath path;
      args |> List.iter loop
    | Tarrow (_, te1, te2, _, _) ->
      loop te1;
      loop te2
    | Ttuple tel -> tel |> List.iter loop
    | Tnil | Tvar _ | Tobject _ | Tfield _ | Tvariant _ | Tunivar _ | Tpackage _
      ->
      ()
  in
  tel |> List.iter loop;
  !paths |> List.rev

let declToString ?printNameAsIs ?(recStatus = Types.Trec_not) name t =
  PrintType.printDecl ?printNameAsIs ~recStatus name t

let cacheTypeToString = ref false
let typeTbl = Hashtbl.create 1

let typeToString ?lineWidth (t : Types.type_expr) =
  match
    if !cacheTypeToString then Hashtbl.find_opt typeTbl (t.id, t) else None
  with
  | None ->
    let s = PrintType.printExpr ?lineWidth t in
    Hashtbl.replace typeTbl (t.id, t) s;
    s
  | Some s -> s
