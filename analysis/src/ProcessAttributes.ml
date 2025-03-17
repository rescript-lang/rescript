open SharedTypes

(* TODO should I hang on to location? *)
let rec findDocAttribute attributes =
  let open Parsetree in
  match attributes with
  | [] -> None
  | ( {Asttypes.txt = "ocaml.doc" | "ocaml.text" | "ns.doc" | "res.doc"},
      PStr
        [
          {
            pstr_desc =
              Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (doc, _))}, _);
          };
        ] )
    :: _ ->
    Some doc
  | _ :: rest -> findDocAttribute rest

let rec findDeprecatedAttribute attributes =
  let open Parsetree in
  match attributes with
  | [] -> None
  | ( {Asttypes.txt = "deprecated"},
      PStr
        [
          {
            pstr_desc =
              Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (msg, _))}, _);
          };
        ] )
    :: _ ->
    Some msg
  | ({Asttypes.txt = "deprecated"}, _) :: _ -> Some ""
  | _ :: rest -> findDeprecatedAttribute rest

let newDeclared ~item ~extent ~name ~stamp ~modulePath isExported attributes =
  {
    Declared.name;
    stamp;
    extentLoc = extent;
    isExported;
    modulePath;
    deprecated = findDeprecatedAttribute attributes;
    docstring =
      (match findDocAttribute attributes with
      | None -> []
      | Some d -> [d]);
    item;
  }

let rec findEditorCompleteFromAttribute ?(modulePaths = []) attributes =
  let open Parsetree in
  match attributes with
  | [] -> modulePaths
  | ( {Asttypes.txt = "editor.completeFrom"},
      PStr [{pstr_desc = Pstr_eval (payloadExpr, _)}] )
    :: rest ->
    let items =
      match payloadExpr with
      | {pexp_desc = Pexp_array items} -> items
      | p -> [p]
    in
    let modulePathsFromArray =
      items
      |> List.filter_map (fun item ->
             match item.Parsetree.pexp_desc with
             | Pexp_construct ({txt = path}, None) ->
               Some (Utils.flattenLongIdent path)
             | _ -> None)
    in
    findEditorCompleteFromAttribute
      ~modulePaths:(modulePathsFromArray @ modulePaths)
      rest
  | _ :: rest -> findEditorCompleteFromAttribute ~modulePaths rest
