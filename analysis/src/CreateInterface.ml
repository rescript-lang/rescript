module SourceFileExtractor = struct
  let create ~path =
    match Files.readFile path with
    | None -> [||]
    | Some text -> text |> String.split_on_char '\n' |> Array.of_list

  let extract lines ~posStart ~posEnd =
    let lineStart, colStart = posStart in
    let lineEnd, colEnd = posEnd in
    let res = ref [] in
    if lineStart < 0 || lineStart > lineEnd || lineEnd >= Array.length lines
    then []
    else (
      for n = lineEnd downto lineStart do
        let line = lines.(n) in
        let len = String.length line in
        if n = lineStart && n = lineEnd then (
          if colStart >= 0 && colStart < colEnd && colEnd <= len then
            let indent = String.make colStart ' ' in
            res :=
              (indent ^ String.sub line colStart (colEnd - colStart)) :: !res)
        else if n = lineStart then (
          if colStart >= 0 && colStart < len then
            let indent = String.make colStart ' ' in
            res := (indent ^ String.sub line colStart (len - colStart)) :: !res)
        else if n = lineEnd then (
          if colEnd > 0 && colEnd <= len then
            res := String.sub line 0 colEnd :: !res)
        else res := line :: !res
      done;
      !res)
end

module AttributesUtils : sig
  type t

  val make : string list -> t

  val contains : string -> t -> bool

  val toString : t -> string
end = struct
  type attribute = {line: int; offset: int; name: string}
  type t = attribute list
  type parseState = Search | Collect of int

  let make lines =
    let makeAttr lineIdx attrOffsetStart attrOffsetEnd line =
      {
        line = lineIdx;
        offset = attrOffsetStart;
        name = String.sub line attrOffsetStart (attrOffsetEnd - attrOffsetStart);
      }
    in
    let res = ref [] in
    lines
    |> List.iteri (fun lineIdx line ->
           let state = ref Search in
           for i = 0 to String.length line - 1 do
             let ch = line.[i] in
             match (!state, ch) with
             | Search, '@' -> state := Collect i
             | Collect attrOffset, ' ' ->
               res := makeAttr lineIdx attrOffset i line :: !res;
               state := Search
             | Search, _ | Collect _, _ -> ()
           done;

           match !state with
           | Collect attrOffset ->
             res :=
               makeAttr lineIdx attrOffset (String.length line) line :: !res
           | _ -> ());
    !res |> List.rev

  let contains attributeForSearch t =
    t |> List.exists (fun {name} -> name = attributeForSearch)

  let toString t =
    match t with
    | [] -> ""
    | {line} :: _ ->
      let prevLine = ref line in
      let buffer = ref "" in
      let res = ref [] in
      t
      |> List.iter (fun attr ->
             let {line; offset; name} = attr in

             if line <> !prevLine then (
               res := !buffer :: !res;
               buffer := "";
               prevLine := line);

             let indent = String.make (offset - String.length !buffer) ' ' in
             buffer := !buffer ^ indent ^ name);
      res := !buffer :: !res;
      !res |> List.rev |> String.concat "\n"
end

let printSignature ~extractor ~signature =
  Printtyp.reset_names ();
  let sigItemToString (item : Outcometree.out_sig_item) =
    item |> Res_outcome_printer.print_out_sig_item_doc
    |> Res_doc.to_string ~width:Res_multi_printer.default_print_width
  in

  let genSigStrForInlineAttr lines attributes id vd =
    let divider = if List.length lines > 1 then "\n" else " " in

    let sigStr =
      sigItemToString
        (Printtyp.tree_of_value_description id {vd with val_kind = Val_reg})
    in

    (attributes |> AttributesUtils.toString) ^ divider ^ sigStr ^ "\n"
  in

  let buf = Buffer.create 10 in

  let getComponentType (typ : Types.type_expr) =
    let reactElement =
      Ctype.newconstr (Pdot (Pident (Ident.create "React"), "element", 0)) []
    in
    match typ.desc with
    | Tarrow
        (_, {desc = Tconstr (Path.Pident propsId, typeArgs, _)}, retType, _, _)
      when Ident.name propsId = "props" ->
      Some (typeArgs, retType)
    | Tconstr
        ( Pdot (Pident {name = "React"}, "component", _),
          [{desc = Tconstr (Path.Pident propsId, typeArgs, _)}],
          _ )
      when Ident.name propsId = "props" ->
      Some (typeArgs, reactElement)
    | Tconstr
        ( Pdot (Pident {name = "React"}, "componentLike", _),
          [{desc = Tconstr (Path.Pident propsId, typeArgs, _)}; retType],
          _ )
      when Ident.name propsId = "props" ->
      Some (typeArgs, retType)
    | _ -> None
  in

  let rec processSignature ~indent (signature : Types.signature) : unit =
    match signature with
    | Sig_type
        (propsId, {type_params; type_kind = Type_record (labelDecls, _)}, _)
      :: Sig_value (makeId (* make *), makeValueDesc)
      :: rest
      when Ident.name propsId = "props"
           && getComponentType makeValueDesc.val_type <> None ->
      (* PPX V4 component declaration:
         type props = {...}
         let v = ...
      *)
      let newItemStr =
        let typeArgs, retType =
          match getComponentType makeValueDesc.val_type with
          | Some x -> x
          | None -> assert false
        in
        let rec mkFunType (labelDecls : Types.label_declaration list) =
          match labelDecls with
          | [] -> retType
          | labelDecl :: rest ->
            let propType =
              TypeUtils.instantiateType ~typeParams:type_params ~typeArgs
                labelDecl.ld_type
            in
            let lblName = labelDecl.ld_id |> Ident.name in
            let lbl =
              if labelDecl.ld_optional then Asttypes.Optional lblName
              else Labelled lblName
            in
            {
              retType with
              desc = Tarrow (lbl, propType, mkFunType rest, Cok, None);
            }
        in
        let funType =
          if List.length labelDecls = 0 (* No props *) then
            let tUnit =
              Ctype.newconstr (Path.Pident (Ident.create "unit")) []
            in
            {retType with desc = Tarrow (Nolabel, tUnit, retType, Cok, None)}
          else mkFunType labelDecls
        in
        sigItemToString
          (Printtyp.tree_of_value_description makeId
             {makeValueDesc with val_type = funType})
      in
      Buffer.add_string buf (indent ^ "@react.component\n");
      Buffer.add_string buf (indent ^ newItemStr ^ "\n");
      processSignature ~indent rest
    | Sig_module (id, modDecl, recStatus) :: rest ->
      let colonOrEquals =
        match modDecl.md_type with
        | Mty_alias _ -> " = "
        | _ -> ": "
      in
      Buffer.add_string buf
        (indent
        ^ (match recStatus with
          | Trec_not -> "module "
          | Trec_first -> "module rec "
          | Trec_next -> "and ")
        ^ Ident.name id ^ colonOrEquals);
      processModuleType ~indent modDecl.md_type;
      Buffer.add_string buf "\n";
      processSignature ~indent rest
    | Sig_modtype (id, mtd) :: rest ->
      let () =
        match mtd.mtd_type with
        | None ->
          Buffer.add_string buf (indent ^ "module type " ^ Ident.name id ^ "\n")
        | Some mt ->
          Buffer.add_string buf (indent ^ "module type " ^ Ident.name id ^ " = ");
          processModuleType ~indent mt;
          Buffer.add_string buf "\n"
      in
      processSignature ~indent rest
    | Sig_value (id, ({val_kind = Val_prim prim; val_loc} as vd)) :: items
      when prim.prim_native_name <> "" && prim.prim_native_name.[0] = '\132' ->
      (* Rescript primitive name, e.g. @val external ... *)
      let lines =
        let posStart, posEnd = Loc.range val_loc in
        extractor |> SourceFileExtractor.extract ~posStart ~posEnd
      in
      let attributes = AttributesUtils.make lines in

      if AttributesUtils.contains "@inline" attributes then
        (* Generate type signature for @inline declaration *)
        Buffer.add_string buf (genSigStrForInlineAttr lines attributes id vd)
      else
        (* Copy the external declaration verbatim from the implementation file *)
        Buffer.add_string buf ((lines |> String.concat "\n") ^ "\n");

      processSignature ~indent items
    | Sig_value (id, vd) :: items ->
      let newItemStr =
        sigItemToString (Printtyp.tree_of_value_description id vd)
      in
      Buffer.add_string buf (indent ^ newItemStr ^ "\n");
      processSignature ~indent items
    | Sig_type (id, typeDecl, resStatus) :: items ->
      let newItemStr =
        sigItemToString
          (Printtyp.tree_of_type_declaration id typeDecl resStatus)
      in
      Buffer.add_string buf (indent ^ newItemStr ^ "\n");
      processSignature ~indent items
    | Sig_typext (id, extConstr, extStatus) :: items ->
      let newItemStr =
        sigItemToString
          (Printtyp.tree_of_extension_constructor id extConstr extStatus)
      in
      Buffer.add_string buf (indent ^ newItemStr ^ "\n");
      processSignature ~indent items
    | Sig_class _ :: items ->
      (* not needed *)
      processSignature ~indent items
    | Sig_class_type _ :: items ->
      (* not needed *)
      processSignature ~indent items
    | [] -> ()
  and processModuleType ~indent (mt : Types.module_type) =
    match mt with
    | Mty_signature signature ->
      Buffer.add_string buf "{\n";
      processSignature ~indent:(indent ^ "  ") signature;
      Buffer.add_string buf (indent ^ "}")
    | Mty_functor _ ->
      let rec collectFunctorArgs ~args (mt : Types.module_type) =
        match mt with
        | Mty_functor (id, None, mt) when Ident.name id = "*" ->
          (* AST encoding of functor with no arguments *)
          collectFunctorArgs ~args mt
        | Mty_functor (id, mto, mt) ->
          collectFunctorArgs ~args:((id, mto) :: args) mt
        | mt -> (List.rev args, mt)
      in
      let args, retMt = collectFunctorArgs ~args:[] mt in
      Buffer.add_string buf "(";
      args
      |> List.iter (fun (id, mto) ->
             Buffer.add_string buf ("\n" ^ indent ^ "  ");
             (match mto with
             | None -> Buffer.add_string buf (Ident.name id)
             | Some mt ->
               Buffer.add_string buf (Ident.name id ^ ": ");
               processModuleType ~indent:(indent ^ "  ") mt);
             Buffer.add_string buf ",");
      if args <> [] then Buffer.add_string buf ("\n" ^ indent);
      Buffer.add_string buf (") =>\n" ^ indent);
      processModuleType ~indent retMt
    | Mty_ident path | Mty_alias (_, path) ->
      let rec outIdentToString (ident : Outcometree.out_ident) =
        match ident with
        | Oide_ident s -> s
        | Oide_dot (ident, s) -> outIdentToString ident ^ "." ^ s
        | Oide_apply (call, arg) ->
          outIdentToString call ^ "(" ^ outIdentToString arg ^ ")"
      in
      Buffer.add_string buf (outIdentToString (Printtyp.tree_of_path path))
  in

  processSignature ~indent:"" signature;
  Buffer.contents buf

let command ~path ~cmiFile =
  match Shared.tryReadCmi cmiFile with
  | Some cmi_info ->
    (* For reading the config *)
    let _ = Cmt.loadFullCmtFromPath ~path in
    let extractor = SourceFileExtractor.create ~path in
    printSignature ~extractor ~signature:cmi_info.cmi_sign
  | None -> ""
