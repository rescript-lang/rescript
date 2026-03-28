(*
   Generally speaking, semantic highlighting here takes care of categorizing identifiers,
   since the kind of an identifier is highly context-specific and hard to catch with a grammar.

   The big exception is labels, whose location is not represented in the AST
   E.g. function definition such as (~foo as _) =>, application (~foo=3) and prop <div foo=3>.
   Labels are handled in the grammar, not here.
   Punned labels such as (~foo) => are both labels and identifiers. They are overridden here.

   There are 2 cases where the grammar and semantic highlighting work jointly.
   The styles emitted in the grammar and here need to be kept in sync.
   1) For jsx angled brackets, the grammar handles basic cases such as />
      whose location is not in the AST.
      Instead < and > are handled here. Those would be difficult to disambiguate in a grammar.
   2) Most operators are handled in the grammar. Except < and > are handled here.
      The reason is again that < and > would be difficult do disambiguate in a grammar.
*)

module Token = struct
  (* This needs to stay synced with the same legend in `server.ts` *)
  (* See https://microsoft.github.io/language-server-protocol/specifications/specification-current/#textDocument_semanticTokens *)
  type token_type =
    | Operator  (** < and > *)
    | Variable  (** let x = *)
    | Type  (** type t = *)
    | JsxTag  (** the < and > in <div> *)
    | Namespace  (** module M = *)
    | EnumMember  (** variant A or poly variant #A *)
    | Property  (** {x:...} *)
    | JsxLowercase  (** div in <div> *)

  let token_type_to_int = function
    | Operator -> 0
    | Variable -> 1
    | Type -> 2
    | JsxTag -> 3
    | Namespace -> 4
    | EnumMember -> 5
    | Property -> 6
    | JsxLowercase -> 7

  let token_type_debug = function
    | Operator -> "Operator"
    | Variable -> "Variable"
    | Type -> "Type"
    | JsxTag -> "JsxTag"
    | Namespace -> "Namespace"
    | EnumMember -> "EnumMember"
    | Property -> "Property"
    | JsxLowercase -> "JsxLowercase"

  let token_modifiers = 0 (* None at the moment *)

  type token = int * int * int * token_type

  type emitter = {
    mutable tokens: token list;
    mutable last_line: int;
    mutable last_char: int;
  }

  let create_emitter () = {tokens = []; last_line = 0; last_char = 0}

  let add ~line ~char ~length ~type_ e =
    e.tokens <- (line, char, length, type_) :: e.tokens

  let emit_token (line, char, length, type_) e =
    let delta_line = line - e.last_line in
    let delta_char = if delta_line = 0 then char - e.last_char else char in
    e.last_line <- line;
    e.last_char <- char;
    if delta_line >= 0 && delta_char >= 0 && length >= 0 then
      Some
        [|
          delta_line;
          delta_char;
          length;
          token_type_to_int type_;
          token_modifiers;
        |]
    else None

  let emit e =
    let sorted_tokens =
      e.tokens
      |> List.sort (fun (l1, c1, _, _) (l2, c2, _, _) ->
             if l1 = l2 then compare c1 c2 else compare l1 l2)
    in
    let arrays =
      sorted_tokens |> List.filter_map (fun t -> e |> emit_token t)
    in
    Array.concat arrays

  let array_to_json_string arr =
    let items = Array.map string_of_int arr |> Array.to_list in
    "[" ^ String.concat "," items ^ "]"
end

let is_lowercase_id id =
  id <> ""
  &&
  let c = id.[0] in
  c == '_' || (c >= 'a' && c <= 'z')

let is_uppercase_id id =
  id <> ""
  &&
  let c = id.[0] in
  c >= 'A' && c <= 'Z'

let emit_from_range (pos_start, pos_end) ~type_ emitter =
  let length =
    if fst pos_start = fst pos_end then snd pos_end - snd pos_start else 0
  in
  if length > 0 then
    emitter
    |> Token.add ~line:(fst pos_start) ~char:(snd pos_start) ~length ~type_

let emit_from_loc ~loc ~type_ emitter =
  emitter |> emit_from_range (Loc.range loc) ~type_

let emit_longident ?(backwards = false) ?(jsx = false)
    ?(lower_case_token = if jsx then Token.JsxLowercase else Token.Variable)
    ?(upper_case_token = Token.Namespace) ?(last_token = None) ?(pos_end = None)
    ~pos ~lid ~debug emitter =
  let rec flatten acc lid =
    match lid with
    | Longident.Lident txt -> txt :: acc
    | Ldot (lid, txt) -> flatten (txt :: acc) lid
  in
  let rec loop pos segments =
    match segments with
    | [id] when is_uppercase_id id || is_lowercase_id id ->
      let type_ =
        match last_token with
        | Some type_ -> type_
        | None ->
          if is_uppercase_id id then upper_case_token else lower_case_token
      in
      let pos_after = (fst pos, snd pos + String.length id) in
      let pos_end, len_mismatch =
        (* There could be a length mismatch when ids are quoted
           e.g. variable /"true" or object field {"x":...} *)
        match pos_end with
        | Some pos_end -> (pos_end, pos_end <> pos_after)
        | None -> (pos_after, false)
      in
      if debug then
        Printf.printf "Lident: %s %s%s %s\n" id (Pos.to_string pos)
          (if len_mismatch then "->" ^ Pos.to_string pos_end else "")
          (Token.token_type_debug type_);
      emitter |> emit_from_range (pos, pos_end) ~type_
    | id :: segments when is_uppercase_id id || is_lowercase_id id ->
      let type_ =
        if is_uppercase_id id then upper_case_token else lower_case_token
      in
      if debug then
        Printf.printf "Ldot: %s %s %s\n" id (Pos.to_string pos)
          (Token.token_type_debug type_);
      let length = String.length id in
      emitter |> emit_from_range (pos, (fst pos, snd pos + length)) ~type_;
      loop (fst pos, snd pos + length + 1) segments
    | _ -> ()
  in
  let segments = flatten [] lid in
  if backwards then (
    let total_length = segments |> String.concat "." |> String.length in
    if snd pos >= total_length then
      loop (fst pos, snd pos - total_length) segments)
  else loop pos segments

let emit_variable ~id ~debug ~loc emitter =
  if debug then Printf.printf "Variable: %s %s\n" id (Loc.to_string loc);
  emitter |> emit_from_loc ~loc ~type_:Variable

let emit_jsx_open ~lid ~debug ~(loc : Location.t) emitter =
  if not loc.loc_ghost then
    emitter |> emit_longident ~pos:(Loc.start loc) ~lid ~jsx:true ~debug

let emit_jsx_close ~lid ~debug ~pos emitter =
  emitter |> emit_longident ~backwards:true ~pos ~lid ~jsx:true ~debug

let emit_jsx_tag ~debug ~name ~pos emitter =
  if debug then Printf.printf "JsxTag %s: %s\n" name (Pos.to_string pos);
  emitter |> emit_from_range (pos, (fst pos, snd pos + 1)) ~type_:Token.JsxTag

let emit_type ~lid ~debug ~(loc : Location.t) emitter =
  if not loc.loc_ghost then
    emitter
    |> emit_longident ~lower_case_token:Token.Type ~pos:(Loc.start loc) ~lid
         ~debug

let emit_record_label ~(label : Longident.t Location.loc) ~debug emitter =
  if not label.loc.loc_ghost then
    emitter
    |> emit_longident ~lower_case_token:Token.Property
         ~pos:(Loc.start label.loc)
         ~pos_end:(Some (Loc.end_ label.loc))
         ~lid:label.txt ~debug

let emit_variant ~(name : Longident.t Location.loc) ~debug emitter =
  if not name.loc.loc_ghost then
    emitter
    |> emit_longident ~last_token:(Some Token.EnumMember)
         ~pos:(Loc.start name.loc) ~lid:name.txt ~debug

let command ~debug ~emitter ~source ~kind_file =
  let process_type_arg (core_type : Parsetree.core_type) =
    if debug then
      Printf.printf "TypeArg: %s\n" (Loc.to_string core_type.ptyp_loc)
  in
  let typ (iterator : Ast_iterator.iterator) (core_type : Parsetree.core_type) =
    match core_type.ptyp_desc with
    | Ptyp_constr ({txt = lid; loc}, args) ->
      emitter |> emit_type ~lid ~debug ~loc;
      args |> List.iter process_type_arg;
      Ast_iterator.default_iterator.typ iterator core_type
    | _ -> Ast_iterator.default_iterator.typ iterator core_type
  in
  let type_declaration (iterator : Ast_iterator.iterator)
      (tydecl : Parsetree.type_declaration) =
    emitter
    |> emit_type ~lid:(Lident tydecl.ptype_name.txt) ~debug
         ~loc:tydecl.ptype_name.loc;
    Ast_iterator.default_iterator.type_declaration iterator tydecl
  in
  let pat (iterator : Ast_iterator.iterator) (p : Parsetree.pattern) =
    match p.ppat_desc with
    | Ppat_var {txt = id} ->
      if is_lowercase_id id then
        emitter |> emit_variable ~id ~debug ~loc:p.ppat_loc;
      Ast_iterator.default_iterator.pat iterator p
    | Ppat_construct ({txt = Lident ("true" | "false")}, _) ->
      (* Don't emit true or false *)
      Ast_iterator.default_iterator.pat iterator p
    | Ppat_record (cases, _, _rest) ->
      Ext_list.iter cases (fun {lid = label} ->
          emitter |> emit_record_label ~label ~debug);
      Ast_iterator.default_iterator.pat iterator p
    | Ppat_construct (name, _) ->
      emitter |> emit_variant ~name ~debug;
      Ast_iterator.default_iterator.pat iterator p
    | Ppat_type {txt = lid; loc} ->
      emitter |> emit_type ~lid ~debug ~loc;
      Ast_iterator.default_iterator.pat iterator p
    | _ -> Ast_iterator.default_iterator.pat iterator p
  in
  let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_ident {txt = lid; loc} ->
      if lid <> Lident "not" then
        if not loc.loc_ghost then (
          (* Don't emit semantic tokens for identifiers not present in source code *)
          let should_emit =
            match lid with
            (* Array spread (`...`) is converted to `Belt.Array.concatMany` with `@res.spread` decorator *)
            | Ldot (Ldot (Lident "Belt", "Array"), "concatMany") ->
              let has_spread_attr =
                e.pexp_attributes
                |> List.exists (fun ({Location.txt}, _) -> txt = "res.spread")
              in
              not has_spread_attr
            (* Dict syntax (`dict{...}`) is converted to `Primitive_dict.make` *)
            | Ldot (Lident "Primitive_dict", "make") -> false
            | Lident "Primitive_dict" -> false
            (* Array access (`arr[index]`) is converted to `Array.get` *)
            | Ldot (Lident "Array", "get") -> false
            (* Array mutation (`arr[index]`) is converted to `Array.set` *)
            | Ldot (Lident "Array", "set") -> false
            | _ -> true
          in
          if should_emit then
            emitter
            |> emit_longident ~pos:(Loc.start loc)
                 ~pos_end:(Some (Loc.end_ loc))
                 ~lid ~debug;
          Ast_iterator.default_iterator.expr iterator e)
    | Pexp_jsx_element (Jsx_unary_element {jsx_unary_element_tag_name = lident})
      ->
      (*
         Angled brackets:
          - These are handled in the grammar:  <>  </>  </  />
          - Here we handle `<` and `>`

         Component names:
          - handled like other Longitent.t, except lowercase id is marked Token.JsxLowercase
      *)
      emitter (* --> <div... *)
      |> emit_jsx_tag ~debug ~name:"<" ~pos:(Loc.start e.pexp_loc);
      let lid = Ast_helper.Jsx.longident_of_jsx_tag_name lident.txt in
      let loc = lident.loc in
      emitter |> emit_jsx_open ~lid ~debug ~loc;
      let closing_line, closing_column = Loc.end_ e.pexp_loc in
      emitter (* <foo ...props /> <-- *)
      |> emit_jsx_tag ~debug ~name:"/>" ~pos:(closing_line, closing_column - 2)
      (* minus two for /> *)
    | Pexp_jsx_element
        (Jsx_container_element
           {
             jsx_container_element_tag_name_start = lident;
             jsx_container_element_opening_tag_end =
               pos_of_greatherthan_after_props;
             jsx_container_element_children = children;
             jsx_container_element_closing_tag = closing_tag_opt;
           }) ->
      (* opening tag *)
      emitter (* --> <div... *)
      |> emit_jsx_tag ~debug ~name:"<" ~pos:(Loc.start e.pexp_loc);
      let lid = Ast_helper.Jsx.longident_of_jsx_tag_name lident.txt in
      let loc = lident.loc in
      emitter |> emit_jsx_open ~lid ~debug ~loc;
      emitter (* <foo ...props > <-- *)
      |> emit_jsx_tag ~debug ~name:">"
           ~pos:(Pos.of_lexing pos_of_greatherthan_after_props);

      (* children *)
      List.iter (iterator.expr iterator) children;

      (* closing tag *)
      closing_tag_opt
      |> Option.iter
           (fun
             {
               (* </ *)
               Parsetree.jsx_closing_container_tag_start = closing_less_than;
               (* name *)
               jsx_closing_container_tag_name = tag_name_end;
               (* > *)
               jsx_closing_container_tag_end = final_greather_than;
             }
           ->
             emitter
             |> emit_jsx_tag ~debug ~name:"</"
                  ~pos:(Pos.of_lexing closing_less_than);
             let lid =
               Ast_helper.Jsx.longident_of_jsx_tag_name tag_name_end.txt
             in
             let loc = tag_name_end.loc in
             emitter |> emit_jsx_close ~debug ~lid ~pos:(Loc.end_ loc);
             emitter (* <foo> ... </foo> <-- *)
             |> emit_jsx_tag ~debug ~name:">"
                  ~pos:(Pos.of_lexing final_greather_than))
    | Pexp_apply
        {
          funct =
            {
              pexp_desc =
                Pexp_ident {txt = Longident.Lident (("<" | ">") as op); loc};
            };
          args = [_; _];
        } ->
      if debug then
        Printf.printf "Binary operator %s %s\n" op (Loc.to_string loc);
      emitter |> emit_from_loc ~loc ~type_:Operator;
      Ast_iterator.default_iterator.expr iterator e
    | Pexp_record (cases, _) ->
      Ext_list.filter_map cases (fun {lid} ->
          match lid.txt with
          | Longident.Lident s when not (Utils.is_first_char_uppercase s) ->
            Some lid
          | _ -> None)
      |> List.iter (fun label -> emitter |> emit_record_label ~label ~debug);
      Ast_iterator.default_iterator.expr iterator e
    | Pexp_field (_, label) | Pexp_setfield (_, label, _) ->
      emitter |> emit_record_label ~label ~debug;
      Ast_iterator.default_iterator.expr iterator e
    | Pexp_construct ({txt = Lident ("true" | "false")}, _) ->
      (* Don't emit true or false *)
      Ast_iterator.default_iterator.expr iterator e
    | Pexp_construct (name, _) ->
      emitter |> emit_variant ~name ~debug;
      Ast_iterator.default_iterator.expr iterator e
    | _ -> Ast_iterator.default_iterator.expr iterator e
  in
  let module_expr (iterator : Ast_iterator.iterator)
      (me : Parsetree.module_expr) =
    match me.pmod_desc with
    | Pmod_ident {txt = lid; loc} ->
      if not loc.loc_ghost then
        emitter |> emit_longident ~pos:(Loc.start loc) ~lid ~debug;
      Ast_iterator.default_iterator.module_expr iterator me
    | _ -> Ast_iterator.default_iterator.module_expr iterator me
  in
  let module_binding (iterator : Ast_iterator.iterator)
      (mb : Parsetree.module_binding) =
    if not mb.pmb_name.loc.loc_ghost then
      emitter
      |> emit_longident
           ~pos:(Loc.start mb.pmb_name.loc)
           ~lid:(Longident.Lident mb.pmb_name.txt) ~debug;
    Ast_iterator.default_iterator.module_binding iterator mb
  in
  let module_declaration (iterator : Ast_iterator.iterator)
      (md : Parsetree.module_declaration) =
    if not md.pmd_name.loc.loc_ghost then
      emitter
      |> emit_longident
           ~pos:(Loc.start md.pmd_name.loc)
           ~lid:(Longident.Lident md.pmd_name.txt) ~debug;
    Ast_iterator.default_iterator.module_declaration iterator md
  in
  let module_type (iterator : Ast_iterator.iterator)
      (mt : Parsetree.module_type) =
    match mt.pmty_desc with
    | Pmty_ident {txt = lid; loc} ->
      if not loc.loc_ghost then
        emitter
        |> emit_longident ~upper_case_token:Token.Type ~pos:(Loc.start loc) ~lid
             ~debug;
      Ast_iterator.default_iterator.module_type iterator mt
    | _ -> Ast_iterator.default_iterator.module_type iterator mt
  in
  let module_type_declaration (iterator : Ast_iterator.iterator)
      (mtd : Parsetree.module_type_declaration) =
    if not mtd.pmtd_name.loc.loc_ghost then
      emitter
      |> emit_longident ~upper_case_token:Token.Type
           ~pos:(Loc.start mtd.pmtd_name.loc)
           ~lid:(Longident.Lident mtd.pmtd_name.txt) ~debug;
    Ast_iterator.default_iterator.module_type_declaration iterator mtd
  in
  let open_description (iterator : Ast_iterator.iterator)
      (od : Parsetree.open_description) =
    if not od.popen_lid.loc.loc_ghost then
      emitter
      |> emit_longident
           ~pos:(Loc.start od.popen_lid.loc)
           ~lid:od.popen_lid.txt ~debug;
    Ast_iterator.default_iterator.open_description iterator od
  in
  let label_declaration (iterator : Ast_iterator.iterator)
      (ld : Parsetree.label_declaration) =
    emitter
    |> emit_record_label
         ~label:{loc = ld.pld_name.loc; txt = Longident.Lident ld.pld_name.txt}
         ~debug;
    Ast_iterator.default_iterator.label_declaration iterator ld
  in
  let constructor_declaration (iterator : Ast_iterator.iterator)
      (cd : Parsetree.constructor_declaration) =
    emitter
    |> emit_variant
         ~name:{loc = cd.pcd_name.loc; txt = Longident.Lident cd.pcd_name.txt}
         ~debug;
    Ast_iterator.default_iterator.constructor_declaration iterator cd
  in

  let structure_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.structure_item) =
    (match item.pstr_desc with
    | Pstr_primitive {pval_name = {txt = id; loc}} ->
      emitter |> emit_variable ~id ~debug ~loc
    | _ -> ());
    Ast_iterator.default_iterator.structure_item iterator item
  in

  let signature_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.signature_item) =
    (match item.psig_desc with
    | Psig_value {pval_name = {txt = id; loc}} ->
      emitter |> emit_variable ~id ~debug ~loc
    | _ -> ());
    Ast_iterator.default_iterator.signature_item iterator item
  in

  let iterator =
    {
      Ast_iterator.default_iterator with
      constructor_declaration;
      expr;
      label_declaration;
      module_declaration;
      module_binding;
      module_expr;
      module_type;
      module_type_declaration;
      open_description;
      pat;
      typ;
      type_declaration;
      structure_item;
      signature_item;
    }
  in

  if kind_file = Files.Res then (
    let parser =
      Res_driver.parsing_engine.parse_implementation_from_source
        ~for_printer:false
    in
    let {Res_driver.parsetree = structure; diagnostics} = parser ~source in
    if debug then
      Printf.printf "structure items:%d diagnostics:%d \n"
        (List.length structure) (List.length diagnostics);
    iterator.structure iterator structure |> ignore)
  else
    let parser =
      Res_driver.parsing_engine.parse_interface_from_source ~for_printer:false
    in
    let {Res_driver.parsetree = signature; diagnostics} = parser ~source in
    if debug then
      Printf.printf "signature items:%d diagnostics:%d \n"
        (List.length signature) (List.length diagnostics);
    iterator.signature iterator signature |> ignore

let semantic_tokens ~source ~kind_file =
  let emitter = Token.create_emitter () in
  command ~emitter ~debug:false ~source ~kind_file;
  Lsp.Types.SemanticTokens.create ~data:(Token.emit emitter) ()
