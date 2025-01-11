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
  type tokenType =
    | Operator  (** < and > *)
    | Variable  (** let x = *)
    | Type  (** type t = *)
    | JsxTag  (** the < and > in <div> *)
    | Namespace  (** module M = *)
    | EnumMember  (** variant A or poly variant #A *)
    | Property  (** {x:...} *)
    | JsxLowercase  (** div in <div> *)

  let tokenTypeToString = function
    | Operator -> "0"
    | Variable -> "1"
    | Type -> "2"
    | JsxTag -> "3"
    | Namespace -> "4"
    | EnumMember -> "5"
    | Property -> "6"
    | JsxLowercase -> "7"

  let tokenTypeDebug = function
    | Operator -> "Operator"
    | Variable -> "Variable"
    | Type -> "Type"
    | JsxTag -> "JsxTag"
    | Namespace -> "Namespace"
    | EnumMember -> "EnumMember"
    | Property -> "Property"
    | JsxLowercase -> "JsxLowercase"

  let tokenModifiersString = "0" (* None at the moment *)

  type token = int * int * int * tokenType

  type emitter = {
    mutable tokens: token list;
    mutable lastLine: int;
    mutable lastChar: int;
  }

  let createEmitter () = {tokens = []; lastLine = 0; lastChar = 0}

  let add ~line ~char ~length ~type_ e =
    e.tokens <- (line, char, length, type_) :: e.tokens

  let emitToken buf (line, char, length, type_) e =
    let deltaLine = line - e.lastLine in
    let deltaChar = if deltaLine = 0 then char - e.lastChar else char in
    e.lastLine <- line;
    e.lastChar <- char;
    if Buffer.length buf > 0 then Buffer.add_char buf ',';
    if
      deltaLine >= 0 && deltaChar >= 0 && length >= 0
      (* Defensive programming *)
    then
      Buffer.add_string buf
        (string_of_int deltaLine ^ "," ^ string_of_int deltaChar ^ ","
       ^ string_of_int length ^ "," ^ tokenTypeToString type_ ^ ","
       ^ tokenModifiersString)

  let emit e =
    let sortedTokens =
      e.tokens
      |> List.sort (fun (l1, c1, _, _) (l2, c2, _, _) ->
             if l1 = l2 then compare c1 c2 else compare l1 l2)
    in
    let buf = Buffer.create 1 in
    sortedTokens |> List.iter (fun t -> e |> emitToken buf t);
    Buffer.contents buf
end

let isLowercaseId id =
  id <> ""
  &&
  let c = id.[0] in
  c == '_' || (c >= 'a' && c <= 'z')

let isUppercaseId id =
  id <> ""
  &&
  let c = id.[0] in
  c >= 'A' && c <= 'Z'

let emitFromRange (posStart, posEnd) ~type_ emitter =
  let length =
    if fst posStart = fst posEnd then snd posEnd - snd posStart else 0
  in
  if length > 0 then
    emitter
    |> Token.add ~line:(fst posStart) ~char:(snd posStart) ~length ~type_

let emitFromLoc ~loc ~type_ emitter =
  emitter |> emitFromRange (Loc.range loc) ~type_

let emitLongident ?(backwards = false) ?(jsx = false)
    ?(lowerCaseToken = if jsx then Token.JsxLowercase else Token.Variable)
    ?(upperCaseToken = Token.Namespace) ?(lastToken = None) ?(posEnd = None)
    ~pos ~lid ~debug emitter =
  let rec flatten acc lid =
    match lid with
    | Longident.Lident txt -> txt :: acc
    | Ldot (lid, txt) ->
      let acc = if jsx && txt = "createElement" then acc else txt :: acc in
      flatten acc lid
    | _ -> acc
  in
  let rec loop pos segments =
    match segments with
    | [id] when isUppercaseId id || isLowercaseId id ->
      let type_ =
        match lastToken with
        | Some type_ -> type_
        | None -> if isUppercaseId id then upperCaseToken else lowerCaseToken
      in
      let posAfter = (fst pos, snd pos + String.length id) in
      let posEnd, lenMismatch =
        (* There could be a length mismatch when ids are quoted
           e.g. variable /"true" or object field {"x":...} *)
        match posEnd with
        | Some posEnd -> (posEnd, posEnd <> posAfter)
        | None -> (posAfter, false)
      in
      if debug then
        Printf.printf "Lident: %s %s%s %s\n" id (Pos.toString pos)
          (if lenMismatch then "->" ^ Pos.toString posEnd else "")
          (Token.tokenTypeDebug type_);
      emitter |> emitFromRange (pos, posEnd) ~type_
    | id :: segments when isUppercaseId id || isLowercaseId id ->
      let type_ = if isUppercaseId id then upperCaseToken else lowerCaseToken in
      if debug then
        Printf.printf "Ldot: %s %s %s\n" id (Pos.toString pos)
          (Token.tokenTypeDebug type_);
      let length = String.length id in
      emitter |> emitFromRange (pos, (fst pos, snd pos + length)) ~type_;
      loop (fst pos, snd pos + length + 1) segments
    | _ -> ()
  in
  let segments = flatten [] lid in
  if backwards then (
    let totalLength = segments |> String.concat "." |> String.length in
    if snd pos >= totalLength then
      loop (fst pos, snd pos - totalLength) segments)
  else loop pos segments

let emitVariable ~id ~debug ~loc emitter =
  if debug then Printf.printf "Variable: %s %s\n" id (Loc.toString loc);
  emitter |> emitFromLoc ~loc ~type_:Variable

let emitJsxOpen ~lid ~debug ~(loc : Location.t) emitter =
  if not loc.loc_ghost then
    emitter |> emitLongident ~pos:(Loc.start loc) ~lid ~jsx:true ~debug

let emitJsxClose ~lid ~debug ~pos emitter =
  emitter |> emitLongident ~backwards:true ~pos ~lid ~jsx:true ~debug

let emitJsxTag ~debug ~name ~pos emitter =
  if debug then Printf.printf "JsxTag %s: %s\n" name (Pos.toString pos);
  emitter |> emitFromRange (pos, (fst pos, snd pos + 1)) ~type_:Token.JsxTag

let emitType ~lid ~debug ~(loc : Location.t) emitter =
  if not loc.loc_ghost then
    emitter
    |> emitLongident ~lowerCaseToken:Token.Type ~pos:(Loc.start loc) ~lid ~debug

let emitRecordLabel ~(label : Longident.t Location.loc) ~debug emitter =
  if not label.loc.loc_ghost then
    emitter
    |> emitLongident ~lowerCaseToken:Token.Property ~pos:(Loc.start label.loc)
         ~posEnd:(Some (Loc.end_ label.loc))
         ~lid:label.txt ~debug

let emitVariant ~(name : Longident.t Location.loc) ~debug emitter =
  if not name.loc.loc_ghost then
    emitter
    |> emitLongident ~lastToken:(Some Token.EnumMember)
         ~pos:(Loc.start name.loc) ~lid:name.txt ~debug

let command ~debug ~emitter ~path =
  let processTypeArg (coreType : Parsetree.core_type) =
    if debug then Printf.printf "TypeArg: %s\n" (Loc.toString coreType.ptyp_loc)
  in
  let typ (iterator : Ast_iterator.iterator) (coreType : Parsetree.core_type) =
    match coreType.ptyp_desc with
    | Ptyp_constr ({txt = lid; loc}, args) ->
      emitter |> emitType ~lid ~debug ~loc;
      args |> List.iter processTypeArg;
      Ast_iterator.default_iterator.typ iterator coreType
    | _ -> Ast_iterator.default_iterator.typ iterator coreType
  in
  let type_declaration (iterator : Ast_iterator.iterator)
      (tydecl : Parsetree.type_declaration) =
    emitter
    |> emitType ~lid:(Lident tydecl.ptype_name.txt) ~debug
         ~loc:tydecl.ptype_name.loc;
    Ast_iterator.default_iterator.type_declaration iterator tydecl
  in
  let pat (iterator : Ast_iterator.iterator) (p : Parsetree.pattern) =
    match p.ppat_desc with
    | Ppat_var {txt = id} ->
      if isLowercaseId id then
        emitter |> emitVariable ~id ~debug ~loc:p.ppat_loc;
      Ast_iterator.default_iterator.pat iterator p
    | Ppat_construct ({txt = Lident ("true" | "false")}, _) ->
      (* Don't emit true or false *)
      Ast_iterator.default_iterator.pat iterator p
    | Ppat_record (cases, _) ->
      cases
      |> List.iter (fun (label, _, _) ->
             emitter |> emitRecordLabel ~label ~debug);
      Ast_iterator.default_iterator.pat iterator p
    | Ppat_construct (name, _) ->
      emitter |> emitVariant ~name ~debug;
      Ast_iterator.default_iterator.pat iterator p
    | Ppat_type {txt = lid; loc} ->
      emitter |> emitType ~lid ~debug ~loc;
      Ast_iterator.default_iterator.pat iterator p
    | _ -> Ast_iterator.default_iterator.pat iterator p
  in
  let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_ident {txt = lid; loc} ->
      if lid <> Lident "not" then
        if not loc.loc_ghost then
          emitter
          |> emitLongident ~pos:(Loc.start loc)
               ~posEnd:(Some (Loc.end_ loc))
               ~lid ~debug;
      Ast_iterator.default_iterator.expr iterator e
    | Pexp_apply {funct = {pexp_desc = Pexp_ident lident; pexp_loc}; args}
      when Res_parsetree_viewer.is_jsx_expression e ->
      (*
         Angled brackets:
          - These are handled in the grammar:  <>  </>  </  />
          - Here we handle `<` and `>`

         Component names:
          - handled like other Longitent.t, except lowercase id is marked Token.JsxLowercase
      *)
      emitter (* --> <div... *)
      |> emitJsxTag ~debug ~name:"<"
           ~pos:
             (let pos = Loc.start e.pexp_loc in
              (fst pos, snd pos - 1 (* the AST skips the loc of < somehow *)));
      emitter |> emitJsxOpen ~lid:lident.txt ~debug ~loc:pexp_loc;

      let posOfGreatherthanAfterProps =
        let rec loop = function
          | (Asttypes.Labelled "children", {Parsetree.pexp_loc}) :: _ ->
            Loc.start pexp_loc
          | _ :: args -> loop args
          | [] -> (* should not happen *) (-1, -1)
        in

        loop args
      in
      let posOfFinalGreatherthan =
        let pos = Loc.end_ e.pexp_loc in
        (fst pos, snd pos - 1)
      in
      let selfClosing =
        fst posOfGreatherthanAfterProps == fst posOfFinalGreatherthan
        && snd posOfGreatherthanAfterProps + 1 == snd posOfFinalGreatherthan
        (* there's an off-by one somehow in the AST *)
      in
      (if not selfClosing then
         let lineStart, colStart = Loc.start pexp_loc in
         let lineEnd, colEnd = Loc.end_ pexp_loc in
         let length = if lineStart = lineEnd then colEnd - colStart else 0 in
         let lineEndWhole, colEndWhole = Loc.end_ e.pexp_loc in
         if length > 0 && colEndWhole > length then (
           emitter
           |> emitJsxClose ~debug ~lid:lident.txt
                ~pos:(lineEndWhole, colEndWhole - 1);
           emitter (* <foo ...props > <-- *)
           |> emitJsxTag ~debug ~name:">" ~pos:posOfGreatherthanAfterProps;
           emitter (* <foo> ... </foo> <-- *)
           |> emitJsxTag ~debug ~name:">" ~pos:posOfFinalGreatherthan));

      args |> List.iter (fun (_lbl, arg) -> iterator.expr iterator arg)
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
        Printf.printf "Binary operator %s %s\n" op (Loc.toString loc);
      emitter |> emitFromLoc ~loc ~type_:Operator;
      Ast_iterator.default_iterator.expr iterator e
    | Pexp_record (cases, _) ->
      cases
      |> List.filter_map (fun ((label : Longident.t Location.loc), _, _) ->
             match label.txt with
             | Longident.Lident s when not (Utils.isFirstCharUppercase s) ->
               Some label
             | _ -> None)
      |> List.iter (fun label -> emitter |> emitRecordLabel ~label ~debug);
      Ast_iterator.default_iterator.expr iterator e
    | Pexp_field (_, label) | Pexp_setfield (_, label, _) ->
      emitter |> emitRecordLabel ~label ~debug;
      Ast_iterator.default_iterator.expr iterator e
    | Pexp_construct ({txt = Lident ("true" | "false")}, _) ->
      (* Don't emit true or false *)
      Ast_iterator.default_iterator.expr iterator e
    | Pexp_construct (name, _) ->
      emitter |> emitVariant ~name ~debug;
      Ast_iterator.default_iterator.expr iterator e
    | _ -> Ast_iterator.default_iterator.expr iterator e
  in
  let module_expr (iterator : Ast_iterator.iterator)
      (me : Parsetree.module_expr) =
    match me.pmod_desc with
    | Pmod_ident {txt = lid; loc} ->
      if not loc.loc_ghost then
        emitter |> emitLongident ~pos:(Loc.start loc) ~lid ~debug;
      Ast_iterator.default_iterator.module_expr iterator me
    | _ -> Ast_iterator.default_iterator.module_expr iterator me
  in
  let module_binding (iterator : Ast_iterator.iterator)
      (mb : Parsetree.module_binding) =
    if not mb.pmb_name.loc.loc_ghost then
      emitter
      |> emitLongident
           ~pos:(Loc.start mb.pmb_name.loc)
           ~lid:(Longident.Lident mb.pmb_name.txt) ~debug;
    Ast_iterator.default_iterator.module_binding iterator mb
  in
  let module_declaration (iterator : Ast_iterator.iterator)
      (md : Parsetree.module_declaration) =
    if not md.pmd_name.loc.loc_ghost then
      emitter
      |> emitLongident
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
        |> emitLongident ~upperCaseToken:Token.Type ~pos:(Loc.start loc) ~lid
             ~debug;
      Ast_iterator.default_iterator.module_type iterator mt
    | _ -> Ast_iterator.default_iterator.module_type iterator mt
  in
  let module_type_declaration (iterator : Ast_iterator.iterator)
      (mtd : Parsetree.module_type_declaration) =
    if not mtd.pmtd_name.loc.loc_ghost then
      emitter
      |> emitLongident ~upperCaseToken:Token.Type
           ~pos:(Loc.start mtd.pmtd_name.loc)
           ~lid:(Longident.Lident mtd.pmtd_name.txt) ~debug;
    Ast_iterator.default_iterator.module_type_declaration iterator mtd
  in
  let open_description (iterator : Ast_iterator.iterator)
      (od : Parsetree.open_description) =
    if not od.popen_lid.loc.loc_ghost then
      emitter
      |> emitLongident
           ~pos:(Loc.start od.popen_lid.loc)
           ~lid:od.popen_lid.txt ~debug;
    Ast_iterator.default_iterator.open_description iterator od
  in
  let label_declaration (iterator : Ast_iterator.iterator)
      (ld : Parsetree.label_declaration) =
    emitter
    |> emitRecordLabel
         ~label:{loc = ld.pld_name.loc; txt = Longident.Lident ld.pld_name.txt}
         ~debug;
    Ast_iterator.default_iterator.label_declaration iterator ld
  in
  let constructor_declaration (iterator : Ast_iterator.iterator)
      (cd : Parsetree.constructor_declaration) =
    emitter
    |> emitVariant
         ~name:{loc = cd.pcd_name.loc; txt = Longident.Lident cd.pcd_name.txt}
         ~debug;
    Ast_iterator.default_iterator.constructor_declaration iterator cd
  in

  let structure_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.structure_item) =
    (match item.pstr_desc with
    | Pstr_primitive {pval_name = {txt = id; loc}} ->
      emitter |> emitVariable ~id ~debug ~loc
    | _ -> ());
    Ast_iterator.default_iterator.structure_item iterator item
  in

  let signature_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.signature_item) =
    (match item.psig_desc with
    | Psig_value {pval_name = {txt = id; loc}} ->
      emitter |> emitVariable ~id ~debug ~loc
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

  if Files.classifySourceFile path = Res then (
    let parser =
      Res_driver.parsing_engine.parse_implementation ~for_printer:false
    in
    let {Res_driver.parsetree = structure; diagnostics} =
      parser ~filename:path
    in
    if debug then
      Printf.printf "structure items:%d diagnostics:%d \n"
        (List.length structure) (List.length diagnostics);
    iterator.structure iterator structure |> ignore)
  else
    let parser = Res_driver.parsing_engine.parse_interface ~for_printer:false in
    let {Res_driver.parsetree = signature; diagnostics} =
      parser ~filename:path
    in
    if debug then
      Printf.printf "signature items:%d diagnostics:%d \n"
        (List.length signature) (List.length diagnostics);
    iterator.signature iterator signature |> ignore

let semanticTokens ~currentFile =
  let emitter = Token.createEmitter () in
  command ~emitter ~debug:false ~path:currentFile;
  Printf.printf "{\"data\":[%s]}" (Token.emit emitter)
