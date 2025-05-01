open SharedTypes

let completionWithParser ~currentFile ~debug ~offset ~path ~posCursor text =
  let offsetNoWhite = Utils.skipWhite text (offset - 1) in
  let posNoWhite =
    let line, col = posCursor in
    (line, max 0 col - offset + offsetNoWhite)
  in
  (* Identifies the first character before the cursor that's not white space.
     Should be used very sparingly, but can be used to drive completion triggering
     in scenarios where the parser eats things we'd need to complete.
     Example: let {whatever,     <cursor>}, char is ','. *)
  let firstCharBeforeCursorNoWhite =
    if offsetNoWhite < String.length text && offsetNoWhite >= 0 then
      Some text.[offsetNoWhite]
    else None
  in
  let posOfDot = Pos.posOfDot text ~pos:posCursor ~offset in
  let charAtCursor =
    if offset < String.length text then text.[offset] else '\n'
  in
  let posBeforeCursor = Pos.posBeforeCursor posCursor in
  let _charBeforeCursor, blankAfterCursor =
    match Pos.positionToOffset text posCursor with
    | Some offset when offset > 0 -> (
      let charBeforeCursor = text.[offset - 1] in
      match charAtCursor with
      | ' ' | '\t' | '\r' | '\n' ->
        (Some charBeforeCursor, Some charBeforeCursor)
      | _ -> (Some charBeforeCursor, None))
    | _ -> (None, None)
  in
  let flattenLidCheckDot ?(jsx = true) (lid : Longident.t Location.loc) =
    (* Flatten an identifier keeping track of whether the current cursor
       is after a "." in the id followed by a blank character.
       In that case, cut the path after ".". *)
    let cutAtOffset =
      let idStart = Loc.start lid.loc in
      match blankAfterCursor with
      | Some '.' ->
        if fst posBeforeCursor = fst idStart then
          Some (snd posBeforeCursor - snd idStart)
        else None
      | _ -> None
    in
    Utils.flattenLongIdent ~cutAtOffset ~jsx lid.txt
  in

  let found = ref false in
  let result = ref None in
  let currentTypeLoc = ref None in
  let scope = ref (Scope.create ()) in
  let setResultOpt x =
    if !result = None then
      match x with
      | None ->
        if Debug.verbose () then
          print_endline
            "[set_result] did not set new result because result already was set";
        ()
      | Some x -> result := Some (x, !scope)
  in

  let setResult (x : CompletableRevamped.t) = setResultOpt (Some x) in
  let scopeValueDescription (vd : Parsetree.value_description) =
    scope :=
      !scope |> Scope.addValue ~name:vd.pval_name.txt ~loc:vd.pval_name.loc
  in
  let rec scopePattern (pat : Parsetree.pattern) =
    match pat.ppat_desc with
    | Ppat_any -> ()
    | Ppat_var {txt; loc} -> scope := !scope |> Scope.addValue ~name:txt ~loc
    | Ppat_alias (p, asA) ->
      scopePattern p;
      scope := !scope |> Scope.addValue ~name:asA.txt ~loc:asA.loc
    | Ppat_constant _ | Ppat_interval _ -> ()
    | Ppat_tuple pl -> pl |> List.iter (fun p -> scopePattern p)
    | Ppat_construct (_, None) -> ()
    | Ppat_construct (_, Some {ppat_desc = Ppat_tuple pl}) ->
      pl |> List.iter (fun p -> scopePattern p)
    | Ppat_construct (_, Some p) -> scopePattern p
    | Ppat_variant (_, None) -> ()
    | Ppat_variant (_, Some {ppat_desc = Ppat_tuple pl}) ->
      pl |> List.iter (fun p -> scopePattern p)
    | Ppat_variant (_, Some p) -> scopePattern p
    | Ppat_record (fields, _) ->
      fields |> List.iter (fun (_fname, p, _) -> scopePattern p)
    | Ppat_array pl -> pl |> List.iter scopePattern
    | Ppat_or (p1, _) -> scopePattern p1
    | Ppat_constraint (p, _coreType) -> scopePattern p
    | Ppat_type _ -> ()
    | Ppat_lazy p -> scopePattern p
    | Ppat_unpack {txt; loc} -> scope := !scope |> Scope.addValue ~name:txt ~loc
    | Ppat_exception p -> scopePattern p
    | Ppat_extension _ -> ()
    | Ppat_open (_, p) -> scopePattern p
  in
  let locHasCursor = CursorPosition.locHasCursor ~pos:posBeforeCursor in
  let locIsEmpty = CursorPosition.locIsEmpty ~pos:posBeforeCursor in
  let scopeValueBinding (vb : Parsetree.value_binding) =
    scopePattern vb.pvb_pat
  in
  let scopeTypeKind (tk : Parsetree.type_kind) =
    match tk with
    | Ptype_variant constrDecls ->
      constrDecls
      |> List.iter (fun (cd : Parsetree.constructor_declaration) ->
             scope :=
               !scope
               |> Scope.addConstructor ~name:cd.pcd_name.txt ~loc:cd.pcd_loc)
    | Ptype_record labelDecls ->
      labelDecls
      |> List.iter (fun (ld : Parsetree.label_declaration) ->
             scope :=
               !scope |> Scope.addField ~name:ld.pld_name.txt ~loc:ld.pld_loc)
    | _ -> ()
  in
  let scopeTypeDeclaration (td : Parsetree.type_declaration) =
    scope :=
      !scope |> Scope.addType ~name:td.ptype_name.txt ~loc:td.ptype_name.loc;
    scopeTypeKind td.ptype_kind
  in
  let scopeModuleBinding (mb : Parsetree.module_binding) =
    scope :=
      !scope |> Scope.addModule ~name:mb.pmb_name.txt ~loc:mb.pmb_name.loc
  in
  let scopeModuleDeclaration (md : Parsetree.module_declaration) =
    scope :=
      !scope |> Scope.addModule ~name:md.pmd_name.txt ~loc:md.pmd_name.loc
  in
  let structure (iterator : Ast_iterator.iterator)
      (structure : Parsetree.structure) =
    let oldScope = !scope in
    Ast_iterator.default_iterator.structure iterator structure;
    scope := oldScope
  in
  let structure_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.structure_item) =
    let processed = ref false in
    (match item.pstr_desc with
    | Pstr_open {popen_lid} ->
      scope := !scope |> Scope.addOpen ~lid:popen_lid.txt
    | Pstr_primitive vd -> scopeValueDescription vd
    | Pstr_value (recFlag, bindings) ->
      if recFlag = Recursive then bindings |> List.iter scopeValueBinding;
      bindings |> List.iter (fun vb -> iterator.value_binding iterator vb);
      if recFlag = Nonrecursive then bindings |> List.iter scopeValueBinding;
      processed := true
    | Pstr_type (recFlag, decls) ->
      if recFlag = Recursive then decls |> List.iter scopeTypeDeclaration;
      decls |> List.iter (fun td -> iterator.type_declaration iterator td);
      if recFlag = Nonrecursive then decls |> List.iter scopeTypeDeclaration;
      processed := true
    | Pstr_module mb ->
      iterator.module_binding iterator mb;
      scopeModuleBinding mb;
      processed := true
    | Pstr_recmodule mbs ->
      mbs |> List.iter scopeModuleBinding;
      mbs |> List.iter (fun b -> iterator.module_binding iterator b);
      processed := true
    | _ -> ());
    if not !processed then
      Ast_iterator.default_iterator.structure_item iterator item
  in
  let value_binding (iterator : Ast_iterator.iterator)
      (value_binding : Parsetree.value_binding) =
    (match value_binding with
    | {pvb_pat = {ppat_desc = Ppat_constraint (p, _)}; pvb_expr; pvb_loc}
      when CompletionExpressions.isExprHole pvb_expr
           && locHasCursor pvb_loc
           && not (locHasCursor p.ppat_loc) ->
      (* let x: constraint = <com> *)
      setResult (Cexpression {kind = Empty; typeLoc = p.ppat_loc; posOfDot})
    | {pvb_pat; pvb_expr; pvb_loc}
      when CompletionExpressions.isExprHole pvb_expr
           && locHasCursor pvb_loc
           && not (locHasCursor pvb_pat.ppat_loc) ->
      (* let x= <com> *)
      (* Unclear if this happens and if we need to care about it. *)
      setResult
        (Cexpression {kind = Empty; typeLoc = pvb_pat.ppat_loc; posOfDot})
    | _ -> ());
    Ast_iterator.default_iterator.value_binding iterator value_binding
  in
  let signature (iterator : Ast_iterator.iterator)
      (signature : Parsetree.signature) =
    let oldScope = !scope in
    Ast_iterator.default_iterator.signature iterator signature;
    scope := oldScope
  in
  let signature_item (iterator : Ast_iterator.iterator)
      (item : Parsetree.signature_item) =
    let processed = ref false in
    (match item.psig_desc with
    | Psig_open {popen_lid} ->
      scope := !scope |> Scope.addOpen ~lid:popen_lid.txt
    | Psig_value vd -> scopeValueDescription vd
    | Psig_type (recFlag, decls) ->
      if recFlag = Recursive then decls |> List.iter scopeTypeDeclaration;
      decls |> List.iter (fun td -> iterator.type_declaration iterator td);
      if recFlag = Nonrecursive then decls |> List.iter scopeTypeDeclaration;
      processed := true
    | Psig_module md ->
      iterator.module_declaration iterator md;
      scopeModuleDeclaration md;
      processed := true
    | Psig_recmodule mds ->
      mds |> List.iter scopeModuleDeclaration;
      mds |> List.iter (fun d -> iterator.module_declaration iterator d);
      processed := true
    | _ -> ());
    if not !processed then
      Ast_iterator.default_iterator.signature_item iterator item
  in
  let attribute (iterator : Ast_iterator.iterator)
      ((id, payload) : Parsetree.attribute) =
    (if String.length id.txt >= 4 && String.sub id.txt 0 4 = "res." then
       (* skip: internal parser attribute *) ()
     else if id.loc.loc_ghost then ()
     else if id.loc |> Loc.hasPos ~pos:posBeforeCursor then
       let posStart, posEnd = Loc.range id.loc in
       match
         (Pos.positionToOffset text posStart, Pos.positionToOffset text posEnd)
       with
       | Some offsetStart, Some offsetEnd ->
         (* Can't trust the parser's location
            E.g. @foo. let x... gives as label @foo.let *)
         let label =
           let rawLabel =
             String.sub text offsetStart (offsetEnd - offsetStart)
           in
           let ( ++ ) x y =
             match (x, y) with
             | Some i1, Some i2 -> Some (min i1 i2)
             | Some _, None -> x
             | None, _ -> y
           in
           let label =
             match
               String.index_opt rawLabel ' '
               ++ String.index_opt rawLabel '\t'
               ++ String.index_opt rawLabel '\r'
               ++ String.index_opt rawLabel '\n'
             with
             | None -> rawLabel
             | Some i -> String.sub rawLabel 0 i
           in
           if label <> "" && label.[0] = '@' then
             String.sub label 1 (String.length label - 1)
           else label
         in
         found := true;
         if debug then
           Printf.printf "Attribute id:%s:%s label:%s\n" id.txt
             (Loc.toString id.loc) label;
         setResult (Cdecorator label)
       | _ -> ()
     else if id.txt = "module" then
       match payload with
       | PStr
           [
             {
               pstr_desc =
                 Pstr_eval
                   ( {pexp_loc; pexp_desc = Pexp_constant (Pconst_string (s, _))},
                     _ );
             };
           ]
         when locHasCursor pexp_loc ->
         if Debug.verbose () then
           print_endline "[decoratorCompletion] Found @module";
         setResult (CdecoratorPayload (Module s))
       | PStr
           [
             {
               pstr_desc =
                 Pstr_eval
                   ( {
                       pexp_desc =
                         Pexp_record
                           (({txt = Lident "from"}, fromExpr, _) :: _, _);
                     },
                     _ );
             };
           ]
         when locHasCursor fromExpr.pexp_loc
              || locIsEmpty fromExpr.pexp_loc
                 && CompletionExpressions.isExprHole fromExpr -> (
         if Debug.verbose () then
           print_endline
             "[decoratorCompletion] Found @module with import attributes and \
              cursor on \"from\"";
         match
           ( locHasCursor fromExpr.pexp_loc,
             locIsEmpty fromExpr.pexp_loc,
             CompletionExpressions.isExprHole fromExpr,
             fromExpr )
         with
         | true, _, _, {pexp_desc = Pexp_constant (Pconst_string (s, _))} ->
           if Debug.verbose () then
             print_endline
               "[decoratorCompletion] @module `from` payload was string";
           setResult (CdecoratorPayload (Module s))
         | false, true, true, _ ->
           if Debug.verbose () then
             print_endline
               "[decoratorCompletion] @module `from` payload was expr hole";
           setResult (CdecoratorPayload (Module ""))
         | _ -> ())
       | PStr [{pstr_desc = Pstr_eval (_expr, _)}] ->
         if Debug.verbose () then
           print_endline
             "[decoratorCompletion] Found @module with non-string payload";
         (* TODO(revamp) Complete *)
         ()
       | _ -> ()
     else if id.txt = "jsxConfig" then
       match payload with
       | PStr [{pstr_desc = Pstr_eval (_expr, _)}] ->
         if Debug.verbose () then
           print_endline "[decoratorCompletion] Found @jsxConfig";
         (* TODO(revamp) Complete *)
         ()
       | _ -> ()
     else if id.txt = "editor.completeFrom" then
       match payload with
       | PStr
           [
             {
               pstr_desc =
                 Pstr_eval
                   ( {
                       pexp_loc;
                       pexp_desc = Pexp_construct ({txt = _path; loc = _}, None);
                     },
                     _ );
             };
           ]
         when locHasCursor pexp_loc ->
         if Debug.verbose () then
           print_endline "[decoratorCompletion] Found @editor.completeFrom";
         (* TODO(revamp) Complete for module identifier *)
         (*setResult
           (Completable.Cpath
              (CPId
                 {
                   path = Utils.flattenLongIdent path;
                   completionContext = Module;
                   loc;
                 }))*)
         ()
       | _ -> ());
    Ast_iterator.default_iterator.attribute iterator (id, payload)
  in
  let expr (iterator : Ast_iterator.iterator) (expr : Parsetree.expression) =
    let processed = ref false in
    let setFound () =
      found := true;
      if debug then
        Printf.printf "posCursor:[%s] posNoWhite:[%s] Found expr:%s\n"
          (Pos.toString posCursor) (Pos.toString posNoWhite)
          (Loc.toString expr.pexp_loc)
    in
    match expr.pexp_desc with
    | Pexp_apply
        {
          funct = {pexp_desc = Pexp_ident {txt = Lident "->"; loc = opLoc}};
          args =
            [
              (_, _lhs);
              (_, {pexp_desc = Pexp_extension _; pexp_loc = {loc_ghost = true}});
            ];
        }
      when opLoc |> Loc.hasPos ~pos:posBeforeCursor ->
      (* Case foo-> when the parser adds a ghost expression to the rhs
         so the apply expression does not include the cursor *)
      (* TODO(revamp) Complete pipe *)
      ()
    (*
       A dot completion for a tagged templated application with an expr hole.
       Example:
         sh`echo "meh"`.
    *)
    | Pexp_apply
        {
          funct = {pexp_desc = Pexp_ident {txt = Lident "."; loc = _}};
          args =
            [
              (* sh`echo "meh"` *)
              (_, ({pexp_desc = Pexp_apply _} as innerExpr));
              (* recovery inserted node *)
              (_, {pexp_desc = Pexp_extension ({txt = "rescript.exprhole"}, _)});
            ];
        }
      when Res_parsetree_viewer.is_tagged_template_literal innerExpr ->
      (* TODO(revamp) Complete *)
      ()
    (*
       A dot completion for a tagged templated application with an ident.
       Example:
         sh`echo "meh"`.foo
    *)
    | Pexp_apply
        {
          funct = {pexp_desc = Pexp_ident {txt = Lident "."; loc = _}};
          args =
            [
              (*  sh`echo "meh"` *)
              (_, ({pexp_desc = Pexp_apply _} as innerExpr));
              (* foo *)
              (_, {pexp_desc = Pexp_ident {txt = Lident _fieldName}});
            ];
        }
      when Res_parsetree_viewer.is_tagged_template_literal innerExpr
           && expr.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor ->
      (* TODO(revamp) Complete *)
      ()
    | _ ->
      if expr.pexp_loc |> Loc.hasPos ~pos:posNoWhite && !result = None then (
        setFound ();
        match expr.pexp_desc with
        (* | Pexp_match (switchExpr, [{pc_lhs = lhsPat}])
          when CompletionPatterns.isPatternHole lhsPat
               && locHasCursor switchExpr.pexp_loc = false ->
          setResult (Cpattern {kind = Empty; typeLoc = switchExpr.pexp_loc}) *)
        | Pexp_match (switchExpr, cases) ->
          let oldTypeLoc = !currentTypeLoc in
          currentTypeLoc := Some switchExpr.pexp_loc;
          cases
          |> List.iter (fun case ->
                 Ast_iterator.default_iterator.case iterator case);
          currentTypeLoc := oldTypeLoc;
          processed := true
        | Pexp_extension ({txt = "obj"}, PStr [str_item]) ->
          Ast_iterator.default_iterator.structure_item iterator str_item
        | Pexp_extension ({txt}, _) -> setResult (CextensionNode txt)
        | Pexp_constant _ -> setResult Cnone
        | Pexp_ident lid ->
          let lidPath = flattenLidCheckDot lid in
          if lid.loc |> Loc.hasPos ~pos:posBeforeCursor then
            let _isLikelyModulePath =
              match lidPath with
              | head :: _
                when String.length head > 0
                     && head.[0] == Char.uppercase_ascii head.[0] ->
                true
              | _ -> false
            in
            ()
          (* TODO(revamp) Complete for module identifier *)
          (*setResult
              (Cpath
                 (CPId
                    {
                      loc = lid.loc;
                      path = lidPath;
                      completionContext =
                        (if
                           isLikelyModulePath
                           && expr |> Res_parsetree_viewer.is_braced_expr
                         then ValueOrField
                         else Value);
                    }))*)
        | Pexp_construct ({txt = Lident ("::" | "()")}, _) ->
          (* Ignore list expressions, used in JSX, unit, and more *) ()
        | Pexp_construct (lid, eOpt) -> (
          let lidPath = flattenLidCheckDot lid in
          if debug then
            Printf.printf "Pexp_construct %s:%s %s\n"
              (lidPath |> String.concat "\n")
              (Loc.toString lid.loc)
              (match eOpt with
              | None -> "None"
              | Some e -> Loc.toString e.pexp_loc);
          if
            eOpt = None && (not lid.loc.loc_ghost)
            && lid.loc |> Loc.hasPos ~pos:posBeforeCursor
          then ()
          (* TODO(revamp) Complete *)
          (*
            setResult
              (Cpath
                 (CPId
                    {loc = lid.loc; path = lidPath; completionContext = Value}))*)
            else
            match eOpt with
            | Some e when locHasCursor e.pexp_loc -> (
              match
                CompletionExpressions.completeConstructorPayload
                  ~posBeforeCursor ~firstCharBeforeCursorNoWhite lid e
              with
              | Some _result ->
                (* Check if anything else more important completes before setting this completion. *)
                Ast_iterator.default_iterator.expr iterator e
              (* TODO(revamp) Complete *)
              (*setResult result*)
              | None -> ())
            | _ -> ())
        | Pexp_field (e, fieldName) ->
          if locHasCursor fieldName.loc then
            match fieldName.txt with
            | Lident name ->
              setResult
                (Cexpression
                   {kind = Field {hint = name}; typeLoc = e.pexp_loc; posOfDot})
            | Ldot (_id, _name) ->
              (* Case x.M.field ignore the x part *)
              (*let contextPath =
                Completable.CPField
                  {
                    contextPath =
                      CPId
                        {
                          loc = fieldName.loc;
                          path = Utils.flattenLongIdent id;
                          completionContext = Module;
                        };
                    fieldName =
                      (if blankAfterCursor = Some '.' then
                         (* x.M. field  --->  M. *) ""
                       else if name = "_" then ""
                       else name);
                    posOfDot;
                    exprLoc = e.pexp_loc;
                    inJsx = !inJsxContext;
                  }
              in*)
              (* TODO(revamp) Complete ID *)
              ()
            | Lapply _ -> ()
          else if Loc.end_ e.pexp_loc = posBeforeCursor then
            setResult
              (Cexpression
                 {kind = Field {hint = ""}; typeLoc = e.pexp_loc; posOfDot})
        (* TODO(revamp) Insert back JSX stuff *)
        | Pexp_apply
            {
              funct = {pexp_desc = Pexp_ident {txt = Lident "->"}};
              args =
                [
                  (_, _lhs);
                  (_, {pexp_desc = Pexp_ident {txt = Longident.Lident _id; loc}});
                ];
            }
          when loc |> Loc.hasPos ~pos:posBeforeCursor ->
          if Debug.verbose () then print_endline "[expr_iter] Case foo->id";
          (* TODO(revamp) Complete pipe (id) *)
          ()
        | Pexp_apply
            {
              funct = {pexp_desc = Pexp_ident {txt = Lident "->"; loc = opLoc}};
              args = [(_, _lhs); _];
            }
          when Loc.end_ opLoc = posCursor ->
          if Debug.verbose () then print_endline "[expr_iter] Case foo->";
          (* TODO(revamp) Complete pipe (empty) *)
          ()
        | Pexp_send (lhs, {txt; loc}) ->
          (* e["txt"]
             If the string for txt is not closed, it could go over several lines.
             Only take the first like to represent the label *)
          let txtLines = txt |> String.split_on_char '\n' in
          let label = List.hd txtLines in
          let label =
            if label <> "" && label.[String.length label - 1] = '\r' then
              String.sub label 0 (String.length label - 1)
            else label
          in
          let labelRange =
            let l, c = Loc.start loc in
            ((l, c + 1), (l, c + 1 + String.length label))
          in
          if debug then
            Printf.printf "Pexp_send %s%s e:%s\n" label
              (Range.toString labelRange)
              (Loc.toString lhs.pexp_loc);
          if
            labelRange |> Range.hasPos ~pos:posBeforeCursor
            || (label = "" && posCursor = fst labelRange)
          then
            (* TODO(revamp) Complete obj *)
            ()
        | Pexp_fun
            {arg_label = _lbl; default = _defaultExpOpt; lhs = pat; rhs = e} ->
          let oldScope = !scope in
          scopePattern pat;
          iterator.pat iterator pat;
          iterator.expr iterator e;
          scope := oldScope;
          processed := true
        | Pexp_let (recFlag, bindings, e) ->
          let oldScope = !scope in
          if recFlag = Recursive then bindings |> List.iter scopeValueBinding;
          bindings |> List.iter (fun vb -> iterator.value_binding iterator vb);
          if recFlag = Nonrecursive then bindings |> List.iter scopeValueBinding;
          iterator.expr iterator e;
          scope := oldScope;
          processed := true
        | Pexp_letmodule (name, modExpr, modBody) ->
          let oldScope = !scope in
          iterator.location iterator name.loc;
          iterator.module_expr iterator modExpr;
          scope := !scope |> Scope.addModule ~name:name.txt ~loc:name.loc;
          iterator.expr iterator modBody;
          scope := oldScope;
          processed := true
        | Pexp_open (_, lid, e) ->
          let oldScope = !scope in
          iterator.location iterator lid.loc;
          scope := !scope |> Scope.addOpen ~lid:lid.txt;
          iterator.expr iterator e;
          scope := oldScope;
          processed := true
        | _ -> ());
      if not !processed then Ast_iterator.default_iterator.expr iterator expr
  in
  let typ (iterator : Ast_iterator.iterator) (core_type : Parsetree.core_type) =
    if core_type.ptyp_loc |> Loc.hasPos ~pos:posNoWhite then (
      found := true;
      if debug then
        Printf.printf "posCursor:[%s] posNoWhite:[%s] Found type:%s\n"
          (Pos.toString posCursor) (Pos.toString posNoWhite)
          (Loc.toString core_type.ptyp_loc);
      match core_type.ptyp_desc with
      | Ptyp_constr (lid, _args) ->
        let lidPath = flattenLidCheckDot lid in
        if debug then
          Printf.printf "Ptyp_constr %s:%s\n"
            (lidPath |> String.concat ".")
            (Loc.toString lid.loc);
        if lid.loc |> Loc.hasPos ~pos:posBeforeCursor then
          (* TODO(revamp) Complete type *)
          ()
      | _ -> ());
    Ast_iterator.default_iterator.typ iterator core_type
  in
  let pat (iterator : Ast_iterator.iterator) (pat : Parsetree.pattern) =
    if pat.ppat_loc |> Loc.hasPos ~pos:posNoWhite then (
      found := true;
      if debug then
        Printf.printf "posCursor:[%s] posNoWhite:[%s] Found pattern:%s\n"
          (Pos.toString posCursor) (Pos.toString posNoWhite)
          (Loc.toString pat.ppat_loc);
      (match pat.ppat_desc with
      | Ppat_record ([], _) ->
        (* No fields means empty record body.*)
        setResult
          (Cpattern
             {kind = Field {hint = ""; seenFields = []}; typeLoc = pat.ppat_loc})
      | Ppat_record (fields, _) -> (
        let fieldWithCursor = ref None in
        let fieldWithPatHole = ref None in
        fields
        |> List.iter (fun (fname, f, _) ->
               match
                 ( fname.Location.txt,
                   f.Parsetree.ppat_loc
                   |> CursorPosition.classifyLoc ~pos:posBeforeCursor )
               with
               | Longident.Lident fname, HasCursor ->
                 fieldWithCursor := Some (fname, f)
               | Lident fname, _ when CompletionPatterns.isPatternHole f ->
                 fieldWithPatHole := Some (fname, f)
               | _ -> ());
        let seenFields =
          fields
          |> List.filter_map (fun (fieldName, _f, _) ->
                 match fieldName with
                 | {Location.txt = Longident.Lident fieldName} -> Some fieldName
                 | _ -> None)
        in
        match (!fieldWithCursor, !fieldWithPatHole) with
        | Some (fname, f), _ | None, Some (fname, f) -> (
          match f.ppat_desc with
          | Ppat_extension ({txt = "rescript.patternhole"}, _) ->
            (* A pattern hole means for example `{someField: <com>}`. We want to complete for the type of `someField`.  *)
            setResult
              (Cpattern
                 {kind = FieldValue {fieldName = fname}; typeLoc = pat.ppat_loc})
          | Ppat_var {txt} ->
            (* A var means `{s}` or similar. Complete for fields. *)
            setResult
              (Cpattern
                 {kind = Field {hint = txt; seenFields}; typeLoc = pat.ppat_loc})
          | _ -> ())
        | None, None -> (
          (* Figure out if we're completing for a new field.
         If the cursor is inside of the record body, but no field has the cursor,
         and there's no pattern hole. Check the first char to the left of the cursor,
         ignoring white space. If that's a comma, we assume you're completing for a new field. *)
          match firstCharBeforeCursorNoWhite with
          | Some ',' ->
            setResult
              (Cpattern
                 {kind = Field {hint = ""; seenFields}; typeLoc = pat.ppat_loc})
          | _ -> ()))
      | Ppat_construct (lid, _) ->
        let lidPath = flattenLidCheckDot lid in
        if debug then
          Printf.printf "Ppat_construct %s:%s\n"
            (lidPath |> String.concat ".")
            (Loc.toString lid.loc);
        let _completion =
          Completable.Cpath
            (CPId {loc = lid.loc; path = lidPath; completionContext = Value})
        in
        (* TODO(revamp) Complete *)
        ()
      | _ ->
        if CompletionPatterns.isPatternHole pat then
          setResult (Cpattern {kind = Empty; typeLoc = pat.ppat_loc}));
      Ast_iterator.default_iterator.pat iterator pat)
  in
  let module_expr (iterator : Ast_iterator.iterator)
      (me : Parsetree.module_expr) =
    (match me.pmod_desc with
    | Pmod_ident lid when lid.loc |> Loc.hasPos ~pos:posBeforeCursor ->
      let lidPath = flattenLidCheckDot lid in
      if debug then
        Printf.printf "Pmod_ident %s:%s\n"
          (lidPath |> String.concat ".")
          (Loc.toString lid.loc);
      found := true
      (* TODO(revamp) Complete module ID *)
      (*
      setResult
        (Cpath
           (CPId {loc = lid.loc; path = lidPath; completionContext = Module}))*)
    | _ -> ());
    Ast_iterator.default_iterator.module_expr iterator me
  in
  let module_type (iterator : Ast_iterator.iterator)
      (mt : Parsetree.module_type) =
    (match mt.pmty_desc with
    | Pmty_ident lid when lid.loc |> Loc.hasPos ~pos:posBeforeCursor ->
      let lidPath = flattenLidCheckDot lid in
      if debug then
        Printf.printf "Pmty_ident %s:%s\n"
          (lidPath |> String.concat ".")
          (Loc.toString lid.loc);
      found := true
      (* TODO(revamp) Complete module ID *)
      (*
      setResult
        (Cpath
           (CPId {loc = lid.loc; path = lidPath; completionContext = Module}))*)
    | _ -> ());
    Ast_iterator.default_iterator.module_type iterator mt
  in
  let type_kind (iterator : Ast_iterator.iterator)
      (type_kind : Parsetree.type_kind) =
    (match type_kind with
    | Ptype_variant [decl]
      when decl.pcd_name.loc |> Loc.hasPos ~pos:posNoWhite
           && decl.pcd_args = Pcstr_tuple [] ->
      (* "type t = Pre" could signal the intent to complete variant "Prelude",
         or the beginning of "Prefix.t" *)
      if debug then
        Printf.printf "Ptype_variant unary %s:%s\n" decl.pcd_name.txt
          (Loc.toString decl.pcd_name.loc);
      found := true
      (* TODO(revamp) Complete *)
      (*setResult
        (Cpath
           (CPId
              {
                loc = decl.pcd_name.loc;
                path = [decl.pcd_name.txt];
                completionContext = Value;
              }))*)
    | _ -> ());
    Ast_iterator.default_iterator.type_kind iterator type_kind
  in

  let lastScopeBeforeCursor = ref (Scope.create ()) in
  let location (_iterator : Ast_iterator.iterator) (loc : Location.t) =
    if Loc.end_ loc <= posCursor then lastScopeBeforeCursor := !scope
  in

  let iterator =
    {
      Ast_iterator.default_iterator with
      attribute;
      expr;
      location;
      module_expr;
      module_type;
      pat;
      signature;
      signature_item;
      structure;
      structure_item;
      typ;
      type_kind;
      value_binding;
    }
  in

  if Filename.check_suffix path ".res" then (
    let parser =
      Res_driver.parsing_engine.parse_implementation ~for_printer:false
    in
    let {Res_driver.parsetree = str} = parser ~filename:currentFile in
    let tree = Res_recovery.map str in
    iterator.structure iterator tree |> ignore;
    if blankAfterCursor = Some ' ' || blankAfterCursor = Some '\n' then
      scope := !lastScopeBeforeCursor
      (* TODO(revamp) Complete any value *)
      (*setResult
        (Cpath
           (CPId {loc = Location.none; path = [""]; completionContext = Value}))*);
    if !found = false then if debug then Printf.printf "XXX Not found!\n";
    !result)
  else if Filename.check_suffix path ".resi" then (
    let parser = Res_driver.parsing_engine.parse_interface ~for_printer:false in
    let {Res_driver.parsetree = signature} = parser ~filename:currentFile in
    iterator.signature iterator signature |> ignore;
    if blankAfterCursor = Some ' ' || blankAfterCursor = Some '\n' then
      scope := !lastScopeBeforeCursor
      (* TODO(revamp) Complete any type *)
      (*setResult
      setResult
        (Cpath
           (CPId {loc = Location.none; path = [""]; completionContext = Type}))*);
    if !found = false then if debug then Printf.printf "XXX Not found!\n";
    !result)
  else None

let completionWithParser ~debug ~path ~posCursor ~currentFile ~text =
  match Pos.positionToOffset text posCursor with
  | Some offset ->
    completionWithParser ~currentFile ~debug ~offset ~path ~posCursor text
  | None -> None
