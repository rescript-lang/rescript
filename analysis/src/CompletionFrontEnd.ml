open SharedTypes

let findArgCompletables ~(args : arg list) ~endPos ~posBeforeCursor
    ~(contextPath : Completable.contextPath) ~posAfterFunExpr
    ~firstCharBeforeCursorNoWhite ~charBeforeCursor ~isPipedExpr =
  let fnHasCursor =
    posAfterFunExpr <= posBeforeCursor && posBeforeCursor < endPos
  in
  let allNames =
    List.fold_right
      (fun arg allLabels ->
        match arg with
        | {label = Some labelled} -> labelled.name :: allLabels
        | {label = None} -> allLabels)
      args []
  in
  let unlabelledCount = ref (if isPipedExpr then 1 else 0) in
  let someArgHadEmptyExprLoc = ref false in
  let rec loop args =
    match args with
    | {label = Some labelled; exp} :: rest ->
      if
        labelled.posStart <= posBeforeCursor
        && posBeforeCursor < labelled.posEnd
      then (
        if Debug.verbose () then
          print_endline "[findArgCompletables] Completing named arg #2";
        Some (Completable.CnamedArg (contextPath, labelled.name, allNames)))
      else if exp.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor then (
        if Debug.verbose () then
          print_endline
            "[findArgCompletables] Completing in the assignment of labelled \
             argument";
        match
          CompletionExpressions.traverseExpr exp ~exprPath:[]
            ~pos:posBeforeCursor ~firstCharBeforeCursorNoWhite
        with
        | None -> None
        | Some (prefix, nested) ->
          if Debug.verbose () then
            print_endline
              "[findArgCompletables] Completing for labelled argument value";
          Some
            (Cexpression
               {
                 contextPath =
                   CArgument
                     {
                       functionContextPath = contextPath;
                       argumentLabel = Labelled labelled.name;
                     };
                 prefix;
                 nested = List.rev nested;
               }))
      else if CompletionExpressions.isExprHole exp then (
        if Debug.verbose () then
          print_endline "[findArgCompletables] found exprhole";
        Some
          (Cexpression
             {
               contextPath =
                 CArgument
                   {
                     functionContextPath = contextPath;
                     argumentLabel = Labelled labelled.name;
                   };
               prefix = "";
               nested = [];
             }))
      else loop rest
    | {label = None; exp} :: rest ->
      if Debug.verbose () then
        Printf.printf "[findArgCompletable] unlabelled arg expr is: %s \n"
          (DumpAst.printExprItem ~pos:posBeforeCursor ~indentation:0 exp);

      (* Track whether there was an arg with an empty loc (indicates parser error)*)
      if CursorPosition.locIsEmpty exp.pexp_loc ~pos:posBeforeCursor then
        someArgHadEmptyExprLoc := true;

      if Res_parsetree_viewer.is_template_literal exp then None
      else if exp.pexp_loc |> Loc.hasPos ~pos:posBeforeCursor then (
        if Debug.verbose () then
          print_endline
            "[findArgCompletables] Completing in an unlabelled argument";
        match
          CompletionExpressions.traverseExpr exp ~pos:posBeforeCursor
            ~firstCharBeforeCursorNoWhite ~exprPath:[]
        with
        | None ->
          if Debug.verbose () then
            print_endline
              "[findArgCompletables] found nothing when traversing expr";
          None
        | Some (prefix, nested) ->
          if Debug.verbose () then
            print_endline
              "[findArgCompletables] completing for unlabelled argument #2";
          Some
            (Cexpression
               {
                 contextPath =
                   CArgument
                     {
                       functionContextPath = contextPath;
                       argumentLabel =
                         Unlabelled {argumentPosition = !unlabelledCount};
                     };
                 prefix;
                 nested = List.rev nested;
               }))
      else if CompletionExpressions.isExprHole exp then (
        if Debug.verbose () then
          print_endline "[findArgCompletables] found an exprhole #2";
        Some
          (Cexpression
             {
               contextPath =
                 CArgument
                   {
                     functionContextPath = contextPath;
                     argumentLabel =
                       Unlabelled {argumentPosition = !unlabelledCount};
                   };
               prefix = "";
               nested = [];
             }))
      else (
        unlabelledCount := !unlabelledCount + 1;
        loop rest)
    | [] ->
      let hadEmptyExpLoc = !someArgHadEmptyExprLoc in
      if fnHasCursor then (
        if Debug.verbose () then
          print_endline "[findArgCompletables] Function has cursor";
        match charBeforeCursor with
        | Some '~' ->
          if Debug.verbose () then
            print_endline "[findArgCompletables] '~' is before cursor";
          Some (Completable.CnamedArg (contextPath, "", allNames))
        | _ when hadEmptyExpLoc ->
          (* Special case: `Console.log(arr->)`, completing on the pipe.
             This match branch happens when the fn call has the cursor and:
             - there's no argument label or expr that has the cursor
             - there's an argument expression with an empty loc (indicates parser error)

             In that case, it's safer to not complete for the unlabelled function
             argument (which we do otherwise), and instead not complete and let the
             completion engine move into the arguments one by one instead to check
             for completions.

             This can be handled in a more robust way in a future refactor of the
             completion engine logic. *)
          if Debug.verbose () then
            print_endline
              "[findArgCompletables] skipping completion in fn call because \
               arg had empty loc";
          None
        | _
          when firstCharBeforeCursorNoWhite = Some '('
               || firstCharBeforeCursorNoWhite = Some ',' ->
          (* Checks to ensure that completing for empty unlabelled arg makes
             sense by checking what's left of the cursor. *)
          if Debug.verbose () then
            Printf.printf
              "[findArgCompletables] Completing for unlabelled argument value \
               because nothing matched and is not labelled argument name \
               completion. isPipedExpr: %b\n"
              isPipedExpr;
          Some
            (Cexpression
               {
                 contextPath =
                   CArgument
                     {
                       functionContextPath = contextPath;
                       argumentLabel =
                         Unlabelled {argumentPosition = !unlabelledCount};
                     };
                 prefix = "";
                 nested = [];
               })
        | _ -> None)
      else None
  in
  match args with
  (* Special handling for empty fn calls, e.g. `let _ = someFn(<com>)` *)
  | [
   {label = None; exp = {pexp_desc = Pexp_construct ({txt = Lident "()"}, _)}};
  ]
    when fnHasCursor ->
    if Debug.verbose () then
      print_endline "[findArgCompletables] Completing for unit argument";
    Some
      (Completable.Cexpression
         {
           contextPath =
             CArgument
               {
                 functionContextPath = contextPath;
                 argumentLabel =
                   Unlabelled
                     {argumentPosition = (if isPipedExpr then 1 else 0)};
               };
           prefix = "";
           nested = [];
         })
  | _ -> loop args

let rec exprToContextPathInner (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_constant (Pconst_string _) -> Some Completable.CPString
  | Pexp_constant (Pconst_integer _) -> Some CPInt
  | Pexp_constant (Pconst_float _) -> Some CPFloat
  | Pexp_construct ({txt = Lident ("true" | "false")}, None) -> Some CPBool
  | Pexp_array exprs ->
    Some
      (CPArray
         (match exprs with
         | [] -> None
         | exp :: _ -> exprToContextPath exp))
  | Pexp_ident {txt = Lident ("|." | "|.u")} -> None
  | Pexp_ident {txt; loc} ->
    Some
      (CPId {path = Utils.flattenLongIdent txt; completionContext = Value; loc})
  | Pexp_field (e1, {txt = Lident name}) -> (
    match exprToContextPath e1 with
    | Some contextPath -> Some (CPField (contextPath, name))
    | _ -> None)
  | Pexp_field (_, {loc; txt = Ldot (lid, name)}) ->
    (* Case x.M.field ignore the x part *)
    Some
      (CPField
         ( CPId
             {
               path = Utils.flattenLongIdent lid;
               completionContext = Module;
               loc;
             },
           name ))
  | Pexp_send (e1, {txt}) -> (
    match exprToContextPath e1 with
    | None -> None
    | Some contexPath -> Some (CPObj (contexPath, txt)))
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Lident ("|." | "|.u")}},
        [
          (_, lhs);
          (_, {pexp_desc = Pexp_apply (d, args); pexp_loc; pexp_attributes});
        ] ) ->
    (* Transform away pipe with apply call *)
    exprToContextPath
      {
        pexp_desc = Pexp_apply (d, (Nolabel, lhs) :: args);
        pexp_loc;
        pexp_attributes;
      }
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Lident ("|." | "|.u")}},
        [(_, lhs); (_, {pexp_desc = Pexp_ident id; pexp_loc; pexp_attributes})]
      ) ->
    (* Transform away pipe with identifier *)
    exprToContextPath
      {
        pexp_desc =
          Pexp_apply
            ( {pexp_desc = Pexp_ident id; pexp_loc; pexp_attributes},
              [(Nolabel, lhs)] );
        pexp_loc;
        pexp_attributes;
      }
  | Pexp_apply (e1, args) -> (
    match exprToContextPath e1 with
    | None -> None
    | Some contexPath -> Some (CPApply (contexPath, args |> List.map fst)))
  | Pexp_tuple exprs ->
    let exprsAsContextPaths = exprs |> List.filter_map exprToContextPath in
    if List.length exprs = List.length exprsAsContextPaths then
      Some (CTuple exprsAsContextPaths)
    else None
  | _ -> None

and exprToContextPath (e : Parsetree.expression) =
  match
    ( Res_parsetree_viewer.has_await_attribute e.pexp_attributes,
      exprToContextPathInner e )
  with
  | true, Some ctxPath -> Some (CPAwait ctxPath)
  | false, Some ctxPath -> Some ctxPath
  | _, None -> None

let completePipeChain (exp : Parsetree.expression) =
  (* Complete the end of pipe chains by reconstructing the pipe chain as a single pipe,
     so it can be completed.
     Example:
      someArray->Js.Array2.filter(v => v > 10)->Js.Array2.map(v => v + 2)->
        will complete as:
      Js.Array2.map(someArray->Js.Array2.filter(v => v > 10), v => v + 2)->
  *)
  match exp.pexp_desc with
  (* When the left side of the pipe we're completing is a function application.
     Example: someArray->Js.Array2.map(v => v + 2)-> *)
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Lident ("|." | "|.u")}},
        [_; (_, {pexp_desc = Pexp_apply (d, _)})] ) ->
    exprToContextPath exp |> Option.map (fun ctxPath -> (ctxPath, d.pexp_loc))
    (* When the left side of the pipe we're completing is an identifier application.
       Example: someArray->filterAllTheGoodStuff-> *)
  | Pexp_apply
      ( {pexp_desc = Pexp_ident {txt = Lident ("|." | "|.u")}},
        [_; (_, {pexp_desc = Pexp_ident _; pexp_loc})] ) ->
    exprToContextPath exp |> Option.map (fun ctxPath -> (ctxPath, pexp_loc))
  | _ -> None

let completionWithParser1 ~currentFile ~debug ~offset ~path ~posCursor
    ?findThisExprLoc text =
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
  let charAtCursor =
    if offset < String.length text then text.[offset] else '\n'
  in
  let posBeforeCursor = Pos.posBeforeCursor posCursor in
  let charBeforeCursor, blankAfterCursor =
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

  let currentCtxPath = ref None in
  let processingFun = ref None in
  let setCurrentCtxPath ctxPath =
    if !Cfg.debugFollowCtxPath then
      Printf.printf "setting current ctxPath: %s\n"
        (Completable.contextPathToString ctxPath);
    currentCtxPath := Some ctxPath
  in
  let resetCurrentCtxPath ctxPath =
    (match (!currentCtxPath, ctxPath) with
    | None, None -> ()
    | _ ->
      if !Cfg.debugFollowCtxPath then
        Printf.printf "resetting current ctxPath to: %s\n"
          (match ctxPath with
          | None -> "None"
          | Some ctxPath -> Completable.contextPathToString ctxPath));
    currentCtxPath := ctxPath
  in

  let found = ref false in
  let result = ref None in
  let scope = ref (Scope.create ()) in
  let setResultOpt x =
    if !result = None then
      match x with
      | None ->
        if Debug.verbose () then
          print_endline
            "[set_result] did not set new result because result already was set";
        ()
      | Some x ->
        if Debug.verbose () then
          Printf.printf "[set_result] set new result to %s\n"
            (Completable.toString x);
        result := Some (x, !scope)
  in
  let setResult x = setResultOpt (Some x) in
  let scopeValueDescription (vd : Parsetree.value_description) =
    scope :=
      !scope |> Scope.addValue ~name:vd.pval_name.txt ~loc:vd.pval_name.loc
  in
  let rec scopePattern ?contextPath
      ?(patternPath : Completable.nestedPath list = [])
      (pat : Parsetree.pattern) =
    let contextPathToSave =
      match (contextPath, patternPath) with
      | maybeContextPath, [] -> maybeContextPath
      | Some contextPath, patternPath ->
        Some
          (Completable.CPatternPath
             {rootCtxPath = contextPath; nested = List.rev patternPath})
      | _ -> None
    in
    match pat.ppat_desc with
    | Ppat_any -> ()
    | Ppat_var {txt; loc} ->
      scope :=
        !scope |> Scope.addValue ~name:txt ~loc ?contextPath:contextPathToSave
    | Ppat_alias (p, asA) ->
      scopePattern p ~patternPath ?contextPath;
      let ctxPath =
        if contextPathToSave = None then
          match p with
          | {ppat_desc = Ppat_var {txt; loc}} ->
            Some
              (Completable.CPId {path = [txt]; completionContext = Value; loc})
          | _ -> None
        else None
      in
      scope :=
        !scope |> Scope.addValue ~name:asA.txt ~loc:asA.loc ?contextPath:ctxPath
    | Ppat_constant _ | Ppat_interval _ -> ()
    | Ppat_tuple pl ->
      pl
      |> List.iteri (fun index p ->
             scopePattern p
               ~patternPath:(NTupleItem {itemNum = index} :: patternPath)
               ?contextPath)
    | Ppat_construct (_, None) -> ()
    | Ppat_construct ({txt}, Some {ppat_desc = Ppat_tuple pl}) ->
      pl
      |> List.iteri (fun index p ->
             scopePattern p
               ~patternPath:
                 (NVariantPayload
                    {
                      itemNum = index;
                      constructorName = Utils.getUnqualifiedName txt;
                    }
                 :: patternPath)
               ?contextPath)
    | Ppat_construct ({txt}, Some p) ->
      scopePattern
        ~patternPath:
          (NVariantPayload
             {itemNum = 0; constructorName = Utils.getUnqualifiedName txt}
          :: patternPath)
        ?contextPath p
    | Ppat_variant (_, None) -> ()
    | Ppat_variant (txt, Some {ppat_desc = Ppat_tuple pl}) ->
      pl
      |> List.iteri (fun index p ->
             scopePattern p
               ~patternPath:
                 (NPolyvariantPayload {itemNum = index; constructorName = txt}
                 :: patternPath)
               ?contextPath)
    | Ppat_variant (txt, Some p) ->
      scopePattern
        ~patternPath:
          (NPolyvariantPayload {itemNum = 0; constructorName = txt}
          :: patternPath)
        ?contextPath p
    | Ppat_record (fields, _) ->
      fields
      |> List.iter (fun (fname, p, _) ->
             match fname with
             | {Location.txt = Longident.Lident fname} ->
               scopePattern
                 ~patternPath:
                   (Completable.NFollowRecordField {fieldName = fname}
                   :: patternPath)
                 ?contextPath p
             | _ -> ())
    | Ppat_array pl ->
      pl
      |> List.iter
           (scopePattern ~patternPath:(NArray :: patternPath) ?contextPath)
    | Ppat_or (p1, _) -> scopePattern ~patternPath ?contextPath p1
    | Ppat_constraint (p, coreType) ->
      scopePattern ~patternPath
        ?contextPath:(TypeUtils.contextPathFromCoreType coreType)
        p
    | Ppat_type _ -> ()
    | Ppat_lazy p -> scopePattern ~patternPath ?contextPath p
    | Ppat_unpack {txt; loc} ->
      scope :=
        !scope |> Scope.addValue ~name:txt ~loc ?contextPath:contextPathToSave
    | Ppat_exception p -> scopePattern ~patternPath ?contextPath p
    | Ppat_extension _ -> ()
    | Ppat_open (_, p) -> scopePattern ~patternPath ?contextPath p
  in
  let locHasCursor = CursorPosition.locHasCursor ~pos:posBeforeCursor in
  let locIsEmpty = CursorPosition.locIsEmpty ~pos:posBeforeCursor in
  let completePattern ?contextPath (pat : Parsetree.pattern) =
    match
      ( pat
        |> CompletionPatterns.traversePattern ~patternPath:[] ~locHasCursor
             ~firstCharBeforeCursorNoWhite ~posBeforeCursor,
        contextPath )
    with
    | Some (prefix, nestedPattern), Some ctxPath ->
      if Debug.verbose () then
        Printf.printf "[completePattern] found pattern that can be completed\n";
      setResult
        (Completable.Cpattern
           {
             contextPath = ctxPath;
             prefix;
             nested = List.rev nestedPattern;
             fallback = None;
             patternMode = Default;
           })
    | _ -> ()
  in
  let scopeValueBinding (vb : Parsetree.value_binding) =
    let contextPath =
      (* Pipe chains get special treatment here, because when assigning values
         we want the return of the entire pipe chain as a function call, rather
         than as a pipe completion call. *)
      match completePipeChain vb.pvb_expr with
      | Some (ctxPath, _) -> Some ctxPath
      | None -> exprToContextPath vb.pvb_expr
    in
    scopePattern ?contextPath vb.pvb_pat
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
  let inJsxContext = ref false in
  (* Identifies expressions where we can do typed pattern or expr completion. *)
  let typedCompletionExpr (exp : Parsetree.expression) =
    let debugTypedCompletionExpr = false in
    if exp.pexp_loc |> CursorPosition.locHasCursor ~pos:posBeforeCursor then (
      if Debug.verbose () && debugTypedCompletionExpr then
        print_endline "[typedCompletionExpr] Has cursor";
      match exp.pexp_desc with
      (* No cases means there's no `|` yet in the switch *)
      | Pexp_match (({pexp_desc = Pexp_ident _} as expr), []) ->
        if Debug.verbose () && debugTypedCompletionExpr then
          print_endline "[typedCompletionExpr] No cases, with ident";
        if locHasCursor expr.pexp_loc then (
          if Debug.verbose () && debugTypedCompletionExpr then
            print_endline "[typedCompletionExpr] No cases - has cursor";
          (* We can do exhaustive switch completion if this is an ident we can
             complete from. *)
          match exprToContextPath expr with
          | None -> ()
          | Some contextPath ->
            setResult (CexhaustiveSwitch {contextPath; exprLoc = exp.pexp_loc}))
      | Pexp_match (_expr, []) ->
        (* switch x { } *)
        if Debug.verbose () && debugTypedCompletionExpr then
          print_endline "[typedCompletionExpr] No cases, rest";
        ()
      | Pexp_match (expr, [{pc_lhs; pc_rhs}])
        when locHasCursor expr.pexp_loc
             && CompletionExpressions.isExprHole pc_rhs
             && CompletionPatterns.isPatternHole pc_lhs ->
        (* switch x { | } when we're in the switch expr itself. *)
        if Debug.verbose () && debugTypedCompletionExpr then
          print_endline
            "[typedCompletionExpr] No cases (expr and pat holes), rest";
        ()
      | Pexp_match
          ( exp,
            [
              {
                pc_lhs =
                  {
                    ppat_desc =
                      Ppat_extension ({txt = "rescript.patternhole"}, _);
                  };
              };
            ] ) -> (
        (* A single case that's a pattern hole typically means `switch x { | }`. Complete as the pattern itself with nothing nested. *)
        match exprToContextPath exp with
        | None -> ()
        | Some ctxPath ->
          setResult
            (Completable.Cpattern
               {
                 contextPath = ctxPath;
                 nested = [];
                 prefix = "";
                 fallback = None;
                 patternMode = Default;
               }))
      | Pexp_match (exp, cases) -> (
        if Debug.verbose () && debugTypedCompletionExpr then
          print_endline "[typedCompletionExpr] Has cases";
        (* If there's more than one case, or the case isn't a pattern hole, figure out if we're completing another
           broken parser case (`switch x { | true => () | <com> }` for example). *)
        match exp |> exprToContextPath with
        | None ->
          if Debug.verbose () && debugTypedCompletionExpr then
            print_endline "[typedCompletionExpr] Has cases - no ctx path"
        | Some ctxPath -> (
          if Debug.verbose () && debugTypedCompletionExpr then
            print_endline "[typedCompletionExpr] Has cases - has ctx path";
          let hasCaseWithCursor =
            cases
            |> List.find_opt (fun case ->
                   locHasCursor case.Parsetree.pc_lhs.ppat_loc)
            |> Option.is_some
          in
          let hasCaseWithEmptyLoc =
            cases
            |> List.find_opt (fun case ->
                   locIsEmpty case.Parsetree.pc_lhs.ppat_loc)
            |> Option.is_some
          in
          if Debug.verbose () && debugTypedCompletionExpr then
            Printf.printf
              "[typedCompletionExpr] Has cases - has ctx path - \
               hasCaseWithEmptyLoc: %b, hasCaseWithCursor: %b\n"
              hasCaseWithEmptyLoc hasCaseWithCursor;
          match (hasCaseWithEmptyLoc, hasCaseWithCursor) with
          | _, true ->
            (* Always continue if there's a case with the cursor *)
            ()
          | true, false ->
            (* If there's no case with the cursor, but a broken parser case, complete for the top level. *)
            setResult
              (Completable.Cpattern
                 {
                   contextPath = ctxPath;
                   nested = [];
                   prefix = "";
                   fallback = None;
                   patternMode = Default;
                 })
          | false, false -> ()))
      | _ -> ())
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
    let oldInJsxContext = !inJsxContext in
    if Utils.isJsxComponent value_binding then inJsxContext := true;
    (match value_binding with
    | {pvb_pat = {ppat_desc = Ppat_constraint (_pat, coreType)}; pvb_expr}
      when locHasCursor pvb_expr.pexp_loc -> (
      (* Expression with derivable type annotation.
         E.g: let x: someRecord = {<com>} *)
      match
        ( TypeUtils.contextPathFromCoreType coreType,
          pvb_expr
          |> CompletionExpressions.traverseExpr ~exprPath:[]
               ~pos:posBeforeCursor ~firstCharBeforeCursorNoWhite )
      with
      | Some ctxPath, Some (prefix, nested) ->
        setResult
          (Completable.Cexpression
             {contextPath = ctxPath; prefix; nested = List.rev nested})
      | _ -> ())
    | {pvb_pat = {ppat_desc = Ppat_var {loc}}; pvb_expr}
      when locHasCursor pvb_expr.pexp_loc -> (
      (* Expression without a type annotation. We can complete this if this
         has compiled previously and there's a type available for the identifier itself.
         This is nice because the type is assigned even if the assignment isn't complete.

         E.g: let x = {name: "name", <com>}, when `x` has compiled. *)
      match
        pvb_expr
        |> CompletionExpressions.traverseExpr ~exprPath:[] ~pos:posBeforeCursor
             ~firstCharBeforeCursorNoWhite
      with
      | Some (prefix, nested) ->
        (* This completion should be low prio, so let any deeper completion
           hit first, and only set this TypeAtPos completion if nothing else
           here hit. *)
        Ast_iterator.default_iterator.value_binding iterator value_binding;
        setResult
          (Completable.Cexpression
             {contextPath = CTypeAtPos loc; prefix; nested = List.rev nested})
      | _ -> ())
    | {
     pvb_pat = {ppat_desc = Ppat_constraint (_pat, coreType); ppat_loc};
     pvb_expr;
    }
      when locHasCursor value_binding.pvb_loc
           && locHasCursor ppat_loc = false
           && locHasCursor pvb_expr.pexp_loc = false
           && CompletionExpressions.isExprHole pvb_expr -> (
      (* Expression with derivable type annotation, when the expression is empty (expr hole).
         E.g: let x: someRecord = <com> *)
      match TypeUtils.contextPathFromCoreType coreType with
      | Some ctxPath ->
        setResult
          (Completable.Cexpression
             {contextPath = ctxPath; prefix = ""; nested = []})
      | _ -> ())
    | {pvb_pat; pvb_expr} when locHasCursor pvb_pat.ppat_loc -> (
      (* Completing a destructuring.
         E.g: let {<com>} = someVar *)
      match
        ( pvb_pat
          |> CompletionPatterns.traversePattern ~patternPath:[] ~locHasCursor
               ~firstCharBeforeCursorNoWhite ~posBeforeCursor,
          exprToContextPath pvb_expr )
      with
      | Some (prefix, nested), Some ctxPath ->
        setResult
          (Completable.Cpattern
             {
               contextPath = ctxPath;
               prefix;
               nested = List.rev nested;
               fallback = None;
               patternMode = Destructuring;
             })
      | _ -> ())
    | _ -> ());
    Ast_iterator.default_iterator.value_binding iterator value_binding;
    inJsxContext := oldInJsxContext
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
         setResult (Completable.Cdecorator label)
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
         setResult (Completable.CdecoratorPayload (Module s))
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
           setResult (Completable.CdecoratorPayload (Module s))
         | false, true, true, _ ->
           if Debug.verbose () then
             print_endline
               "[decoratorCompletion] @module `from` payload was expr hole";
           setResult (Completable.CdecoratorPayload (Module ""))
         | _ -> ())
       | PStr [{pstr_desc = Pstr_eval (expr, _)}] -> (
         if Debug.verbose () then
           print_endline
             "[decoratorCompletion] Found @module with non-string payload";
         match
           CompletionExpressions.traverseExpr expr ~exprPath:[]
             ~pos:posBeforeCursor ~firstCharBeforeCursorNoWhite
         with
         | None -> ()
         | Some (prefix, nested) ->
           if Debug.verbose () then
             print_endline "[decoratorCompletion] Found @module record path";
           setResult
             (Completable.CdecoratorPayload
                (ModuleWithImportAttributes {nested = List.rev nested; prefix}))
         )
       | _ -> ()
     else if id.txt = "jsxConfig" then
       match payload with
       | PStr [{pstr_desc = Pstr_eval (expr, _)}] -> (
         if Debug.verbose () then
           print_endline "[decoratorCompletion] Found @jsxConfig";
         match
           CompletionExpressions.traverseExpr expr ~exprPath:[]
             ~pos:posBeforeCursor ~firstCharBeforeCursorNoWhite
         with
         | None -> ()
         | Some (prefix, nested) ->
           if Debug.verbose () then
             print_endline "[decoratorCompletion] Found @jsxConfig path!";
           setResult
             (Completable.CdecoratorPayload
                (JsxConfig {nested = List.rev nested; prefix})))
       | _ -> ());
    Ast_iterator.default_iterator.attribute iterator (id, payload)
  in
  let rec iterateFnArguments ~args ~iterator ~isPipe
      (argCompletable : Completable.t option) =
    match argCompletable with
    | None -> (
      match !currentCtxPath with
      | None -> ()
      | Some functionContextPath ->
        let currentUnlabelledCount = ref (if isPipe then 1 else 0) in
        args
        |> List.iter (fun (arg : arg) ->
               let previousCtxPath = !currentCtxPath in
               setCurrentCtxPath
                 (CArgument
                    {
                      functionContextPath;
                      argumentLabel =
                        (match arg with
                        | {label = None} ->
                          let current = !currentUnlabelledCount in
                          currentUnlabelledCount := current + 1;
                          Unlabelled {argumentPosition = current}
                        | {label = Some {name; opt = true}} -> Optional name
                        | {label = Some {name; opt = false}} -> Labelled name);
                    });
               expr iterator arg.exp;
               resetCurrentCtxPath previousCtxPath))
    | Some argCompletable -> setResult argCompletable
  and iterateJsxProps ~iterator (props : CompletionJsx.jsxProps) =
    props.props
    |> List.iter (fun (prop : CompletionJsx.prop) ->
           let previousCtxPath = !currentCtxPath in
           setCurrentCtxPath
             (CJsxPropValue
                {
                  pathToComponent =
                    Utils.flattenLongIdent ~jsx:true props.compName.txt;
                  propName = prop.name;
                  emptyJsxPropNameHint = None;
                });
           expr iterator prop.exp;
           resetCurrentCtxPath previousCtxPath)
  and expr (iterator : Ast_iterator.iterator) (expr : Parsetree.expression) =
    let oldInJsxContext = !inJsxContext in
    let processed = ref false in
    let setFound () =
      found := true;
      if debug then
        Printf.printf "posCursor:[%s] posNoWhite:[%s] Found expr:%s\n"
          (Pos.toString posCursor) (Pos.toString posNoWhite)
          (Loc.toString expr.pexp_loc)
    in
    (match findThisExprLoc with
    | Some loc when expr.pexp_loc = loc -> (
      match exprToContextPath expr with
      | None -> ()
      | Some ctxPath -> setResult (Cpath ctxPath))
    | _ -> ());
    let setPipeResult ~(lhs : Parsetree.expression) ~id =
      match completePipeChain lhs with
      | None -> (
        match exprToContextPath lhs with
        | Some pipe ->
          setResult
            (Cpath
               (CPPipe
                  {
                    contextPath = pipe;
                    id;
                    lhsLoc = lhs.pexp_loc;
                    inJsx = !inJsxContext;
                  }));
          true
        | None -> false)
      | Some (pipe, lhsLoc) ->
        setResult
          (Cpath
             (CPPipe {contextPath = pipe; id; lhsLoc; inJsx = !inJsxContext}));
        true
    in
    typedCompletionExpr expr;
    match expr.pexp_desc with
    | Pexp_match (expr, cases)
      when cases <> []
           && locHasCursor expr.pexp_loc = false
           && Option.is_none findThisExprLoc ->
      if Debug.verbose () then
        print_endline "[completionFrontend] Checking each case";
      let ctxPath = exprToContextPath expr in
      let oldCtxPath = !currentCtxPath in
      cases
      |> List.iter (fun (case : Parsetree.case) ->
             let oldScope = !scope in
             if
               locHasCursor case.pc_rhs.pexp_loc = false
               && locHasCursor case.pc_lhs.ppat_loc
             then completePattern ?contextPath:ctxPath case.pc_lhs;
             scopePattern ?contextPath:ctxPath case.pc_lhs;
             Ast_iterator.default_iterator.case iterator case;
             scope := oldScope);
      resetCurrentCtxPath oldCtxPath
    | Pexp_apply
        ( {pexp_desc = Pexp_ident {txt = Lident ("|." | "|.u"); loc = opLoc}},
          [
            (_, lhs);
            (_, {pexp_desc = Pexp_extension _; pexp_loc = {loc_ghost = true}});
          ] )
      when opLoc |> Loc.hasPos ~pos:posBeforeCursor ->
      (* Case foo-> when the parser adds a ghost expression to the rhs
         so the apply expression does not include the cursor *)
      if setPipeResult ~lhs ~id:"" then setFound ()
    | _ -> (
      if expr.pexp_loc |> Loc.hasPos ~pos:posNoWhite && !result = None then (
        setFound ();
        match expr.pexp_desc with
        | Pexp_extension ({txt}, _) -> setResult (CextensionNode txt)
        | Pexp_constant _ -> setResult Cnone
        | Pexp_ident lid ->
          let lidPath = flattenLidCheckDot lid in
          if debug then
            Printf.printf "Pexp_ident %s:%s\n"
              (lidPath |> String.concat ".")
              (Loc.toString lid.loc);
          if lid.loc |> Loc.hasPos ~pos:posBeforeCursor then
            let isLikelyModulePath =
              match lidPath with
              | head :: _
                when String.length head > 0
                     && head.[0] == Char.uppercase_ascii head.[0] ->
                true
              | _ -> false
            in
            setResult
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
                    }))
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
          then
            setResult
              (Cpath
                 (CPId
                    {loc = lid.loc; path = lidPath; completionContext = Value}))
          else
            match eOpt with
            | Some e when locHasCursor e.pexp_loc -> (
              match
                CompletionExpressions.completeConstructorPayload
                  ~posBeforeCursor ~firstCharBeforeCursorNoWhite lid e
              with
              | Some result ->
                (* Check if anything else more important completes before setting this completion. *)
                Ast_iterator.default_iterator.expr iterator e;
                setResult result
              | None -> ())
            | _ -> ())
        | Pexp_field (e, fieldName) -> (
          if debug then
            Printf.printf "Pexp_field %s %s:%s\n" (Loc.toString e.pexp_loc)
              (Utils.flattenLongIdent fieldName.txt |> String.concat ".")
              (Loc.toString fieldName.loc);
          if fieldName.loc |> Loc.hasPos ~pos:posBeforeCursor then
            match fieldName.txt with
            | Lident name -> (
              match exprToContextPath e with
              | Some contextPath ->
                let contextPath = Completable.CPField (contextPath, name) in
                setResult (Cpath contextPath)
              | None -> ())
            | Ldot (id, name) ->
              (* Case x.M.field ignore the x part *)
              let contextPath =
                Completable.CPField
                  ( CPId
                      {
                        loc = fieldName.loc;
                        path = Utils.flattenLongIdent id;
                        completionContext = Module;
                      },
                    if blankAfterCursor = Some '.' then
                      (* x.M. field  --->  M. *) ""
                    else if name = "_" then ""
                    else name )
              in
              setResult (Cpath contextPath)
            | Lapply _ -> ()
          else if Loc.end_ e.pexp_loc = posBeforeCursor then
            match exprToContextPath e with
            | Some contextPath -> setResult (Cpath (CPField (contextPath, "")))
            | None -> ())
        | Pexp_apply ({pexp_desc = Pexp_ident compName}, args)
          when Res_parsetree_viewer.is_jsx_expression expr ->
          inJsxContext := true;
          let jsxProps = CompletionJsx.extractJsxProps ~compName ~args in
          let compNamePath = flattenLidCheckDot ~jsx:true compName in
          if debug then
            Printf.printf "JSX <%s:%s %s> _children:%s\n"
              (compNamePath |> String.concat ".")
              (Loc.toString compName.loc)
              (jsxProps.props
              |> List.map
                   (fun ({name; posStart; posEnd; exp} : CompletionJsx.prop) ->
                     Printf.sprintf "%s[%s->%s]=...%s" name
                       (Pos.toString posStart) (Pos.toString posEnd)
                       (Loc.toString exp.pexp_loc))
              |> String.concat " ")
              (match jsxProps.childrenStart with
              | None -> "None"
              | Some childrenPosStart -> Pos.toString childrenPosStart);
          let jsxCompletable =
            CompletionJsx.findJsxPropsCompletable ~jsxProps
              ~endPos:(Loc.end_ expr.pexp_loc) ~posBeforeCursor
              ~posAfterCompName:(Loc.end_ compName.loc)
              ~firstCharBeforeCursorNoWhite ~charAtCursor
          in
          if jsxCompletable <> None then setResultOpt jsxCompletable
          else if compName.loc |> Loc.hasPos ~pos:posBeforeCursor then
            setResult
              (match compNamePath with
              | [prefix] when Char.lowercase_ascii prefix.[0] = prefix.[0] ->
                ChtmlElement {prefix}
              | _ ->
                Cpath
                  (CPId
                     {
                       loc = compName.loc;
                       path = compNamePath;
                       completionContext = Module;
                     }))
          else iterateJsxProps ~iterator jsxProps
        | Pexp_apply
            ( {pexp_desc = Pexp_ident {txt = Lident ("|." | "|.u")}},
              [
                (_, lhs);
                (_, {pexp_desc = Pexp_ident {txt = Longident.Lident id; loc}});
              ] )
          when loc |> Loc.hasPos ~pos:posBeforeCursor ->
          if Debug.verbose () then print_endline "[expr_iter] Case foo->id";
          setPipeResult ~lhs ~id |> ignore
        | Pexp_apply
            ( {pexp_desc = Pexp_ident {txt = Lident ("|." | "|.u"); loc = opLoc}},
              [(_, lhs); _] )
          when Loc.end_ opLoc = posCursor ->
          if Debug.verbose () then print_endline "[expr_iter] Case foo->";
          setPipeResult ~lhs ~id:"" |> ignore
        | Pexp_apply
            ( {pexp_desc = Pexp_ident {txt = Lident ("|." | "|.u")}},
              [_; (_, {pexp_desc = Pexp_apply (funExpr, args)})] )
          when (* Normally named arg completion fires when the cursor is right after the expression.
                  E.g in foo(~<---there
                  But it should not fire in foo(~a)<---there *)
               not
                 (Loc.end_ expr.pexp_loc = posCursor
                 && charBeforeCursor = Some ')') -> (
          (* Complete fn argument values and named args when the fn call is piped. E.g. someVar->someFn(<com>). *)
          if Debug.verbose () then
            print_endline "[expr_iter] Complete fn arguments (piped)";
          let args = extractExpApplyArgs ~args in
          let funCtxPath = exprToContextPath funExpr in
          let argCompletable =
            match funCtxPath with
            | Some contextPath ->
              findArgCompletables ~contextPath ~args
                ~endPos:(Loc.end_ expr.pexp_loc) ~posBeforeCursor
                ~posAfterFunExpr:(Loc.end_ funExpr.pexp_loc)
                ~charBeforeCursor ~isPipedExpr:true
                ~firstCharBeforeCursorNoWhite
            | None -> None
          in
          match argCompletable with
          | None -> (
            match funCtxPath with
            | None -> ()
            | Some funCtxPath ->
              let oldCtxPath = !currentCtxPath in
              setCurrentCtxPath funCtxPath;
              argCompletable |> iterateFnArguments ~isPipe:true ~args ~iterator;
              resetCurrentCtxPath oldCtxPath)
          | Some argCompletable -> setResult argCompletable)
        | Pexp_apply
            ({pexp_desc = Pexp_ident {txt = Lident ("|." | "|.u")}}, [_; _]) ->
          (* Ignore any other pipe. *)
          ()
        | Pexp_apply (funExpr, args)
          when not
                 (Loc.end_ expr.pexp_loc = posCursor
                 && charBeforeCursor = Some ')') -> (
          (* Complete fn argument values and named args when the fn call is _not_ piped. E.g. someFn(<com>). *)
          if Debug.verbose () then
            print_endline "[expr_iter] Complete fn arguments (not piped)";
          let args = extractExpApplyArgs ~args in
          if debug then
            Printf.printf "Pexp_apply ...%s (%s)\n"
              (Loc.toString funExpr.pexp_loc)
              (args
              |> List.map (fun {label; exp} ->
                     Printf.sprintf "%s...%s"
                       (match label with
                       | None -> ""
                       | Some {name; opt; posStart; posEnd} ->
                         "~" ^ name ^ Pos.toString posStart ^ "->"
                         ^ Pos.toString posEnd ^ "="
                         ^ if opt then "?" else "")
                       (Loc.toString exp.pexp_loc))
              |> String.concat ", ");

          let funCtxPath = exprToContextPath funExpr in
          let argCompletable =
            match funCtxPath with
            | Some contextPath ->
              findArgCompletables ~contextPath ~args
                ~endPos:(Loc.end_ expr.pexp_loc) ~posBeforeCursor
                ~posAfterFunExpr:(Loc.end_ funExpr.pexp_loc)
                ~charBeforeCursor ~isPipedExpr:false
                ~firstCharBeforeCursorNoWhite
            | None -> None
          in
          match argCompletable with
          | None -> (
            match funCtxPath with
            | None -> ()
            | Some funCtxPath ->
              let oldCtxPath = !currentCtxPath in
              setCurrentCtxPath funCtxPath;
              argCompletable |> iterateFnArguments ~isPipe:false ~args ~iterator;
              resetCurrentCtxPath oldCtxPath)
          | Some argCompletable -> setResult argCompletable)
        | Pexp_send (lhs, {txt; loc}) -> (
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
            match exprToContextPath lhs with
            | Some contextPath -> setResult (Cpath (CPObj (contextPath, label)))
            | None -> ())
        | Pexp_fun
            {arg_label = lbl; default = defaultExpOpt; lhs = pat; rhs = e} ->
          let oldScope = !scope in
          (match (!processingFun, !currentCtxPath) with
          | None, Some ctxPath -> processingFun := Some (ctxPath, 0)
          | _ -> ());
          let argContextPath =
            match !processingFun with
            | None -> None
            | Some (ctxPath, currentUnlabelledCount) ->
              (processingFun :=
                 match lbl with
                 | Nolabel -> Some (ctxPath, currentUnlabelledCount + 1)
                 | _ -> Some (ctxPath, currentUnlabelledCount));
              if Debug.verbose () then
                print_endline "[expr_iter] Completing for argument value";
              Some
                (Completable.CArgument
                   {
                     functionContextPath = ctxPath;
                     argumentLabel =
                       (match lbl with
                       | Nolabel ->
                         Unlabelled {argumentPosition = currentUnlabelledCount}
                       | Optional name -> Optional name
                       | Labelled name -> Labelled name);
                   })
          in
          (match defaultExpOpt with
          | None -> ()
          | Some defaultExp -> iterator.expr iterator defaultExp);
          if locHasCursor e.pexp_loc = false then
            completePattern ?contextPath:argContextPath pat;
          scopePattern ?contextPath:argContextPath pat;
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
      if not !processed then Ast_iterator.default_iterator.expr iterator expr;
      inJsxContext := oldInJsxContext;
      match expr.pexp_desc with
      | Pexp_fun _ -> ()
      | _ -> processingFun := None)
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
          setResult
            (Cpath
               (CPId {loc = lid.loc; path = lidPath; completionContext = Type}))
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
      | Ppat_construct (lid, _) -> (
        let lidPath = flattenLidCheckDot lid in
        if debug then
          Printf.printf "Ppat_construct %s:%s\n"
            (lidPath |> String.concat ".")
            (Loc.toString lid.loc);
        let completion =
          Completable.Cpath
            (CPId {loc = lid.loc; path = lidPath; completionContext = Value})
        in
        match !result with
        | Some (Completable.Cpattern p, scope) ->
          result := Some (Cpattern {p with fallback = Some completion}, scope)
        | _ -> setResult completion)
      | _ -> ());
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
      found := true;
      setResult
        (Cpath
           (CPId {loc = lid.loc; path = lidPath; completionContext = Module}))
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
      found := true;
      setResult
        (Cpath
           (CPId {loc = lid.loc; path = lidPath; completionContext = Module}))
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
      found := true;
      setResult
        (Cpath
           (CPId
              {
                loc = decl.pcd_name.loc;
                path = [decl.pcd_name.txt];
                completionContext = Value;
              }))
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
    iterator.structure iterator str |> ignore;
    if blankAfterCursor = Some ' ' || blankAfterCursor = Some '\n' then (
      scope := !lastScopeBeforeCursor;
      setResult
        (Cpath
           (CPId {loc = Location.none; path = [""]; completionContext = Value})));
    if !found = false then if debug then Printf.printf "XXX Not found!\n";
    !result)
  else if Filename.check_suffix path ".resi" then (
    let parser = Res_driver.parsing_engine.parse_interface ~for_printer:false in
    let {Res_driver.parsetree = signature} = parser ~filename:currentFile in
    iterator.signature iterator signature |> ignore;
    if blankAfterCursor = Some ' ' || blankAfterCursor = Some '\n' then (
      scope := !lastScopeBeforeCursor;
      setResult
        (Cpath
           (CPId {loc = Location.none; path = [""]; completionContext = Type})));
    if !found = false then if debug then Printf.printf "XXX Not found!\n";
    !result)
  else None

let completionWithParser ~debug ~path ~posCursor ~currentFile ~text =
  match Pos.positionToOffset text posCursor with
  | Some offset ->
    completionWithParser1 ~currentFile ~debug ~offset ~path ~posCursor text
  | None -> None

let findTypeOfExpressionAtLoc ~debug ~path ~posCursor ~currentFile loc =
  let textOpt = Files.readFile currentFile in
  match textOpt with
  | None | Some "" -> None
  | Some text -> (
    match Pos.positionToOffset text posCursor with
    | Some offset ->
      completionWithParser1 ~findThisExprLoc:loc ~currentFile ~debug ~offset
        ~path ~posCursor text
    | None -> None)
