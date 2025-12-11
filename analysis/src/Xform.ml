(** Code transformations using the parser/printer and ast operations *)

let isBracedExpr = Res_parsetree_viewer.is_braced_expr

let extractTypeFromExpr expr ~debug ~path ~currentFile ~full ~pos =
  match
    expr.Parsetree.pexp_loc
    |> CompletionFrontEnd.findTypeOfExpressionAtLoc ~debug ~path ~currentFile
         ~posCursor:(Pos.ofLexing expr.Parsetree.pexp_loc.loc_start)
  with
  | Some (completable, scope) -> (
    let env = SharedTypes.QueryEnv.fromFile full.SharedTypes.file in
    let completions =
      completable
      |> CompletionBackEnd.processCompletable ~debug ~full ~pos ~scope ~env
           ~forHover:true
    in
    let rawOpens = Scope.getRawOpens scope in
    match completions with
    | {env} :: _ -> (
      let opens =
        CompletionBackEnd.getOpens ~debug ~rawOpens ~package:full.package ~env
      in
      match
        CompletionBackEnd.completionsGetCompletionType2 ~debug ~full ~rawOpens
          ~opens ~pos completions
      with
      | Some (typ, _env) ->
        let extractedType =
          match typ with
          | ExtractedType t -> Some t
          | TypeExpr t ->
            TypeUtils.extractType t ~env ~package:full.package
            |> TypeUtils.getExtractedType
        in
        extractedType
      | None -> None)
    | _ -> None)
  | _ -> None

module IfThenElse = struct
  (* Convert if-then-else to switch *)

  let rec listToPat ~itemToPat = function
    | [] -> Some []
    | x :: xList -> (
      match (itemToPat x, listToPat ~itemToPat xList) with
      | Some p, Some pList -> Some (p :: pList)
      | _ -> None)

  let rec expToPat (exp : Parsetree.expression) =
    let mkPat ppat_desc =
      Ast_helper.Pat.mk ~loc:exp.pexp_loc ~attrs:exp.pexp_attributes ppat_desc
    in
    match exp.pexp_desc with
    | Pexp_construct (lid, None) -> Some (mkPat (Ppat_construct (lid, None)))
    | Pexp_construct (lid, Some e1) -> (
      match expToPat e1 with
      | None -> None
      | Some p1 -> Some (mkPat (Ppat_construct (lid, Some p1))))
    | Pexp_variant (label, None) -> Some (mkPat (Ppat_variant (label, None)))
    | Pexp_variant (label, Some e1) -> (
      match expToPat e1 with
      | None -> None
      | Some p1 -> Some (mkPat (Ppat_variant (label, Some p1))))
    | Pexp_constant c -> Some (mkPat (Ppat_constant c))
    | Pexp_tuple eList -> (
      match listToPat ~itemToPat:expToPat eList with
      | None -> None
      | Some patList -> Some (mkPat (Ppat_tuple patList)))
    | Pexp_record (items, None) -> (
      let itemToPat {Parsetree.lid; x = e; opt} =
        match expToPat e with
        | None -> None
        | Some p -> Some {Parsetree.lid; x = p; opt}
      in
      match listToPat ~itemToPat items with
      | None -> None
      | Some patItems -> Some (mkPat (Ppat_record (patItems, Closed))))
    | Pexp_record (_, Some _) -> None
    | _ -> None

  let mkIterator ~pos ~changed =
    let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) =
      let newExp =
        match e.pexp_desc with
        | Pexp_ifthenelse
            ( {
                pexp_desc =
                  Pexp_apply
                    {
                      funct =
                        {
                          pexp_desc =
                            Pexp_ident
                              {txt = Longident.Lident (("==" | "!=") as op)};
                        };
                      args = [(Nolabel, arg1); (Nolabel, arg2)];
                    };
              },
              e1,
              Some e2 )
          when Loc.hasPos ~pos e.pexp_loc -> (
          let e1, e2 = if op = "==" then (e1, e2) else (e2, e1) in
          let mkMatch ~arg ~pat =
            let cases =
              [
                Ast_helper.Exp.case pat e1;
                Ast_helper.Exp.case (Ast_helper.Pat.any ()) e2;
              ]
            in
            Ast_helper.Exp.match_ ~loc:e.pexp_loc ~attrs:e.pexp_attributes arg
              cases
          in

          match expToPat arg2 with
          | None -> (
            match expToPat arg1 with
            | None -> None
            | Some pat1 ->
              let newExp = mkMatch ~arg:arg2 ~pat:pat1 in
              Some newExp)
          | Some pat2 ->
            let newExp = mkMatch ~arg:arg1 ~pat:pat2 in
            Some newExp)
        | _ -> None
      in
      match newExp with
      | Some newExp -> changed := Some newExp
      | None -> Ast_iterator.default_iterator.expr iterator e
    in

    {Ast_iterator.default_iterator with expr}

  let xform ~pos ~codeActions ~printExpr ~path structure =
    let changed = ref None in
    let iterator = mkIterator ~pos ~changed in
    iterator.structure iterator structure;
    match !changed with
    | None -> ()
    | Some newExpr ->
      let range = Loc.rangeOfLoc newExpr.pexp_loc in
      let newText = printExpr ~range newExpr in
      let codeAction =
        CodeActions.make ~title:"Replace with switch" ~kind:RefactorRewrite
          ~uri:path ~newText ~range
      in
      codeActions := codeAction :: !codeActions
end

module ModuleToFile = struct
  let mkIterator ~pos ~changed ~path ~printStandaloneStructure =
    let structure_item (iterator : Ast_iterator.iterator)
        (structure_item : Parsetree.structure_item) =
      (match structure_item.pstr_desc with
      | Pstr_module
          {pmb_loc; pmb_name; pmb_expr = {pmod_desc = Pmod_structure structure}}
        when structure_item.pstr_loc |> Loc.hasPos ~pos ->
        let range = Loc.rangeOfLoc structure_item.pstr_loc in
        let newTextInCurrentFile = "" in
        let textForExtractedFile =
          printStandaloneStructure ~loc:pmb_loc structure
        in
        let moduleName = pmb_name.txt in
        let newFilePath =
          Uri.fromPath
            (Filename.concat (Filename.dirname path) moduleName ^ ".res")
        in
        changed :=
          Some
            (CodeActions.makeWithDocumentChanges
               ~title:
                 (Printf.sprintf "Extract local module \"%s\" to file \"%s\""
                    moduleName (moduleName ^ ".res"))
               ~kind:RefactorRewrite
               ~documentChanges:
                 [
                   Protocol.CreateFile
                     {
                       uri = newFilePath |> Uri.toString;
                       options =
                         Some
                           {overwrite = Some false; ignoreIfExists = Some true};
                     };
                   TextDocumentEdit
                     {
                       textDocument =
                         {uri = newFilePath |> Uri.toString; version = None};
                       edits =
                         [
                           {
                             newText = textForExtractedFile;
                             range =
                               {
                                 start = {line = 0; character = 0};
                                 end_ = {line = 0; character = 0};
                               };
                           };
                         ];
                     };
                   TextDocumentEdit
                     {
                       textDocument = {uri = path; version = None};
                       edits = [{newText = newTextInCurrentFile; range}];
                     };
                 ]);
        ()
      | _ -> ());
      Ast_iterator.default_iterator.structure_item iterator structure_item
    in

    {Ast_iterator.default_iterator with structure_item}

  let xform ~pos ~codeActions ~path ~printStandaloneStructure structure =
    let changed = ref None in
    let iterator = mkIterator ~pos ~path ~changed ~printStandaloneStructure in
    iterator.structure iterator structure;
    match !changed with
    | None -> ()
    | Some codeAction -> codeActions := codeAction :: !codeActions
end

module AddBracesToFn = struct
  (* Add braces to fn without braces *)

  let mkIterator ~pos ~changed =
    (* While iterating the AST, keep info on which structure item we are in.
       Printing from the structure item, rather than the body of the function,
       gives better local pretty printing *)
    let currentStructureItem = ref None in

    let structure_item (iterator : Ast_iterator.iterator)
        (item : Parsetree.structure_item) =
      let saved = !currentStructureItem in
      currentStructureItem := Some item;
      Ast_iterator.default_iterator.structure_item iterator item;
      currentStructureItem := saved
    in
    let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) =
      let bracesAttribute =
        let loc =
          {
            Location.none with
            loc_start = Lexing.dummy_pos;
            loc_end =
              {
                Lexing.dummy_pos with
                pos_lnum = Lexing.dummy_pos.pos_lnum + 1 (* force line break *);
              };
          }
        in
        (Location.mkloc "res.braces" loc, Parsetree.PStr [])
      in
      let isFunction = function
        | {Parsetree.pexp_desc = Pexp_fun _} -> true
        | _ -> false
      in
      (match e.pexp_desc with
      | Pexp_fun {rhs = bodyExpr}
        when Loc.hasPos ~pos bodyExpr.pexp_loc
             && isBracedExpr bodyExpr = false
             && isFunction bodyExpr = false ->
        bodyExpr.pexp_attributes <- bracesAttribute :: bodyExpr.pexp_attributes;
        changed := !currentStructureItem
      | _ -> ());
      Ast_iterator.default_iterator.expr iterator e
    in

    {Ast_iterator.default_iterator with expr; structure_item}

  let xform ~pos ~codeActions ~path ~printStructureItem structure =
    let changed = ref None in
    let iterator = mkIterator ~pos ~changed in
    iterator.structure iterator structure;
    match !changed with
    | None -> ()
    | Some newStructureItem ->
      let range = Loc.rangeOfLoc newStructureItem.pstr_loc in
      let newText = printStructureItem ~range newStructureItem in
      let codeAction =
        CodeActions.make ~title:"Add braces to function" ~kind:RefactorRewrite
          ~uri:path ~newText ~range
      in
      codeActions := codeAction :: !codeActions
end

module AddTypeAnnotation = struct
  (* Add type annotation to value declaration *)

  type annotation = Plain | WithParens

  let mkIterator ~pos ~result =
    let processPattern ?(isUnlabeledOnlyArg = false) (pat : Parsetree.pattern) =
      match pat.ppat_desc with
      | Ppat_var {loc} when Loc.hasPos ~pos loc ->
        result := Some (if isUnlabeledOnlyArg then WithParens else Plain)
      | _ -> ()
    in
    let rec processFunction ~argNum (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_fun {arg_label; lhs = pat; rhs = e} ->
        let isUnlabeledOnlyArg =
          argNum = 1 && arg_label = Nolabel
          &&
          match e.pexp_desc with
          | Pexp_fun _ -> false
          | _ -> true
        in
        processPattern ~isUnlabeledOnlyArg pat;
        processFunction ~argNum:(argNum + 1) e
      | _ -> ()
    in
    let structure_item (iterator : Ast_iterator.iterator)
        (si : Parsetree.structure_item) =
      match si.pstr_desc with
      | Pstr_value (_recFlag, bindings) ->
        let processBinding (vb : Parsetree.value_binding) =
          (* Can't add a type annotation to a jsx component, or the compiler crashes *)
          let isJsxComponent = Utils.isJsxComponent vb in
          if not isJsxComponent then processPattern vb.pvb_pat;
          processFunction vb.pvb_expr
        in
        bindings |> List.iter (processBinding ~argNum:1);
        Ast_iterator.default_iterator.structure_item iterator si
      | _ -> Ast_iterator.default_iterator.structure_item iterator si
    in
    {Ast_iterator.default_iterator with structure_item}

  let xform ~path ~pos ~full ~structure ~codeActions ~debug =
    let result = ref None in
    let iterator = mkIterator ~pos ~result in
    iterator.structure iterator structure;
    match !result with
    | None -> ()
    | Some annotation -> (
      match References.getLocItem ~full ~pos ~debug with
      | None -> ()
      | Some locItem -> (
        match locItem.locType with
        | Typed (name, typ, _) ->
          let range, newText =
            match annotation with
            | Plain ->
              ( Loc.rangeOfLoc {locItem.loc with loc_start = locItem.loc.loc_end},
                ": " ^ (typ |> Shared.typeToString) )
            | WithParens ->
              ( Loc.rangeOfLoc locItem.loc,
                "(" ^ name ^ ": " ^ (typ |> Shared.typeToString) ^ ")" )
          in
          let codeAction =
            CodeActions.make ~title:"Add type annotation" ~kind:RefactorRewrite
              ~uri:path ~newText ~range
          in
          codeActions := codeAction :: !codeActions
        | _ -> ()))
end

module ExpandCatchAllForVariants = struct
  let mkIterator ~pos ~result =
    let expr (iterator : Ast_iterator.iterator) (e : Parsetree.expression) =
      (if e.pexp_loc |> Loc.hasPos ~pos then
         match e.pexp_desc with
         | Pexp_match (switchExpr, cases) -> (
           let catchAllCase =
             cases
             |> List.find_opt (fun (c : Parsetree.case) ->
                    match c with
                    | {pc_lhs = {ppat_desc = Ppat_any}} -> true
                    | _ -> false)
           in
           match catchAllCase with
           | None -> ()
           | Some catchAllCase ->
             result := Some (switchExpr, catchAllCase, cases))
         | _ -> ());
      Ast_iterator.default_iterator.expr iterator e
    in
    {Ast_iterator.default_iterator with expr}

  let xform ~path ~pos ~full ~structure ~currentFile ~codeActions ~debug =
    let result = ref None in
    let iterator = mkIterator ~pos ~result in
    iterator.structure iterator structure;
    match !result with
    | None -> ()
    | Some (switchExpr, catchAllCase, cases) -> (
      if Debug.verbose () then
        print_endline
          "[codeAction - ExpandCatchAllForVariants] Found target switch";
      let rec findAllConstructorNames ?(mode : [`option | `default] = `default)
          ?(constructorNames = []) (p : Parsetree.pattern) =
        match p.ppat_desc with
        | Ppat_construct ({txt = Lident "Some"}, Some payload)
          when mode = `option ->
          findAllConstructorNames ~mode ~constructorNames payload
        | Ppat_construct ({txt}, _) -> Longident.last txt :: constructorNames
        | Ppat_variant (name, _) -> name :: constructorNames
        | Ppat_or (a, b) ->
          findAllConstructorNames ~mode ~constructorNames a
          @ findAllConstructorNames ~mode ~constructorNames b
          @ constructorNames
        | _ -> constructorNames
      in
      let getCurrentConstructorNames ?mode cases =
        cases
        |> List.map (fun (c : Parsetree.case) ->
               if Option.is_some c.pc_guard then []
               else findAllConstructorNames ?mode c.pc_lhs)
        |> List.flatten
      in
      let currentConstructorNames = getCurrentConstructorNames cases in
      match
        switchExpr
        |> extractTypeFromExpr ~debug ~path ~currentFile ~full
             ~pos:(Pos.ofLexing switchExpr.pexp_loc.loc_end)
      with
      | Some (Tvariant {constructors}) ->
        let missingConstructors =
          constructors
          |> List.filter (fun (c : SharedTypes.Constructor.t) ->
                 currentConstructorNames |> List.mem c.cname.txt = false)
        in
        if List.length missingConstructors > 0 then
          let newText =
            missingConstructors
            |> List.map (fun (c : SharedTypes.Constructor.t) ->
                   c.cname.txt
                   ^
                   match c.args with
                   | Args [] -> ""
                   | Args _ | InlineRecord _ -> "(_)")
            |> String.concat " | "
          in
          let range = Loc.rangeOfLoc catchAllCase.pc_lhs.ppat_loc in
          let codeAction =
            CodeActions.make ~title:"Expand catch-all" ~kind:RefactorRewrite
              ~uri:path ~newText ~range
          in
          codeActions := codeAction :: !codeActions
        else ()
      | Some (Tpolyvariant {constructors}) ->
        let missingConstructors =
          constructors
          |> List.filter (fun (c : SharedTypes.polyVariantConstructor) ->
                 currentConstructorNames |> List.mem c.name = false)
        in
        if List.length missingConstructors > 0 then
          let newText =
            missingConstructors
            |> List.map (fun (c : SharedTypes.polyVariantConstructor) ->
                   Res_printer.polyvar_ident_to_string c.name
                   ^
                   match c.args with
                   | [] -> ""
                   | _ -> "(_)")
            |> String.concat " | "
          in
          let range = Loc.rangeOfLoc catchAllCase.pc_lhs.ppat_loc in
          let codeAction =
            CodeActions.make ~title:"Expand catch-all" ~kind:RefactorRewrite
              ~uri:path ~newText ~range
          in
          codeActions := codeAction :: !codeActions
        else ()
      | Some (Toption (env, innerType)) -> (
        if Debug.verbose () then
          print_endline
            "[codeAction - ExpandCatchAllForVariants] Found option type";
        let innerType =
          match innerType with
          | ExtractedType t -> Some t
          | TypeExpr t -> (
            match TypeUtils.extractType ~env ~package:full.package t with
            | None -> None
            | Some (t, _) -> Some t)
        in
        match innerType with
        | Some ((Tvariant _ | Tpolyvariant _) as variant) ->
          let currentConstructorNames =
            getCurrentConstructorNames ~mode:`option cases
          in
          let hasNoneCase =
            cases
            |> List.exists (fun (c : Parsetree.case) ->
                   match c.pc_lhs.ppat_desc with
                   | Ppat_construct ({txt = Lident "None"}, _) -> true
                   | _ -> false)
          in
          let missingConstructors =
            match variant with
            | Tvariant {constructors} ->
              constructors
              |> List.filter_map (fun (c : SharedTypes.Constructor.t) ->
                     if currentConstructorNames |> List.mem c.cname.txt = false
                     then
                       Some
                         ( c.cname.txt,
                           match c.args with
                           | Args [] -> false
                           | _ -> true )
                     else None)
            | Tpolyvariant {constructors} ->
              constructors
              |> List.filter_map
                   (fun (c : SharedTypes.polyVariantConstructor) ->
                     if currentConstructorNames |> List.mem c.name = false then
                       Some
                         ( Res_printer.polyvar_ident_to_string c.name,
                           match c.args with
                           | [] -> false
                           | _ -> true )
                     else None)
            | _ -> []
          in
          if List.length missingConstructors > 0 || not hasNoneCase then
            let newText =
              "Some("
              ^ (missingConstructors
                |> List.map (fun (name, hasArgs) ->
                       name ^ if hasArgs then "(_)" else "")
                |> String.concat " | ")
              ^ ")"
            in
            let newText =
              if hasNoneCase then newText else newText ^ " | None"
            in
            let range = Loc.rangeOfLoc catchAllCase.pc_lhs.ppat_loc in
            let codeAction =
              CodeActions.make ~title:"Expand catch-all" ~kind:RefactorRewrite
                ~uri:path ~newText ~range
            in
            codeActions := codeAction :: !codeActions
          else ()
        | _ -> ())
      | _ -> ())
end

module ExhaustiveSwitch = struct
  (* Expand expression to be an exhaustive switch of the underlying value *)
  type posType = Single of Pos.t | Range of Pos.t * Pos.t

  type completionType =
    | Switch of {
        pos: Pos.t;
        switchExpr: Parsetree.expression;
        completionExpr: Parsetree.expression;
      }
    | Selection of {expr: Parsetree.expression}

  let mkIteratorSingle ~pos ~result =
    let expr (iterator : Ast_iterator.iterator) (exp : Parsetree.expression) =
      (match exp.pexp_desc with
      | Pexp_ident _ when Loc.hasPosInclusiveEnd ~pos exp.pexp_loc ->
        (* Exhaustive switch for having the cursor on an identifier. *)
        result := Some (Selection {expr = exp})
      | Pexp_match (completionExpr, [])
        when Loc.hasPosInclusiveEnd ~pos exp.pexp_loc ->
        (* No cases means there's no `|` yet in the switch, so `switch someExpr` *)
        result := Some (Switch {pos; switchExpr = exp; completionExpr})
      | _ -> ());
      Ast_iterator.default_iterator.expr iterator exp
    in
    {Ast_iterator.default_iterator with expr}

  let mkIteratorRange ~startPos ~endPos ~foundSelection =
    let expr (iterator : Ast_iterator.iterator) (exp : Parsetree.expression) =
      let expStartPos = Pos.ofLexing exp.pexp_loc.loc_start in
      let expEndPos = Pos.ofLexing exp.pexp_loc.loc_end in

      (if expStartPos = startPos then
         match !foundSelection with
         | None, endExpr -> foundSelection := (Some exp, endExpr)
         | _ -> ());

      (if expEndPos = endPos then
         match !foundSelection with
         | startExp, _ -> foundSelection := (startExp, Some exp));

      Ast_iterator.default_iterator.expr iterator exp
    in
    {Ast_iterator.default_iterator with expr}

  let xform ~printExpr ~path ~currentFile ~pos ~full ~structure ~codeActions
      ~debug =
    (* TODO: Adapt to '(' as leading/trailing character (skip one col, it's not included in the AST) *)
    let result = ref None in
    let foundSelection = ref (None, None) in
    let iterator =
      match pos with
      | Single pos -> mkIteratorSingle ~pos ~result
      | Range (startPos, endPos) ->
        mkIteratorRange ~startPos ~endPos ~foundSelection
    in
    iterator.structure iterator structure;
    (match !foundSelection with
    | Some startExp, Some endExp ->
      if debug then
        Printf.printf "found selection: %s -> %s\n"
          (Loc.toString startExp.pexp_loc)
          (Loc.toString endExp.pexp_loc);
      result := Some (Selection {expr = startExp})
    | _ -> ());
    match !result with
    | None -> ()
    | Some (Selection {expr}) -> (
      match
        expr
        |> extractTypeFromExpr ~debug ~path ~currentFile ~full
             ~pos:(Pos.ofLexing expr.pexp_loc.loc_start)
      with
      | None -> ()
      | Some extractedType -> (
        let open TypeUtils.Codegen in
        let exhaustiveSwitch =
          extractedTypeToExhaustiveCases
            ~env:(SharedTypes.QueryEnv.fromFile full.file)
            ~full extractedType
        in
        match exhaustiveSwitch with
        | None -> ()
        | Some cases ->
          let range = Loc.rangeOfLoc expr.pexp_loc in
          let newText =
            printExpr ~range {expr with pexp_desc = Pexp_match (expr, cases)}
          in
          let codeAction =
            CodeActions.make ~title:"Exhaustive switch" ~kind:RefactorRewrite
              ~uri:path ~newText ~range
          in
          codeActions := codeAction :: !codeActions))
    | Some (Switch {switchExpr; completionExpr; pos}) -> (
      match
        completionExpr
        |> extractTypeFromExpr ~debug ~path ~currentFile ~full ~pos
      with
      | None -> ()
      | Some extractedType -> (
        let open TypeUtils.Codegen in
        let exhaustiveSwitch =
          extractedTypeToExhaustiveCases
            ~env:(SharedTypes.QueryEnv.fromFile full.file)
            ~full extractedType
        in
        match exhaustiveSwitch with
        | None -> ()
        | Some cases ->
          let range = Loc.rangeOfLoc switchExpr.pexp_loc in
          let newText =
            printExpr ~range
              {switchExpr with pexp_desc = Pexp_match (completionExpr, cases)}
          in
          let codeAction =
            CodeActions.make ~title:"Exhaustive switch" ~kind:RefactorRewrite
              ~uri:path ~newText ~range
          in
          codeActions := codeAction :: !codeActions))
end

module AddDocTemplate = struct
  let createTemplate () =
    let docContent = ["\n"; "\n"] in
    let expression =
      Ast_helper.Exp.constant
        (Parsetree.Pconst_string (String.concat "" docContent, None))
    in
    let structureItemDesc = Parsetree.Pstr_eval (expression, []) in
    let structureItem = Ast_helper.Str.mk structureItemDesc in
    let attrLoc =
      {
        Location.none with
        loc_start = Lexing.dummy_pos;
        loc_end =
          {
            Lexing.dummy_pos with
            pos_lnum = Lexing.dummy_pos.pos_lnum (* force line break *);
          };
      }
    in
    (Location.mkloc "res.doc" attrLoc, Parsetree.PStr [structureItem])

  module Interface = struct
    let mkIterator ~pos ~result =
      let signature_item (iterator : Ast_iterator.iterator)
          (item : Parsetree.signature_item) =
        match item.psig_desc with
        | Psig_value value_description as r
          when Loc.hasPos ~pos value_description.pval_loc
               && ProcessAttributes.findDocAttribute
                    value_description.pval_attributes
                  = None ->
          result := Some (r, item.psig_loc)
        | Psig_type (_, hd :: _) as r
          when Loc.hasPos ~pos hd.ptype_loc
               && ProcessAttributes.findDocAttribute hd.ptype_attributes = None
          ->
          result := Some (r, item.psig_loc)
        | Psig_module {pmd_name = {loc}} as r ->
          if Loc.start loc = pos then result := Some (r, item.psig_loc)
          else Ast_iterator.default_iterator.signature_item iterator item
        | _ -> Ast_iterator.default_iterator.signature_item iterator item
      in
      {Ast_iterator.default_iterator with signature_item}

    let processSigValue (valueDesc : Parsetree.value_description) loc =
      let attr = createTemplate () in
      let newValueBinding =
        {valueDesc with pval_attributes = attr :: valueDesc.pval_attributes}
      in
      let signature_item_desc = Parsetree.Psig_value newValueBinding in
      Ast_helper.Sig.mk ~loc signature_item_desc

    let processTypeDecl (typ : Parsetree.type_declaration) =
      let attr = createTemplate () in
      let newTypeDeclaration =
        {typ with ptype_attributes = attr :: typ.ptype_attributes}
      in
      newTypeDeclaration

    let processModDecl (modDecl : Parsetree.module_declaration) loc =
      let attr = createTemplate () in
      let newModDecl =
        {modDecl with pmd_attributes = attr :: modDecl.pmd_attributes}
      in
      Ast_helper.Sig.mk ~loc (Parsetree.Psig_module newModDecl)

    let xform ~path ~pos ~codeActions ~signature ~printSignatureItem =
      let result = ref None in
      let iterator = mkIterator ~pos ~result in
      iterator.signature iterator signature;
      match !result with
      | Some (signatureItem, loc) -> (
        let newSignatureItem =
          match signatureItem with
          | Psig_value value_desc ->
            Some (processSigValue value_desc value_desc.pval_loc) (* Some loc *)
          | Psig_type (flag, hd :: tl) ->
            let newFirstTypeDecl = processTypeDecl hd in
            Some
              (Ast_helper.Sig.mk ~loc
                 (Parsetree.Psig_type (flag, newFirstTypeDecl :: tl)))
          | Psig_module modDecl -> Some (processModDecl modDecl loc)
          | _ -> None
        in

        match newSignatureItem with
        | Some signatureItem ->
          let range = Loc.rangeOfLoc signatureItem.psig_loc in
          let newText = printSignatureItem ~range signatureItem in
          let codeAction =
            CodeActions.make ~title:"Add Documentation template"
              ~kind:RefactorRewrite ~uri:path ~newText ~range
          in
          codeActions := codeAction :: !codeActions
        | None -> ())
      | None -> ()
  end

  module Implementation = struct
    let mkIterator ~pos ~result =
      let structure_item (iterator : Ast_iterator.iterator)
          (si : Parsetree.structure_item) =
        match si.pstr_desc with
        | Pstr_value (_, {pvb_pat = {ppat_loc}; pvb_attributes} :: _) as r
          when Loc.hasPos ~pos ppat_loc
               && ProcessAttributes.findDocAttribute pvb_attributes = None ->
          result := Some (r, si.pstr_loc)
        | Pstr_primitive value_description as r
          when Loc.hasPos ~pos value_description.pval_loc
               && ProcessAttributes.findDocAttribute
                    value_description.pval_attributes
                  = None ->
          result := Some (r, si.pstr_loc)
        | Pstr_module {pmb_name = {loc}} as r ->
          if Loc.start loc = pos then result := Some (r, si.pstr_loc)
          else Ast_iterator.default_iterator.structure_item iterator si
        | Pstr_type (_, hd :: _) as r
          when Loc.hasPos ~pos hd.ptype_loc
               && ProcessAttributes.findDocAttribute hd.ptype_attributes = None
          ->
          result := Some (r, si.pstr_loc)
        | _ -> Ast_iterator.default_iterator.structure_item iterator si
      in
      {Ast_iterator.default_iterator with structure_item}

    let processValueBinding (valueBinding : Parsetree.value_binding) =
      let attr = createTemplate () in
      let newValueBinding =
        {valueBinding with pvb_attributes = attr :: valueBinding.pvb_attributes}
      in
      newValueBinding

    let processPrimitive (valueDesc : Parsetree.value_description) loc =
      let attr = createTemplate () in
      let newValueDesc =
        {valueDesc with pval_attributes = attr :: valueDesc.pval_attributes}
      in
      Ast_helper.Str.primitive ~loc newValueDesc

    let processModuleBinding (modBind : Parsetree.module_binding) loc =
      let attr = createTemplate () in
      let newModBinding =
        {modBind with pmb_attributes = attr :: modBind.pmb_attributes}
      in
      Ast_helper.Str.module_ ~loc newModBinding

    let xform ~pos ~codeActions ~path ~printStructureItem ~structure =
      let result = ref None in
      let iterator = mkIterator ~pos ~result in
      iterator.structure iterator structure;
      match !result with
      | None -> ()
      | Some (structureItem, loc) -> (
        let newStructureItem =
          match structureItem with
          | Pstr_value (flag, hd :: tl) ->
            let newValueBinding = processValueBinding hd in
            Some
              (Ast_helper.Str.mk ~loc
                 (Parsetree.Pstr_value (flag, newValueBinding :: tl)))
          | Pstr_primitive valueDesc -> Some (processPrimitive valueDesc loc)
          | Pstr_module modBind -> Some (processModuleBinding modBind loc)
          | Pstr_type (flag, hd :: tl) ->
            let newFirstTypeDecl = Interface.processTypeDecl hd in
            Some
              (Ast_helper.Str.mk ~loc
                 (Parsetree.Pstr_type (flag, newFirstTypeDecl :: tl)))
          | _ -> None
        in

        match newStructureItem with
        | Some structureItem ->
          let range = Loc.rangeOfLoc structureItem.pstr_loc in
          let newText = printStructureItem ~range structureItem in
          let codeAction =
            CodeActions.make ~title:"Add Documentation template"
              ~kind:RefactorRewrite ~uri:path ~newText ~range
          in
          codeActions := codeAction :: !codeActions
        | None -> ())
  end
end

let parseImplementation ~filename =
  let {Res_driver.parsetree = structure; comments} =
    Res_driver.parsing_engine.parse_implementation ~for_printer:false ~filename
  in
  let filterComments ~loc comments =
    (* Relevant comments in the range of the expression *)
    let filter comment =
      Loc.hasPos ~pos:(Loc.start (Res_comment.loc comment)) loc
    in
    comments |> List.filter filter
  in
  let printExpr ~(range : Protocol.range) (expr : Parsetree.expression) =
    let structure = [Ast_helper.Str.eval ~loc:expr.pexp_loc expr] in
    structure
    |> Res_printer.print_implementation
         ~comments:(comments |> filterComments ~loc:expr.pexp_loc)
    |> Utils.indent range.start.character
  in
  let printStructureItem ~(range : Protocol.range)
      (item : Parsetree.structure_item) =
    let structure = [item] in
    structure
    |> Res_printer.print_implementation
         ~comments:(comments |> filterComments ~loc:item.pstr_loc)
    |> Utils.indent range.start.character
  in
  let printStandaloneStructure ~(loc : Location.t) structure =
    structure
    |> Res_printer.print_implementation
         ~comments:(comments |> filterComments ~loc)
  in
  (structure, printExpr, printStructureItem, printStandaloneStructure)

let parseInterface ~filename =
  let {Res_driver.parsetree = structure; comments} =
    Res_driver.parsing_engine.parse_interface ~for_printer:false ~filename
  in
  let filterComments ~loc comments =
    (* Relevant comments in the range of the expression *)
    let filter comment =
      Loc.hasPos ~pos:(Loc.start (Res_comment.loc comment)) loc
    in
    comments |> List.filter filter
  in
  let printSignatureItem ~(range : Protocol.range)
      (item : Parsetree.signature_item) =
    let signature_item = [item] in
    signature_item
    |> Res_printer.print_interface
         ~comments:(comments |> filterComments ~loc:item.psig_loc)
    |> Utils.indent range.start.character
  in
  (structure, printSignatureItem)

module InlineVariable = struct
  (*
     Inline a value binding:
     - Replace all references (across files) with the binding's RHS, printing text verbatim
     - Remove the original definition (top-level and inside modules)
     - Only supports non‑destructured `let name = expr` (v1)
     - Naive: no scope/capture checks (v2 TODO)
  *)

  type found = {
    edit_item: Parsetree.structure_item;
    new_item_opt: Parsetree.structure_item option;
    target_vb: Parsetree.value_binding;
  }

  let equal_loc (a : Location.t) (b : Location.t) =
    a.loc_start.pos_cnum = b.loc_start.pos_cnum
    && a.loc_end.pos_cnum = b.loc_end.pos_cnum
    && a.loc_start.pos_lnum = b.loc_start.pos_lnum
    && a.loc_end.pos_lnum = b.loc_end.pos_lnum

  let pattern_matches_target (target_loc : Location.t) (pat : Parsetree.pattern)
      =
    match pat.ppat_desc with
    | Ppat_var {loc} -> equal_loc loc target_loc
    | _ -> false

  (*
     Find the enclosing structure_item that owns the target value binding.
     When found, return:
       - edit_item: structure_item to edit (in the current traversal scope)
       - new_item_opt: replacement item (None means delete the whole item)
       - target_vb: the matched value_binding (for printing its RHS)
  *)

  let rec find_value_binding_anywhere ~(target_loc : Location.t)
      (items : Parsetree.structure) : found option =
    let rec loop = function
      | [] -> None
      | (si : Parsetree.structure_item) :: tl -> (
        match si.pstr_desc with
        | Pstr_value (recFlag, bindings) -> (
          match
            bindings
            |> List.partition (fun (vb : Parsetree.value_binding) ->
                   pattern_matches_target target_loc vb.pvb_pat)
          with
          | [target_vb], others ->
            let new_item_opt =
              if others = [] then None
              else
                Some
                  (Ast_helper.Str.mk ~loc:si.pstr_loc
                     (Parsetree.Pstr_value (recFlag, others)))
            in
            Some {edit_item = si; new_item_opt; target_vb}
          | _ -> loop tl)
        | Pstr_module {pmb_expr = {pmod_desc = Pmod_structure inner}} -> (
          match find_value_binding_anywhere ~target_loc inner with
          | None -> loop tl
          | Some found_inner -> Some found_inner)
        | _ -> loop tl)
    in
    loop items

  let xform ~pos ~full ~codeActions ~debug =
    match References.getLocItem ~full ~pos ~debug with
    | None -> ()
    | Some locItem -> (
      let tip_is_value =
        match locItem.locType with
        | Typed (_, _, (LocalReference (_, Value) | Definition (_, Value)))
        | Typed (_, _, GlobalReference (_, _, Value)) ->
          true
        | _ -> false
      in
      if not tip_is_value then ()
      else
        let def_uri, def_loc =
          match References.definitionForLocItem ~full locItem with
          | Some (def_uri, def_loc) -> (def_uri, def_loc)
          | None -> (full.file.uri, locItem.loc)
        in
        (* Parse implementation that contains the definition to be able to print expr and structure items *)
        let def_path = Uri.toPath def_uri in
        let def_structure, printExpr, printStructureItem, _ =
          parseImplementation ~filename:def_path
        in
        match find_value_binding_anywhere ~target_loc:def_loc def_structure with
        | None -> ()
        | Some {edit_item; new_item_opt; target_vb} ->
          (* Collect edits for all references, across files *)
          let allRefs =
            let def_same (loc : Location.t) = equal_loc loc def_loc in
            let def_item_loc = edit_item.pstr_loc in
            let skip loc = def_same loc || Loc.isInside loc def_item_loc in
            References.allReferencesForLocItem ~full locItem
            |> Utils.filterMap (function
                 | {References.uri; locOpt = Some loc} ->
                   if skip loc then None else Some (uri, loc)
                 | _ -> None)
          in
          let module StringMap = Misc.StringMap in
          let editsByUri = ref StringMap.empty in
          let textByUri = ref StringMap.empty in
          let get_lines_for_uri (uri : Uri.t) =
            let uriStr = Uri.toString uri in
            match StringMap.find_opt uriStr !textByUri with
            | Some lines -> lines
            | None ->
              let path = Uri.toPath uri in
              let text =
                match Files.readFile path with
                | Some t -> t
                | None -> ""
              in
              let lines = text |> String.split_on_char '\n' |> Array.of_list in
              textByUri := StringMap.add uriStr lines !textByUri;
              lines
          in
          let is_ident_char c =
            (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c >= '0' && c <= '9')
            || c = '_'
          in
          let expand_qualified_range ~(uri : Uri.t) (range : Protocol.range) =
            let line = range.start.line in
            let col = range.start.character in
            let lines = get_lines_for_uri uri in
            if line < Array.length lines then
              let l = lines.(line) in
              if col > 0 && String.length l >= col && l.[col - 1] = '.' then
                let rec back i =
                  if i <= 0 then 0
                  else
                    let c = l.[i - 1] in
                    if c = '.' || is_ident_char c then back (i - 1) else i
                in
                let newCol = back col in
                {range with start = {range.start with character = newCol}}
              else range
            else range
          in
          let add_edit uri range newText =
            let uriStr = Uri.toString uri in
            let textEdit = {Protocol.range; newText} in
            let prevEdits =
              match StringMap.find_opt uriStr !editsByUri with
              | None -> []
              | Some l -> l
            in
            editsByUri :=
              StringMap.add uriStr (textEdit :: prevEdits) !editsByUri
          in
          (* For each reference, replace identifier with the defining expression text. *)
          allRefs
          |> List.iter (fun (uri, loc) ->
                 let baseRange = Utils.cmtLocToRange loc in
                 let range = expand_qualified_range ~uri baseRange in
                 let newText =
                   (* Indentation is computed from the reference position *)
                   printExpr ~range target_vb.pvb_expr
                 in
                 add_edit uri range newText);

          (* Also remove the defining binding in its file *)
          let def_edit_opt =
            let range = Loc.rangeOfLoc edit_item.pstr_loc in
            let newText =
              match new_item_opt with
              | None -> ""
              | Some new_si -> printStructureItem ~range new_si
            in
            Some (def_uri, range, newText)
          in
          let documentChanges =
            (* Accumulate TextDocumentEdits per uri, including def file edit. *)
            let open Protocol in
            (* Insert def edit first for determinism *)
            let edits_list = ref [] in
            let def_uri_str_opt =
              match def_edit_opt with
              | None -> None
              | Some (def_u, def_range, def_newText) ->
                let def_uri_str = Uri.toString def_u in
                edits_list :=
                  [
                    TextDocumentEdit
                      {
                        textDocument = {uri = def_uri_str; version = None};
                        edits = [{range = def_range; newText = def_newText}];
                      };
                  ];
                Some def_uri_str
            in
            StringMap.iter
              (fun uriStr edits ->
                (* Do not add a duplicate edit for the def file reference if present; merge instead *)
                if Some uriStr = def_uri_str_opt then
                  match !edits_list with
                  | TextDocumentEdit tde :: rest
                    when tde.textDocument.uri = uriStr ->
                    let tde' =
                      {tde with edits = List.rev_append edits tde.edits}
                    in
                    edits_list := TextDocumentEdit tde' :: rest
                  | _ ->
                    edits_list :=
                      TextDocumentEdit
                        {
                          textDocument = {uri = uriStr; version = None};
                          edits = List.rev edits;
                        }
                      :: !edits_list
                else
                  edits_list :=
                    TextDocumentEdit
                      {
                        textDocument = {uri = uriStr; version = None};
                        edits = List.rev edits;
                      }
                    :: !edits_list)
              !editsByUri;
            List.rev !edits_list
          in
          let codeAction =
            CodeActions.makeWithDocumentChanges ~title:"Inline variable"
              ~kind:RefactorRewrite ~documentChanges
          in
          codeActions := codeAction :: !codeActions)
end

let extractCodeActions ~path ~startPos ~endPos ~currentFile ~debug =
  let pos = startPos in
  let codeActions = ref [] in
  match Files.classifySourceFile currentFile with
  | Res ->
    let structure, printExpr, printStructureItem, printStandaloneStructure =
      parseImplementation ~filename:currentFile
    in
    IfThenElse.xform ~pos ~codeActions ~printExpr ~path structure;
    ModuleToFile.xform ~pos ~codeActions ~path ~printStandaloneStructure
      structure;
    AddBracesToFn.xform ~pos ~codeActions ~path ~printStructureItem structure;
    AddDocTemplate.Implementation.xform ~pos ~codeActions ~path
      ~printStructureItem ~structure;

    (* This Code Action needs type info *)
    let () =
      match Cmt.loadFullCmtFromPath ~path with
      | Some full ->
        AddTypeAnnotation.xform ~path ~pos ~full ~structure ~codeActions ~debug;
        InlineVariable.xform ~pos ~full ~codeActions ~debug;
        ExpandCatchAllForVariants.xform ~path ~pos ~full ~structure ~codeActions
          ~currentFile ~debug;
        ExhaustiveSwitch.xform ~printExpr ~path
          ~pos:
            (if startPos = endPos then Single startPos
             else Range (startPos, endPos))
          ~full ~structure ~codeActions ~debug ~currentFile
      | None -> ()
    in

    !codeActions
  | Resi ->
    let signature, printSignatureItem = parseInterface ~filename:currentFile in
    AddDocTemplate.Interface.xform ~pos ~codeActions ~path ~signature
      ~printSignatureItem;
    !codeActions
  | Other -> []
