open SharedTypes
type cursorAtArg = Unlabelled of int | Labelled of string

(* Produces the doc string shown below the signature help for each parameter. *)
let docsForLabel typeExpr ~file ~package ~supportsMarkdownLinks =
  let types = Hover.findRelevantTypesFromType ~file ~package typeExpr in
  let typeNames = types |> List.map (fun {Hover.name} -> name) in
  let typeDefinitions =
    types
    |> List.map (fun {Hover.decl; name; env; loc; path} ->
           let linkToTypeDefinitionStr =
             if supportsMarkdownLinks then
               Markdown.goToDefinitionText ~env ~pos:loc.Warnings.loc_start
             else ""
           in
           (* Since printing the whole name via its path can get quite long, and
              we're short on space for the signature help, we'll only print the
              fully "qualified" type name if we must (ie if several types we're
              displaying have the same name). *)
           let multipleTypesHaveThisName =
             typeNames
             |> List.filter (fun typeName -> typeName = name)
             |> List.length > 1
           in
           let typeName =
             if multipleTypesHaveThisName then
               path |> SharedTypes.pathIdentToString
             else name
           in
           Markdown.codeBlock
             (Shared.declToString ~printNameAsIs:true typeName decl)
           ^ linkToTypeDefinitionStr)
  in
  typeDefinitions |> String.concat "\n"

let findFunctionType ~currentFile ~debug ~path ~pos =
  (* Start by looking at the typed info at the loc of the fn *)
  match Cmt.loadFullCmtFromPath ~path with
  | None -> None
  | Some full -> (
    let {file; package} = full in
    let env = QueryEnv.fromFile file in
    let fnFromLocItem =
      match References.getLocItem ~full ~pos ~debug:false with
      | Some {locType = Typed (_, typeExpr, locKind)} -> (
        let docstring =
          match References.definedForLoc ~file ~package locKind with
          | None -> []
          | Some (docstring, _) -> docstring
        in
        if Debug.verbose () then
          Printf.printf "[sig_help_fn] Found loc item: %s.\n"
            (Shared.typeToString typeExpr);
        match
          TypeUtils.extractFunctionType2 ~env ~package:full.package typeExpr
        with
        | args, _tRet, _ when args <> [] ->
          Some (args, docstring, typeExpr, package, env, file)
        | _ -> None)
      | None ->
        if Debug.verbose () then
          Printf.printf "[sig_help_fn] Found no loc item.\n";
        None
      | Some _ ->
        if Debug.verbose () then
          Printf.printf
            "[sig_help_fn] Found loc item, but not what was expected.\n";
        None
    in
    match fnFromLocItem with
    | Some fnFromLocItem -> Some fnFromLocItem
    | None -> (
      (* If nothing was found there, try using the unsaved completion engine *)
      let completables =
        let textOpt = Files.readFile currentFile in
        match textOpt with
        | None | Some "" -> None
        | Some text -> (
          (* Leverage the completion functionality to pull out the type of the identifier doing the function application.
             This lets us leverage all of the smart work done in completions to find the correct type in many cases even
             for files not saved yet. *)
          match
            CompletionFrontEnd.completionWithParser ~debug ~path ~posCursor:pos
              ~currentFile ~text
          with
          | None -> None
          | Some (completable, scope) ->
            Some
              ( completable
                |> CompletionBackEnd.processCompletable ~debug ~full ~pos ~scope
                     ~env ~forHover:true,
                env,
                package,
                file ))
      in
      match completables with
      | Some ({kind = Value type_expr; docstring} :: _, env, package, file) ->
        let args, _, _ =
          TypeUtils.extractFunctionType2 type_expr ~env ~package
        in
        Some (args, docstring, type_expr, package, env, file)
      | _ -> None))

(* Extracts all parameters from a parsed function signature *)
let extractParameters ~signature ~typeStrForParser ~labelPrefixLen =
  match signature with
  | [{Parsetree.psig_desc = Psig_value {pval_type = expr}}]
    when match expr.ptyp_desc with
         | Ptyp_arrow _ -> true
         | _ -> false ->
    let rec extractParams expr params =
      match expr with
      | {
       (* Gotcha: functions with multiple arugments are modelled as a series of single argument functions. *)
       Parsetree.ptyp_desc =
         Ptyp_arrow
           {lbl = argumentLabel; arg = argumentTypeExpr; ret = nextFunctionExpr};
       ptyp_loc;
      } ->
        let startOffset =
          ptyp_loc |> Loc.start
          |> Pos.positionToOffset typeStrForParser
          |> Option.get
        in
        let endOffset =
          argumentTypeExpr.ptyp_loc |> Loc.end_
          |> Pos.positionToOffset typeStrForParser
          |> Option.get
        in
        (* The AST locations does not account for "=?" of optional arguments, so add that to the offset here if needed. *)
        let endOffset =
          match argumentLabel with
          | Asttypes.Optional _ -> endOffset + 2
          | _ -> endOffset
        in
        extractParams nextFunctionExpr
          (params
          @ [
              ( argumentLabel,
                (* Remove the label prefix offset here, since we're not showing
                   that to the end user. *)
                startOffset - labelPrefixLen,
                endOffset - labelPrefixLen );
            ])
      | _ -> params
    in
    extractParams expr []
  | _ -> []

(* Finds what parameter is active, if any *)
let findActiveParameter ~argAtCursor ~args =
  match argAtCursor with
  | None -> (
    (* If a function only has one, unlabelled argument, we can safely assume that's active whenever we're in the signature help for that function,
       even if we technically didn't find anything at the cursor (which we don't for empty expressions). *)
    match args with
    | [(Asttypes.Noloc.Nolabel, _)] -> Some 0
    | _ -> None)
  | Some (Unlabelled unlabelledArgumentIndex) ->
    let index = ref 0 in
    args
    |> List.find_map (fun (label, _) ->
           match label with
           | Asttypes.Noloc.Nolabel when !index = unlabelledArgumentIndex ->
             Some !index
           | _ ->
             index := !index + 1;
             None)
  | Some (Labelled name) ->
    let index = ref 0 in
    args
    |> List.find_map (fun (label, _) ->
           match label with
           | (Asttypes.Noloc.Labelled labelName | Optional labelName)
             when labelName = name ->
             Some !index
           | _ ->
             index := !index + 1;
             None)

type constructorInfo = {
  docstring: string list;
  name: string;
  args: constructorArgs;
}

let findConstructorArgs ~full ~env ~constructorName loc =
  match
    References.getLocItem ~debug:false ~full
      ~pos:(Pos.ofLexing loc.Location.loc_end)
  with
  | None -> None
  | Some {locType = Typed (_, typExpr, _)} -> (
    match TypeUtils.extractType ~env ~package:full.package typExpr with
    | Some ((Toption (_, TypeExpr t) as extractedType), _) -> (
      match constructorName with
      | "Some" ->
        Some
          {
            name = "Some";
            docstring =
              [
                Markdown.codeBlock
                  (TypeUtils.extractedTypeToString extractedType);
              ];
            args = Args [(t, Location.none)];
          }
      | _ -> None)
    | Some ((Tresult {okType; errorType} as extractedType), _) -> (
      match constructorName with
      | "Ok" ->
        Some
          {
            name = "Ok";
            docstring =
              [
                Markdown.codeBlock
                  (TypeUtils.extractedTypeToString extractedType);
              ];
            args = Args [(okType, Location.none)];
          }
      | "Error" ->
        Some
          {
            name = "Error";
            docstring =
              [
                Markdown.codeBlock
                  (TypeUtils.extractedTypeToString extractedType);
              ];
            args = Args [(errorType, Location.none)];
          }
      | _ -> None)
    | Some (Tvariant {constructors}, _) ->
      constructors
      |> List.find_opt (fun (c : Constructor.t) ->
             c.cname.txt = constructorName)
      |> Option.map (fun (c : Constructor.t) ->
             {docstring = c.docstring; name = c.cname.txt; args = c.args})
    | _ -> None)
  | _ -> None

let signatureHelp ~path ~pos ~currentFile ~debug ~allowForConstructorPayloads =
  let textOpt = Files.readFile currentFile in
  match textOpt with
  | None | Some "" -> None
  | Some text -> (
    match Pos.positionToOffset text pos with
    | None -> None
    | Some offset -> (
      let posBeforeCursor = Pos.posBeforeCursor pos in
      let offsetNoWhite = Utils.skipWhite text (offset - 1) in
      let firstCharBeforeCursorNoWhite =
        if offsetNoWhite < String.length text && offsetNoWhite >= 0 then
          Some text.[offsetNoWhite]
        else None
      in
      let locHasCursor loc =
        loc |> CursorPosition.locHasCursor ~pos:posBeforeCursor
      in
      let supportsMarkdownLinks = true in
      let result = ref None in
      let printThing thg =
        match thg with
        | `ConstructorExpr _ -> "Constructor(expr)"
        | `ConstructorPat _ -> "Constructor(pat)"
        | `FunctionCall _ -> "FunctionCall"
      in
      let setResult (loc, thing) =
        match (thing, allowForConstructorPayloads) with
        | (`ConstructorExpr _ | `ConstructorPat _), false -> ()
        | _ -> (
          match !result with
          | None ->
            if Debug.verbose () then
              Printf.printf "[sig_help_result] Setting because had none\n";
            result := Some (loc, thing)
          | Some (currentLoc, currentThing)
            when Pos.ofLexing loc.Location.loc_start
                 > Pos.ofLexing currentLoc.Location.loc_start ->
            result := Some (loc, thing);

            if Debug.verbose () then
              Printf.printf
                "[sig_help_result] Setting because loc of %s > then existing \
                 of %s\n"
                (printThing thing) (printThing currentThing)
          | Some (_, currentThing) ->
            if Debug.verbose () then
              Printf.printf
                "[sig_help_result] Doing nothing because loc of %s < then \
                 existing of %s\n"
                (printThing thing) (printThing currentThing))
      in
      let searchForArgWithCursor ~isPipeExpr ~args =
        let extractedArgs = extractExpApplyArgs ~args in
        let argAtCursor =
          let firstArgIndex = if isPipeExpr then 1 else 0 in
          let unlabelledArgCount = ref firstArgIndex in
          let lastUnlabelledArgBeforeCursor = ref firstArgIndex in
          let argAtCursor_ =
            extractedArgs
            |> List.find_map (fun arg ->
                   match arg.label with
                   | None ->
                     let currentUnlabelledArgCount = !unlabelledArgCount in
                     unlabelledArgCount := currentUnlabelledArgCount + 1;
                     (* An argument without a label is just the expression, so we can use that. *)
                     if locHasCursor arg.exp.pexp_loc then
                       Some (Unlabelled currentUnlabelledArgCount)
                     else (
                       (* If this unlabelled arg doesn't have the cursor, record
                          it as the last seen unlabelled arg before the
                          cursor.*)
                       if posBeforeCursor >= (arg.exp.pexp_loc |> Loc.start)
                       then
                         lastUnlabelledArgBeforeCursor :=
                           currentUnlabelledArgCount;
                       None)
                   | Some {name; posStart; posEnd} -> (
                     (* Check for the label identifier itself having the cursor *)
                     match
                       pos |> CursorPosition.classifyPositions ~posStart ~posEnd
                     with
                     | HasCursor -> Some (Labelled name)
                     | NoCursor | EmptyLoc -> (
                       (* If we're not in the label, check the exp. Either the exp
                          exists and has the cursor. Or the exp is a parser recovery
                          node, in which case we assume that the parser recovery
                          indicates that the cursor was here. *)
                       match
                         ( arg.exp.pexp_desc,
                           arg.exp.pexp_loc
                           |> CursorPosition.classifyLoc ~pos:posBeforeCursor )
                       with
                       | Pexp_extension ({txt = "rescript.exprhole"}, _), _
                       | _, HasCursor ->
                         Some (Labelled name)
                       | _ -> None)))
          in

          match argAtCursor_ with
          | None ->
            Some
              (Unlabelled
                 (!lastUnlabelledArgBeforeCursor
                 +
                 if firstCharBeforeCursorNoWhite = Some ',' then 1
                   (* If we found no argument with the cursor, we might still be
                      able to complete for an unlabelled argument, if the char
                      before the cursor is ',', like: `someFn(123, <com>)`
                      complete for argument 2, or: `someFn(123, <com>, true)`
                      complete for argument 2 as well. Adding 1 here accounts
                      for the comma telling us that the users intent is to fill
                      in the next argument. *)
                 else 0))
          | v -> v
        in
        (argAtCursor, extractedArgs)
      in
      let expr (iterator : Ast_iterator.iterator) (expr : Parsetree.expression)
          =
        (match expr with
        (* Handle pipes, like someVar->someFunc(... *)
        | {
         pexp_desc =
           Pexp_apply
             {
               funct = {pexp_desc = Pexp_ident {txt = Lident "->"}};
               args =
                 [
                   _;
                   ( _,
                     {
                       pexp_desc =
                         Pexp_apply
                           {funct = {pexp_desc = Pexp_ident _} as exp; args};
                       pexp_loc;
                     } );
                 ];
             };
        }
          when locHasCursor pexp_loc ->
          let argAtCursor, extractedArgs =
            searchForArgWithCursor ~isPipeExpr:true ~args
          in
          setResult
            (exp.pexp_loc, `FunctionCall (argAtCursor, exp, extractedArgs))
        (* Look for applying idents, like someIdent(...) *)
        | {
         pexp_desc =
           Pexp_apply {funct = {pexp_desc = Pexp_ident _} as exp; args};
         pexp_loc;
        }
          when locHasCursor pexp_loc ->
          let argAtCursor, extractedArgs =
            searchForArgWithCursor ~isPipeExpr:false ~args
          in
          setResult
            (exp.pexp_loc, `FunctionCall (argAtCursor, exp, extractedArgs))
        | {pexp_desc = Pexp_construct (lid, Some payloadExp); pexp_loc}
          when locHasCursor payloadExp.pexp_loc
               || CompletionExpressions.isExprHole payloadExp
                  && locHasCursor pexp_loc ->
          (* Constructor payloads *)
          setResult (lid.loc, `ConstructorExpr (lid, payloadExp))
        | _ -> ());
        Ast_iterator.default_iterator.expr iterator expr
      in
      let pat (iterator : Ast_iterator.iterator) (pat : Parsetree.pattern) =
        (match pat with
        | {ppat_desc = Ppat_construct (lid, Some payloadPat)}
          when locHasCursor payloadPat.ppat_loc ->
          (* Constructor payloads *)
          setResult (lid.loc, `ConstructorPat (lid, payloadPat))
        | _ -> ());
        Ast_iterator.default_iterator.pat iterator pat
      in
      let iterator = {Ast_iterator.default_iterator with expr; pat} in
      let parser =
        Res_driver.parsing_engine.parse_implementation ~for_printer:false
      in
      let {Res_driver.parsetree = structure} = parser ~filename:currentFile in
      iterator.structure iterator structure |> ignore;
      (* Handle function application, if found *)
      match !result with
      | Some (_, `FunctionCall (argAtCursor, exp, _extractedArgs)) -> (
        (* Not looking for the cursor position after this, but rather the target function expression's loc. *)
        let pos = exp.pexp_loc |> Loc.end_ in
        match findFunctionType ~currentFile ~debug ~path ~pos with
        | Some (args, docstring, type_expr, package, _env, file) ->
          if debug then
            Printf.printf "argAtCursor: %s\n"
              (match argAtCursor with
              | None -> "none"
              | Some (Labelled name) -> "~" ^ name
              | Some (Unlabelled index) ->
                "unlabelled<" ^ string_of_int index ^ ">");

          (* The LS protocol wants us to send both the full type signature (label) that the end user sees as the signature help, and all parameters in that label
             in the form of a list of start/end character offsets. We leverage the parser to figure the offsets out by parsing the label, and extract the
             offsets from the parser. *)

          (* A full let binding with the type text is needed for the parser to be able to parse it.  *)
          let labelPrefix = "let fn: " in
          let labelPrefixLen = String.length labelPrefix in
          let fnTypeStr = Shared.typeToString type_expr in
          let typeStrForParser = labelPrefix ^ fnTypeStr in
          let {Res_driver.parsetree = signature} =
            Res_driver.parse_interface_from_source ~for_printer:false
              ~display_filename:"<missing-file>" ~source:typeStrForParser
          in

          let parameters =
            extractParameters ~signature ~typeStrForParser ~labelPrefixLen
          in
          if debug then
            Printf.printf "extracted params: \n%s\n"
              (parameters
              |> List.map (fun (_, start, end_) ->
                     String.sub fnTypeStr start (end_ - start))
              |> list);

          (* Figure out the active parameter *)
          let activeParameter = findActiveParameter ~argAtCursor ~args in

          let paramUnlabelledArgCount = ref 0 in
          Some
            {
              Protocol.signatures =
                [
                  {
                    label = fnTypeStr;
                    parameters =
                      parameters
                      |> List.map (fun (argLabel, start, end_) ->
                             let argLabel = Asttypes.to_noloc argLabel in
                             let paramArgCount = !paramUnlabelledArgCount in
                             paramUnlabelledArgCount := paramArgCount + 1;
                             let unlabelledArgCount = ref 0 in
                             {
                               Protocol.label = (start, end_);
                               documentation =
                                 (match
                                    args
                                    |> List.find_opt (fun (lbl, _) ->
                                           let argCount = !unlabelledArgCount in
                                           unlabelledArgCount := argCount + 1;
                                           match (lbl, argLabel) with
                                           | ( Asttypes.Noloc.Optional l1,
                                               Asttypes.Noloc.Optional l2 )
                                             when l1 = l2 ->
                                             true
                                           | Labelled l1, Labelled l2
                                             when l1 = l2 ->
                                             true
                                           | Nolabel, Nolabel
                                             when paramArgCount = argCount ->
                                             true
                                           | _ -> false)
                                  with
                                 | None ->
                                   {Protocol.kind = "markdown"; value = ""}
                                 | Some (_, labelTypExpr) ->
                                   {
                                     Protocol.kind = "markdown";
                                     value =
                                       docsForLabel ~supportsMarkdownLinks ~file
                                         ~package labelTypExpr;
                                   });
                             });
                    documentation =
                      (match List.nth_opt docstring 0 with
                      | None -> None
                      | Some docs ->
                        Some {Protocol.kind = "markdown"; value = docs});
                  };
                ];
              activeSignature = Some 0;
              activeParameter =
                (match activeParameter with
                | None -> Some (-1)
                | activeParameter -> activeParameter);
            }
        | _ -> None)
      | Some (_, ((`ConstructorExpr (lid, _) | `ConstructorPat (lid, _)) as cs))
        -> (
        if Debug.verbose () then
          Printf.printf "[signature_help] Found constructor!\n";
        match Cmt.loadFullCmtFromPath ~path with
        | None ->
          if Debug.verbose () then
            Printf.printf "[signature_help] Could not load cmt\n";
          None
        | Some full -> (
          let {file} = full in
          let env = QueryEnv.fromFile file in
          let constructorName = Longident.last lid.txt in
          match
            findConstructorArgs ~full ~env ~constructorName
              {lid.loc with loc_start = lid.loc.loc_end}
          with
          | None ->
            if Debug.verbose () then
              Printf.printf "[signature_help] Did not find constructor '%s'\n"
                constructorName;
            None
          | Some constructor ->
            let argParts =
              match constructor.args with
              | Args [] -> None
              | InlineRecord fields ->
                let offset = ref 0 in
                Some
                  (`InlineRecord
                     (fields
                     |> List.map (fun (field : field) ->
                            let startOffset = !offset in
                            let argText =
                              Printf.sprintf "%s%s: %s" field.fname.txt
                                (if field.optional then "?" else "")
                                (Shared.typeToString
                                   (if field.optional then
                                      Utils.unwrapIfOption field.typ
                                    else field.typ))
                            in
                            let endOffset =
                              startOffset + String.length argText
                            in
                            offset := endOffset + String.length ", ";
                            (argText, field, (startOffset, endOffset)))))
              | Args [(typ, _)] ->
                Some
                  (`SingleArg
                     ( typ |> Shared.typeToString,
                       docsForLabel ~file:full.file ~package:full.package
                         ~supportsMarkdownLinks typ ))
              | Args args ->
                let offset = ref 0 in
                Some
                  (`TupleArg
                     (args
                     |> List.map (fun (typ, _) ->
                            let startOffset = !offset in
                            let argText = typ |> Shared.typeToString in
                            let endOffset =
                              startOffset + String.length argText
                            in
                            offset := endOffset + String.length ", ";
                            ( argText,
                              docsForLabel ~file:full.file ~package:full.package
                                ~supportsMarkdownLinks typ,
                              (startOffset, endOffset) ))))
            in
            let label =
              constructor.name ^ "("
              ^ (match argParts with
                | None -> ""
                | Some (`InlineRecord fields) ->
                  "{"
                  ^ (fields
                    |> List.map (fun (argText, _, _) -> argText)
                    |> String.concat ", ")
                  ^ "}"
                | Some (`SingleArg (arg, _)) -> arg
                | Some (`TupleArg items) ->
                  items
                  |> List.map (fun (argText, _, _) -> argText)
                  |> String.concat ", ")
              ^ ")"
            in
            let activeParameter =
              match cs with
              | `ConstructorExpr (_, {pexp_desc = Pexp_tuple items}) -> (
                let idx = ref 0 in
                let tupleItemWithCursor =
                  items
                  |> List.find_map (fun (item : Parsetree.expression) ->
                         let currentIndex = !idx in
                         idx := currentIndex + 1;
                         if locHasCursor item.pexp_loc then Some currentIndex
                         else None)
                in
                match tupleItemWithCursor with
                | None -> -1
                | Some i -> i)
              | `ConstructorExpr (_, {pexp_desc = Pexp_record (fields, _)}) -> (
                let fieldNameWithCursor =
                  fields
                  |> List.find_map
                       (fun
                         (({loc; txt}, expr, _) :
                           Longident.t Location.loc
                           * Parsetree.expression
                           * bool)
                       ->
                         if
                           posBeforeCursor >= Pos.ofLexing loc.loc_start
                           && posBeforeCursor
                              <= Pos.ofLexing expr.pexp_loc.loc_end
                         then Some (Longident.last txt)
                         else None)
                in
                match (fieldNameWithCursor, argParts) with
                | Some fieldName, Some (`InlineRecord fields) ->
                  let idx = ref 0 in
                  let fieldIndex = ref (-1) in
                  fields
                  |> List.iter (fun (_, field, _) ->
                         idx := !idx + 1;
                         let currentIndex = !idx in
                         if fieldName = field.fname.txt then
                           fieldIndex := currentIndex
                         else ());
                  !fieldIndex
                | _ -> -1)
              | `ConstructorExpr (_, expr) when locHasCursor expr.pexp_loc -> 0
              | `ConstructorPat (_, {ppat_desc = Ppat_tuple items}) -> (
                let idx = ref 0 in
                let tupleItemWithCursor =
                  items
                  |> List.find_map (fun (item : Parsetree.pattern) ->
                         let currentIndex = !idx in
                         idx := currentIndex + 1;
                         if locHasCursor item.ppat_loc then Some currentIndex
                         else None)
                in
                match tupleItemWithCursor with
                | None -> -1
                | Some i -> i)
              | `ConstructorPat (_, {ppat_desc = Ppat_record (fields, _)}) -> (
                let fieldNameWithCursor =
                  fields
                  |> List.find_map
                       (fun
                         (({loc; txt}, pat, _) :
                           Longident.t Location.loc * Parsetree.pattern * bool)
                       ->
                         if
                           posBeforeCursor >= Pos.ofLexing loc.loc_start
                           && posBeforeCursor
                              <= Pos.ofLexing pat.ppat_loc.loc_end
                         then Some (Longident.last txt)
                         else None)
                in
                match (fieldNameWithCursor, argParts) with
                | Some fieldName, Some (`InlineRecord fields) ->
                  let idx = ref 0 in
                  let fieldIndex = ref (-1) in
                  fields
                  |> List.iter (fun (_, field, _) ->
                         idx := !idx + 1;
                         let currentIndex = !idx in
                         if fieldName = field.fname.txt then
                           fieldIndex := currentIndex
                         else ());
                  !fieldIndex
                | _ -> -1)
              | `ConstructorPat (_, pat) when locHasCursor pat.ppat_loc -> 0
              | _ -> -1
            in

            let constructorNameLength = String.length constructor.name in
            let params =
              match argParts with
              | None -> []
              | Some (`SingleArg (_, docstring)) ->
                [
                  {
                    Protocol.label =
                      (constructorNameLength + 1, String.length label - 1);
                    documentation =
                      {Protocol.kind = "markdown"; value = docstring};
                  };
                ]
              | Some (`InlineRecord fields) ->
                (* Account for leading '({' *)
                let baseOffset = constructorNameLength + 2 in
                {
                  Protocol.label = (0, 0);
                  documentation = {Protocol.kind = "markdown"; value = ""};
                }
                :: (fields
                   |> List.map (fun (_, (field : field), (start, end_)) ->
                          {
                            Protocol.label =
                              (baseOffset + start, baseOffset + end_);
                            documentation =
                              {
                                Protocol.kind = "markdown";
                                value = field.docstring |> String.concat "\n";
                              };
                          }))
              | Some (`TupleArg items) ->
                (* Account for leading '(' *)
                let baseOffset = constructorNameLength + 1 in
                items
                |> List.map (fun (_, docstring, (start, end_)) ->
                       {
                         Protocol.label = (baseOffset + start, baseOffset + end_);
                         documentation =
                           {Protocol.kind = "markdown"; value = docstring};
                       })
            in
            Some
              {
                Protocol.signatures =
                  [
                    {
                      label;
                      parameters = params;
                      documentation =
                        (match List.nth_opt constructor.docstring 0 with
                        | None -> None
                        | Some docs ->
                          Some {Protocol.kind = "markdown"; value = docs});
                    };
                  ];
                activeSignature = Some 0;
                activeParameter = Some activeParameter;
              }))
      | _ -> None))
