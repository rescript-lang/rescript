open DeadCommon

module Values = struct
  let valueBindingsTable =
    (Hashtbl.create 15 : (string, (Name.t, Exceptions.t) Hashtbl.t) Hashtbl.t)

  let currentFileTable = ref (Hashtbl.create 1)

  let add ~modulePath ~name exceptions =
    let path = (name |> Name.create) :: modulePath.ModulePath.path in
    Hashtbl.replace !currentFileTable (path |> DcePath.toName) exceptions

  let getFromModule ~moduleName ~modulePath (path_ : DcePath.t) =
    let name = path_ @ modulePath |> DcePath.toName in
    match
      Hashtbl.find_opt valueBindingsTable (String.capitalize_ascii moduleName)
    with
    | Some tbl -> Hashtbl.find_opt tbl name
    | None -> (
      match
        Hashtbl.find_opt valueBindingsTable
          (String.uncapitalize_ascii moduleName)
      with
      | Some tbl -> Hashtbl.find_opt tbl name
      | None -> None)

  let rec findLocal ~moduleName ~modulePath path =
    match path |> getFromModule ~moduleName ~modulePath with
    | Some exceptions -> Some exceptions
    | None -> (
      match modulePath with
      | [] -> None
      | _ :: restModulePath ->
        path |> findLocal ~moduleName ~modulePath:restModulePath)

  let findPath ~moduleName ~modulePath path =
    let findExternal ~externalModuleName ~pathRev =
      pathRev |> List.rev
      |> getFromModule
           ~moduleName:(externalModuleName |> Name.toString)
           ~modulePath:[]
    in
    match path |> findLocal ~moduleName ~modulePath with
    | None -> (
      (* Search in another file *)
      match path |> List.rev with
      | externalModuleName :: pathRev -> (
        match (findExternal ~externalModuleName ~pathRev, pathRev) with
        | (Some _ as found), _ -> found
        | None, externalModuleName2 :: pathRev2
          when !Cli.cmtCommand && pathRev2 <> [] ->
          (* Simplistic namespace resolution for dune namespace: skip the root of the path *)
          findExternal ~externalModuleName:externalModuleName2 ~pathRev:pathRev2
        | None, _ -> None)
      | [] -> None)
    | Some exceptions -> Some exceptions

  let newCmt ~moduleName =
    currentFileTable := Hashtbl.create 15;
    Hashtbl.replace valueBindingsTable moduleName !currentFileTable
end

module Event = struct
  type kind =
    | Catches of t list (* with | E => ... *)
    | Call of {callee: DcePath.t; modulePath: DcePath.t} (* foo() *)
    | DoesNotThrow of
        t list (* DoesNotThrow(events) where events come from an expression *)
    | Throws  (** throw E *)

  and t = {exceptions: Exceptions.t; kind: kind; loc: Location.t}

  let rec print ppf event =
    match event with
    | {kind = Call {callee; modulePath}; exceptions; loc} ->
      Format.fprintf ppf "%s Call(%s, modulePath:%s) %a@."
        (loc.loc_start |> Pos.toString)
        (callee |> DcePath.toString)
        (modulePath |> DcePath.toString)
        (Exceptions.pp ~exnTable:None)
        exceptions
    | {kind = DoesNotThrow nestedEvents; loc} ->
      Format.fprintf ppf "%s DoesNotThrow(%a)@."
        (loc.loc_start |> Pos.toString)
        (fun ppf () ->
          nestedEvents |> List.iter (fun e -> Format.fprintf ppf "%a " print e))
        ()
    | {kind = Throws; exceptions; loc} ->
      Format.fprintf ppf "%s throws %a@."
        (loc.loc_start |> Pos.toString)
        (Exceptions.pp ~exnTable:None)
        exceptions
    | {kind = Catches nestedEvents; exceptions; loc} ->
      Format.fprintf ppf "%s Catches exceptions:%a nestedEvents:%a@."
        (loc.loc_start |> Pos.toString)
        (Exceptions.pp ~exnTable:None)
        exceptions
        (fun ppf () ->
          nestedEvents |> List.iter (fun e -> Format.fprintf ppf "%a " print e))
        ()

  let combine ~config ~moduleName events =
    if config.DceConfig.cli.debug then (
      Log_.item "@.";
      Log_.item "Events combine: #events %d@." (events |> List.length));
    let exnTable = Hashtbl.create 1 in
    let extendExnTable exn loc =
      match Hashtbl.find_opt exnTable exn with
      | Some locSet -> Hashtbl.replace exnTable exn (LocSet.add loc locSet)
      | None -> Hashtbl.replace exnTable exn (LocSet.add loc LocSet.empty)
    in
    let shrinkExnTable exn loc =
      match Hashtbl.find_opt exnTable exn with
      | Some locSet -> Hashtbl.replace exnTable exn (LocSet.remove loc locSet)
      | None -> ()
    in
    let rec loop exnSet events =
      match events with
      | ({kind = Throws; exceptions; loc} as ev) :: rest ->
        if config.DceConfig.cli.debug then Log_.item "%a@." print ev;
        exceptions |> Exceptions.iter (fun exn -> extendExnTable exn loc);
        loop (Exceptions.union exnSet exceptions) rest
      | ({kind = Call {callee; modulePath}; loc} as ev) :: rest ->
        if config.DceConfig.cli.debug then Log_.item "%a@." print ev;
        let exceptions =
          match callee |> Values.findPath ~moduleName ~modulePath with
          | Some exceptions -> exceptions
          | _ -> (
            match ExnLib.find callee with
            | Some exceptions -> exceptions
            | None -> Exceptions.empty)
        in
        exceptions |> Exceptions.iter (fun exn -> extendExnTable exn loc);
        loop (Exceptions.union exnSet exceptions) rest
      | ({kind = DoesNotThrow nestedEvents; loc} as ev) :: rest ->
        if config.DceConfig.cli.debug then Log_.item "%a@." print ev;
        let nestedExceptions = loop Exceptions.empty nestedEvents in
        (if Exceptions.isEmpty nestedExceptions (* catch-all *) then
           let name =
             match nestedEvents with
             | {kind = Call {callee}} :: _ -> callee |> DcePath.toName
             | _ -> "expression" |> Name.create
           in
           Log_.warning ~loc
             (Issue.ExceptionAnalysis
                {
                  message =
                    Format.asprintf
                      "@{<info>%s@} does not throw and is annotated with \
                       redundant @doesNotThrow"
                      (name |> Name.toString);
                }));
        loop exnSet rest
      | ({kind = Catches nestedEvents; exceptions} as ev) :: rest ->
        if config.DceConfig.cli.debug then Log_.item "%a@." print ev;
        if Exceptions.isEmpty exceptions then loop exnSet rest
        else
          let nestedExceptions = loop Exceptions.empty nestedEvents in
          let newThrows = Exceptions.diff nestedExceptions exceptions in
          exceptions
          |> Exceptions.iter (fun exn ->
                 nestedEvents
                 |> List.iter (fun event -> shrinkExnTable exn event.loc));
          loop (Exceptions.union exnSet newThrows) rest
      | [] -> exnSet
    in
    let exnSet = loop Exceptions.empty events in
    (exnSet, exnTable)
end

module Checks = struct
  type check = {
    events: Event.t list;
    loc: Location.t;
    locFull: Location.t;
    moduleName: string;
    exnName: string;
    exceptions: Exceptions.t;
  }

  type t = check list

  let checks = (ref [] : t ref)

  let add ~events ~exceptions ~loc ?(locFull = loc) ~moduleName exnName =
    checks := {events; exceptions; loc; locFull; moduleName; exnName} :: !checks

  let doCheck ~config {events; exceptions; loc; locFull; moduleName; exnName} =
    let throwSet, exnTable = events |> Event.combine ~config ~moduleName in
    let missingAnnotations = Exceptions.diff throwSet exceptions in
    let redundantAnnotations = Exceptions.diff exceptions throwSet in
    (if not (Exceptions.isEmpty missingAnnotations) then
       let description =
         Issue.ExceptionAnalysisMissing
           {exnName; exnTable; throwSet; missingAnnotations; locFull}
       in
       Log_.warning ~loc description);
    if not (Exceptions.isEmpty redundantAnnotations) then
      Log_.warning ~loc
        (Issue.ExceptionAnalysis
           {
             message =
               (let throwsDescription ppf () =
                  if throwSet |> Exceptions.isEmpty then
                    Format.fprintf ppf "throws nothing"
                  else
                    Format.fprintf ppf "might throw %a"
                      (Exceptions.pp ~exnTable:(Some exnTable))
                      throwSet
                in
                Format.asprintf
                  "@{<info>%s@} %a and is annotated with redundant @throws(%a)"
                  exnName throwsDescription ()
                  (Exceptions.pp ~exnTable:None)
                  redundantAnnotations);
           })

  let doChecks ~config = !checks |> List.rev |> List.iter (doCheck ~config)
end

let traverseAst ~file () =
  let super = Tast_mapper.default in
  let currentId = ref "" in
  let currentEvents = ref [] in
  let exceptionsOfPatterns patterns =
    patterns
    |> List.fold_left
         (fun acc desc ->
           match desc with
           | Typedtree.Tpat_construct ({txt}, _, _) ->
             Exceptions.add (Exn.fromLid txt) acc
           | _ -> acc)
         Exceptions.empty
  in
  let iterExpr self e = self.Tast_mapper.expr self e |> ignore in
  let iterExprOpt self eo =
    match eo with
    | None -> ()
    | Some e -> e |> iterExpr self
  in
  let iterPat self p = self.Tast_mapper.pat self p |> ignore in
  let iterCases self cases =
    cases
    |> List.iter (fun case ->
           case.Typedtree.c_lhs |> iterPat self;
           case.c_guard |> iterExprOpt self;
           case.c_rhs |> iterExpr self)
  in
  let isThrow s = s = "Pervasives.raise" || s = "Pervasives.throw" in
  let throwArgs args =
    match args with
    | [(_, Some {Typedtree.exp_desc = Texp_construct ({txt}, _, _)})] ->
      [Exn.fromLid txt] |> Exceptions.fromList
    | [(_, Some {Typedtree.exp_desc = Texp_ident _})] ->
      [Exn.fromString "genericException"] |> Exceptions.fromList
    | _ -> [Exn.fromString "TODO_from_raise1"] |> Exceptions.fromList
  in
  let doesNotThrow attributes =
    attributes
    |> Annotation.getAttributePayload (function
         | "doesNotRaise" | "doesnotraise" | "DoesNoRaise" | "doesNotraise"
         | "doNotRaise" | "donotraise" | "DoNoRaise" | "doNotraise"
         | "doesNotThrow" | "doesnotthrow" | "DoesNoThrow" | "doesNotthrow"
         | "doNotThrow" | "donotthrow" | "DoNoThrow" | "doNotthrow" ->
           true
         | _ -> false)
    <> None
  in
  let expr ~(modulePath : ModulePath.t) (self : Tast_mapper.mapper)
      (expr : Typedtree.expression) =
    let loc = expr.exp_loc in
    let isDoesNoThrow = expr.exp_attributes |> doesNotThrow in
    let oldEvents = !currentEvents in
    if isDoesNoThrow then currentEvents := [];
    (match expr.exp_desc with
    | Texp_ident (callee_, _, _) ->
      let callee =
        callee_ |> DcePath.fromPathT |> ModulePath.resolveAlias modulePath
      in
      let calleeName = callee |> DcePath.toName in
      if calleeName |> Name.toString |> isThrow then
        Log_.warning ~loc
          (Issue.ExceptionAnalysis
             {
               message =
                 Format.asprintf
                   "@{<info>%s@} can be analyzed only if called directly"
                   (calleeName |> Name.toString);
             });
      currentEvents :=
        {
          Event.exceptions = Exceptions.empty;
          loc;
          kind = Call {callee; modulePath = modulePath.path};
        }
        :: !currentEvents
    | Texp_apply
        {
          funct = {exp_desc = Texp_ident (atat, _, _)};
          args = [(_lbl1, Some {exp_desc = Texp_ident (callee, _, _)}); arg];
        }
      when (* raise @@ Exn(...) *)
           atat |> Path.name = "Pervasives.@@" && callee |> Path.name |> isThrow
      ->
      let exceptions = [arg] |> throwArgs in
      currentEvents := {Event.exceptions; loc; kind = Throws} :: !currentEvents;
      arg |> snd |> iterExprOpt self
    | Texp_apply {funct = {exp_desc = Texp_ident (callee, _, _)} as e; args} ->
      let calleeName = Path.name callee in
      if calleeName |> isThrow then
        let exceptions = args |> throwArgs in
        currentEvents :=
          {Event.exceptions; loc; kind = Throws} :: !currentEvents
      else e |> iterExpr self;
      args |> List.iter (fun (_, eOpt) -> eOpt |> iterExprOpt self)
    | Texp_match (e, casesOk, casesExn, partial) ->
      let cases = casesOk @ casesExn in
      let exceptionPatterns =
        casesExn
        |> List.map (fun (case : Typedtree.case) -> case.c_lhs.pat_desc)
      in
      let exceptions = exceptionPatterns |> exceptionsOfPatterns in
      if exceptionPatterns <> [] then (
        let oldEvents = !currentEvents in
        currentEvents := [];
        e |> iterExpr self;
        currentEvents :=
          {Event.exceptions; loc; kind = Catches !currentEvents} :: oldEvents)
      else e |> iterExpr self;
      cases |> iterCases self;
      if partial = Partial then
        currentEvents :=
          {
            Event.exceptions = [Exn.matchFailure] |> Exceptions.fromList;
            loc;
            kind = Throws;
          }
          :: !currentEvents
    | Texp_try (e, cases) ->
      let exceptions =
        cases
        |> List.map (fun case -> case.Typedtree.c_lhs.pat_desc)
        |> exceptionsOfPatterns
      in
      let oldEvents = !currentEvents in
      currentEvents := [];
      e |> iterExpr self;
      currentEvents :=
        {Event.exceptions; loc; kind = Catches !currentEvents} :: oldEvents;
      cases |> iterCases self
    | _ -> super.expr self expr |> ignore);
    (if isDoesNoThrow then
       let nestedEvents = !currentEvents in
       currentEvents :=
         {
           Event.exceptions = Exceptions.empty;
           loc;
           kind = DoesNotThrow nestedEvents;
         }
         :: oldEvents);
    expr
  in
  let getExceptionsFromAnnotations attributes =
    let throwsAnnotationPayload =
      attributes
      |> Annotation.getAttributePayload (fun s ->
             s = "throws" || s = "throw" || s = "raises" || s = "raise")
    in
    let rec getExceptions payload =
      match payload with
      | Annotation.StringPayload s -> [Exn.fromString s] |> Exceptions.fromList
      | Annotation.ConstructPayload s when s <> "::" ->
        [Exn.fromString s] |> Exceptions.fromList
      | Annotation.IdentPayload s ->
        [Exn.fromString (s |> Longident.flatten |> String.concat ".")]
        |> Exceptions.fromList
      | Annotation.TuplePayload tuple ->
        tuple
        |> List.map (fun payload ->
               payload |> getExceptions |> Exceptions.toList)
        |> List.concat |> Exceptions.fromList
      | _ -> Exceptions.empty
    in
    match throwsAnnotationPayload with
    | None -> Exceptions.empty
    | Some payload -> payload |> getExceptions
  in
  let toplevelEval (self : Tast_mapper.mapper) (expr : Typedtree.expression)
      attributes =
    let oldId = !currentId in
    let oldEvents = !currentEvents in
    let name = "Toplevel expression" in
    currentId := name;
    currentEvents := [];
    let moduleName = file.FileContext.module_name in
    self.expr self expr |> ignore;
    Checks.add ~events:!currentEvents
      ~exceptions:(getExceptionsFromAnnotations attributes)
      ~loc:expr.exp_loc ~moduleName name;
    currentId := oldId;
    currentEvents := oldEvents
  in
  let value_binding ~(modulePath : ModulePath.t) (self : Tast_mapper.mapper)
      (vb : Typedtree.value_binding) =
    let oldId = !currentId in
    let oldEvents = !currentEvents in
    let isFunction =
      match vb.vb_expr.exp_desc with
      | Texp_function _ -> true
      | _ -> false
    in
    let isToplevel = !currentId = "" in
    let processBinding name =
      currentId := name;
      currentEvents := [];
      let exceptionsFromAnnotations =
        getExceptionsFromAnnotations vb.vb_attributes
      in
      exceptionsFromAnnotations |> Values.add ~modulePath ~name;
      let res = super.value_binding self vb in
      let moduleName = file.FileContext.module_name in
      let path = [name |> Name.create] in
      let exceptions =
        match
          path |> Values.findPath ~moduleName ~modulePath:modulePath.path
        with
        | Some exceptions -> exceptions
        | _ -> Exceptions.empty
      in
      Checks.add ~events:!currentEvents ~exceptions ~loc:vb.vb_pat.pat_loc
        ~locFull:vb.vb_loc ~moduleName name;
      currentId := oldId;
      currentEvents := oldEvents;
      res
    in
    match vb.vb_pat.pat_desc with
    | Tpat_any when isToplevel && not vb.vb_loc.loc_ghost -> processBinding "_"
    | Tpat_construct ({txt}, _, _)
      when isToplevel && (not vb.vb_loc.loc_ghost)
           && txt = Longident.Lident "()" ->
      processBinding "()"
    | Tpat_var (id, {loc = {loc_ghost}})
      when (isFunction || isToplevel) && (not loc_ghost)
           && not vb.vb_loc.loc_ghost ->
      processBinding (id |> Ident.name)
    | _ -> super.value_binding self vb
  in
  let make_mapper (modulePath : ModulePath.t) : Tast_mapper.mapper =
    let open Tast_mapper in
    {
      super with
      expr = expr ~modulePath;
      value_binding = value_binding ~modulePath;
    }
  in
  let rec process_module_expr (modulePath : ModulePath.t)
      (me : Typedtree.module_expr) =
    match me.mod_desc with
    | Tmod_structure structure -> process_structure modulePath structure
    | Tmod_constraint (me1, _mty, _mtc, _coercion) ->
      process_module_expr modulePath me1
    | Tmod_apply (me1, me2, _) ->
      process_module_expr modulePath me1;
      process_module_expr modulePath me2
    | _ ->
      let mapper = make_mapper modulePath in
      super.module_expr mapper me |> ignore
  and process_structure (modulePath : ModulePath.t)
      (structure : Typedtree.structure) =
    let rec loop (mp : ModulePath.t) (items : Typedtree.structure_item list) =
      match items with
      | [] -> ()
      | structureItem :: rest ->
        let mapper = make_mapper mp in
        let mp' =
          match structureItem.str_desc with
          | Tstr_eval (expr, attributes) ->
            toplevelEval mapper expr attributes;
            mp
          | Tstr_module {mb_id; mb_loc; mb_expr} -> (
            let name = mb_id |> Ident.name |> Name.create in
            let mp_inside = ModulePath.enterModule mp ~name ~loc:mb_loc in
            process_module_expr mp_inside mb_expr;
            match mb_expr.mod_desc with
            | Tmod_ident (path_, _lid) ->
              ModulePath.addAlias mp ~name ~path:(path_ |> DcePath.fromPathT)
            | _ -> mp)
          | Tstr_recmodule mbs ->
            (* Process each module in the recursive group in the current scope; aliases are collected in the current scope too. *)
            List.fold_left
              (fun acc {Typedtree.mb_id; mb_loc; mb_expr} ->
                let name = mb_id |> Ident.name |> Name.create in
                let mp_inside = ModulePath.enterModule acc ~name ~loc:mb_loc in
                process_module_expr mp_inside mb_expr;
                match mb_expr.mod_desc with
                | Tmod_ident (path_, _lid) ->
                  ModulePath.addAlias acc ~name
                    ~path:(path_ |> DcePath.fromPathT)
                | _ -> acc)
              mp mbs
          | _ ->
            super.structure_item mapper structureItem |> ignore;
            mp
        in
        loop mp' rest
    in
    loop modulePath structure.str_items
  in
  fun (structure : Typedtree.structure) ->
    process_structure ModulePath.initial structure

let processStructure ~file (structure : Typedtree.structure) =
  let process = traverseAst ~file () in
  process structure

let processCmt ~file (cmt_infos : Cmt_format.cmt_infos) =
  match cmt_infos.cmt_annots with
  | Interface _ -> ()
  | Implementation structure ->
    Values.newCmt ~moduleName:file.FileContext.module_name;
    structure |> processStructure ~file
  | _ -> ()
