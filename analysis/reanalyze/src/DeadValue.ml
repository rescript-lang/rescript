(* Adapted from https://github.com/LexiFi/dead_code_analyzer *)

open DeadCommon

let checkAnyValueBindingWithNoSideEffects ~config ~decls ~file
    ~(modulePath : ModulePath.t)
    ({vb_pat = {pat_desc}; vb_expr = expr; vb_loc = loc} :
      Typedtree.value_binding) =
  match pat_desc with
  | Tpat_any when (not (SideEffects.checkExpr expr)) && not loc.loc_ghost ->
    let name = "_" |> Name.create ~isInterface:false in
    let path = modulePath.path @ [FileContext.module_name_tagged file] in
    name
    |> addValueDeclaration ~config ~decls ~file ~path ~loc
         ~moduleLoc:modulePath.loc ~sideEffects:false
  | _ -> ()

let collectValueBinding ~config ~decls ~file ~(current_binding : Location.t)
    ~(modulePath : ModulePath.t) (vb : Typedtree.value_binding) =
  let oldLastBinding = current_binding in
  checkAnyValueBindingWithNoSideEffects ~config ~decls ~file ~modulePath vb;
  let loc =
    match vb.vb_pat.pat_desc with
    | Tpat_var (id, {loc = {loc_start; loc_ghost} as loc})
    | Tpat_alias
        ({pat_desc = Tpat_any}, id, {loc = {loc_start; loc_ghost} as loc})
      when (not loc_ghost) && not vb.vb_loc.loc_ghost ->
      let name = Ident.name id |> Name.create ~isInterface:false in
      let optionalArgs =
        vb.vb_expr.exp_type |> DeadOptionalArgs.fromTypeExpr
        |> OptionalArgs.fromList
      in
      let exists =
        match Declarations.find_opt_builder decls loc_start with
        | Some {declKind = Value r} ->
          r.optionalArgs <- optionalArgs;
          true
        | _ -> false
      in
      let path = modulePath.path @ [FileContext.module_name_tagged file] in
      let isFirstClassModule =
        match vb.vb_expr.exp_type.desc with
        | Tpackage _ -> true
        | _ -> false
      in
      (if (not exists) && not isFirstClassModule then
         (* This is never toplevel currently *)
         let isToplevel = oldLastBinding = Location.none in
         let sideEffects = SideEffects.checkExpr vb.vb_expr in
         name
         |> addValueDeclaration ~config ~decls ~file ~isToplevel ~loc
              ~moduleLoc:modulePath.loc ~optionalArgs ~path ~sideEffects);
      (match Declarations.find_opt_builder decls loc_start with
      | None -> ()
      | Some decl ->
        (* Value bindings contain the correct location for the entire declaration: update final position.
           The previous value was taken from the signature, which only has positions for the id. *)
        let declKind =
          match decl.declKind with
          | Value vk ->
            Decl.Kind.Value
              {vk with sideEffects = SideEffects.checkExpr vb.vb_expr}
          | dk -> dk
        in
        Declarations.replace_builder decls loc_start
          {
            decl with
            declKind;
            posEnd = vb.vb_loc.loc_end;
            posStart = vb.vb_loc.loc_start;
          });
      loc
    | _ -> current_binding
  in
  loc

let processOptionalArgs ~config ~cross_file ~expType ~(locFrom : Location.t)
    ~(binding : Location.t) ~locTo ~path args =
  if expType |> DeadOptionalArgs.hasOptionalArgs then (
    let supplied = ref [] in
    let suppliedMaybe = ref [] in
    args
    |> List.iter (fun (lbl, arg) ->
           let argIsSupplied =
             match arg with
             | Some
                 {
                   Typedtree.exp_desc =
                     Texp_construct (_, {cstr_name = "Some"}, _);
                 } ->
               Some true
             | Some
                 {
                   Typedtree.exp_desc =
                     Texp_construct (_, {cstr_name = "None"}, _);
                 } ->
               Some false
             | Some _ -> None
             | None -> Some false
           in
           match lbl with
           | Asttypes.Optional {txt = s} when not locFrom.loc_ghost ->
             if argIsSupplied <> Some false then supplied := s :: !supplied;
             if argIsSupplied = None then suppliedMaybe := s :: !suppliedMaybe
           | _ -> ());
    (!supplied, !suppliedMaybe)
    |> DeadOptionalArgs.addReferences ~config ~cross_file ~locFrom ~locTo
         ~binding ~path)

let rec collectExpr ~config ~refs ~file_deps ~cross_file
    ~(last_binding : Location.t) super self (e : Typedtree.expression) =
  let locFrom = e.exp_loc in
  let binding = last_binding in
  (match e.exp_desc with
  | Texp_ident (_path, _, {Types.val_loc = {loc_ghost = false; _} as locTo}) ->
    (* if Path.name _path = "rc" then assert false; *)
    if locFrom = locTo && _path |> Path.name = "emptyArray" then (
      (* Work around lowercase jsx with no children producing an artifact `emptyArray`
         which is called from its own location as many things are generated on the same location. *)
      if config.DceConfig.cli.debug then
        Log_.item "addDummyReference %s --> %s@."
          (Location.none.loc_start |> Pos.toString)
          (locTo.loc_start |> Pos.toString);
      References.add_value_ref refs ~posTo:locTo.loc_start
        ~posFrom:Location.none.loc_start)
    else
      addValueReference ~config ~refs ~file_deps ~binding ~addFileReference:true
        ~locFrom ~locTo
  | Texp_apply
      {
        funct =
          {
            exp_desc =
              Texp_ident
                (path, _, {Types.val_loc = {loc_ghost = false; _} as locTo});
            exp_type;
          };
        args;
      } ->
    args
    |> processOptionalArgs ~config ~cross_file ~expType:exp_type
         ~locFrom:(locFrom : Location.t)
         ~binding:last_binding ~locTo ~path
  | Texp_let
      ( (* generated for functions with optional args *)
        Nonrecursive,
        [
          {
            vb_pat = {pat_desc = Tpat_var (idArg, _)};
            vb_expr =
              {
                exp_desc =
                  Texp_ident
                    (path, _, {Types.val_loc = {loc_ghost = false; _} as locTo});
                exp_type;
              };
          };
        ],
        {
          exp_desc =
            Texp_function
              {
                case =
                  {
                    c_lhs = {pat_desc = Tpat_var (etaArg, _)};
                    c_rhs =
                      {
                        exp_desc =
                          Texp_apply
                            {
                              funct = {exp_desc = Texp_ident (idArg2, _, _)};
                              args;
                            };
                      };
                  };
              };
        } )
    when Ident.name idArg = "arg"
         && Ident.name etaArg = "eta"
         && Path.name idArg2 = "arg" ->
    args
    |> processOptionalArgs ~config ~cross_file ~expType:exp_type
         ~locFrom:(locFrom : Location.t)
         ~binding:last_binding ~locTo ~path
  | Texp_field
      (_, _, {lbl_loc = {Location.loc_start = posTo; loc_ghost = false}; _}) ->
    if !Config.analyzeTypes then
      DeadType.addTypeReference ~config ~refs ~posTo ~posFrom:locFrom.loc_start
  | Texp_construct
      ( _,
        {cstr_loc = {Location.loc_start = posTo; loc_ghost} as locTo; cstr_tag},
        _ ) ->
    (match cstr_tag with
    | Cstr_extension path ->
      path
      |> DeadException.markAsUsed ~config ~refs ~file_deps ~cross_file ~binding
           ~locFrom ~locTo
    | _ -> ());
    if !Config.analyzeTypes && not loc_ghost then
      DeadType.addTypeReference ~config ~refs ~posTo ~posFrom:locFrom.loc_start
  | Texp_record {fields} ->
    fields
    |> Array.iter (fun (_, record_label_definition, _) ->
           match record_label_definition with
           | Typedtree.Overridden (_, ({exp_loc} as e)) when exp_loc.loc_ghost
             ->
             (* Punned field in OCaml projects has ghost location in expression *)
             let e = {e with exp_loc = {exp_loc with loc_ghost = false}} in
             collectExpr ~config ~refs ~file_deps ~cross_file ~last_binding
               super self e
             |> ignore
           | _ -> ())
  | _ -> ());
  super.Tast_mapper.expr self e

(*
  type k. is a locally abstract type
  https://caml.inria.fr/pub/docs/manual-ocaml/locallyabstract.html
  it is required because in ocaml >= 4.11 Typedtree.pattern and ADT is converted
  in a GADT
  https://github.com/ocaml/ocaml/commit/312253ce822c32740349e572498575cf2a82ee96
  in short: all branches of pattern matches aren't the same type.
  With this annotation we declare a new type for each branch to allow the
  function to be typed.
  *)
let collectPattern ~config ~refs :
    _ -> _ -> Typedtree.pattern -> Typedtree.pattern =
 fun super self pat ->
  let posFrom = pat.Typedtree.pat_loc.loc_start in
  (match pat.pat_desc with
  | Typedtree.Tpat_record (cases, _clodsedFlag) ->
    cases
    |> List.iter (fun (_loc, {Types.lbl_loc = {loc_start = posTo}}, _pat, _) ->
           if !Config.analyzeTypes then
             DeadType.addTypeReference ~config ~refs ~posFrom ~posTo)
  | _ -> ());
  super.Tast_mapper.pat self pat

let rec getSignature (moduleType : Types.module_type) =
  match moduleType with
  | Mty_signature signature -> signature
  | Mty_functor (_, _mtParam, mt) -> getSignature mt
  | _ -> []

let rec processSignatureItem ~config ~decls ~file ~doTypes ~doValues ~moduleLoc
    ~(modulePath : ModulePath.t) ~path (si : Types.signature_item) =
  match si with
  | Sig_type (id, t, _) when doTypes ->
    if !Config.analyzeTypes then
      DeadType.addDeclaration ~config ~decls ~file ~modulePath ~typeId:id
        ~typeKind:t.type_kind
  | Sig_value (id, {Types.val_loc = loc; val_kind = kind; val_type})
    when doValues ->
    if not loc.Location.loc_ghost then
      let isPrimitive =
        match kind with
        | Val_prim _ -> true
        | _ -> false
      in
      if (not isPrimitive) || !Config.analyzeExternals then
        let optionalArgs =
          val_type |> DeadOptionalArgs.fromTypeExpr |> OptionalArgs.fromList
        in

        (* if Ident.name id = "someValue" then
           Printf.printf "XXX %s\n" (Ident.name id); *)
        Ident.name id
        |> Name.create ~isInterface:false
        |> addValueDeclaration ~config ~decls ~file ~loc ~moduleLoc
             ~optionalArgs ~path ~sideEffects:false
  | Sig_module (id, {Types.md_type = moduleType; md_loc = moduleLoc}, _)
  | Sig_modtype (id, {Types.mtd_type = Some moduleType; mtd_loc = moduleLoc}) ->
    let modulePath' =
      ModulePath.enterModule modulePath
        ~name:(id |> Ident.name |> Name.create)
        ~loc:moduleLoc
    in
    let collect =
      match si with
      | Sig_modtype _ -> false
      | _ -> true
    in
    if collect then
      getSignature moduleType
      |> List.iter
           (processSignatureItem ~config ~decls ~file ~doTypes ~doValues
              ~moduleLoc ~modulePath:modulePath'
              ~path:((id |> Ident.name |> Name.create) :: path))
  | _ -> ()

(* Traverse the AST *)
let traverseStructure ~config ~decls ~refs ~file_deps ~cross_file ~file ~doTypes
    ~doExternals (structure : Typedtree.structure) : unit =
  let rec create_mapper (last_binding : Location.t) (modulePath : ModulePath.t)
      =
    let super = Tast_mapper.default in
    let rec mapper =
      {
        super with
        expr =
          (fun _self e ->
            e
            |> collectExpr ~config ~refs ~file_deps ~cross_file ~last_binding
                 super mapper);
        pat = (fun _self p -> p |> collectPattern ~config ~refs super mapper);
        structure_item =
          (fun _self (structureItem : Typedtree.structure_item) ->
            let modulePath_for_item_opt =
              match structureItem.str_desc with
              | Tstr_module {mb_expr; mb_id; mb_loc} ->
                let hasInterface =
                  match mb_expr.mod_desc with
                  | Tmod_constraint _ -> true
                  | _ -> false
                in
                let modulePath' =
                  ModulePath.enterModule modulePath
                    ~name:(mb_id |> Ident.name |> Name.create)
                    ~loc:mb_loc
                in
                if hasInterface then
                  match mb_expr.mod_type with
                  | Mty_signature signature ->
                    signature
                    |> List.iter
                         (processSignatureItem ~config ~decls ~file ~doTypes
                            ~doValues:false ~moduleLoc:mb_expr.mod_loc
                            ~modulePath:modulePath'
                            ~path:
                              (modulePath'.path
                              @ [FileContext.module_name_tagged file]))
                  | _ -> ()
                else ();
                Some modulePath'
              | Tstr_primitive vd when doExternals && !Config.analyzeExternals
                ->
                let path =
                  modulePath.path @ [FileContext.module_name_tagged file]
                in
                let exists =
                  match
                    Declarations.find_opt_builder decls vd.val_loc.loc_start
                  with
                  | Some {declKind = Value _} -> true
                  | _ -> false
                in
                let id = vd.val_id |> Ident.name in
                Printf.printf "Primitive %s\n" id;
                if
                  (not exists) && id <> "unsafe_expr"
                  (* see https://github.com/BuckleScript/bucklescript/issues/4532 *)
                then
                  id
                  |> Name.create ~isInterface:false
                  |> addValueDeclaration ~config ~decls ~file ~path
                       ~loc:vd.val_loc ~moduleLoc:modulePath.loc
                       ~sideEffects:false;
                None
              | Tstr_type (_recFlag, typeDeclarations) when doTypes ->
                if !Config.analyzeTypes then
                  typeDeclarations
                  |> List.iter
                       (fun (typeDeclaration : Typedtree.type_declaration) ->
                         DeadType.addDeclaration ~config ~decls ~file
                           ~modulePath ~typeId:typeDeclaration.typ_id
                           ~typeKind:typeDeclaration.typ_type.type_kind);
                None
              | Tstr_include {incl_mod; incl_type} ->
                (match incl_mod.mod_desc with
                | Tmod_ident (_path, _lid) ->
                  let currentPath =
                    modulePath.path @ [FileContext.module_name_tagged file]
                  in
                  incl_type
                  |> List.iter
                       (processSignatureItem ~config ~decls ~file ~doTypes
                          ~doValues:false (* TODO: also values? *)
                          ~moduleLoc:incl_mod.mod_loc ~modulePath
                          ~path:currentPath)
                | _ -> ());
                None
              | Tstr_exception {ext_id = id; ext_loc = loc} ->
                let path =
                  modulePath.path @ [FileContext.module_name_tagged file]
                in
                let name = id |> Ident.name |> Name.create in
                ignore
                  (DeadException.add ~config ~decls ~file ~path ~loc
                     ~strLoc:structureItem.str_loc ~moduleLoc:modulePath.loc
                     name);
                None
              | _ -> None
            in
            let mapper_for_item =
              match modulePath_for_item_opt with
              | None -> mapper
              | Some modulePath_for_item ->
                create_mapper last_binding modulePath_for_item
            in
            super.structure_item mapper_for_item structureItem);
        value_binding =
          (fun _self vb ->
            let loc =
              vb
              |> collectValueBinding ~config ~decls ~file
                   ~current_binding:last_binding ~modulePath
            in
            let nested_mapper = create_mapper loc modulePath in
            super.Tast_mapper.value_binding nested_mapper vb);
      }
    in
    mapper
  in
  let mapper = create_mapper Location.none ModulePath.initial in
  mapper.structure mapper structure |> ignore

(* Merge a location's references to another one's *)
let processValueDependency ~config ~decls ~refs ~file_deps ~cross_file
    ( ({
         val_loc =
           {loc_start = {pos_fname = fnTo} as posTo; loc_ghost = ghost1} as
           locTo;
       } :
        Types.value_description),
      ({
         val_loc =
           {loc_start = {pos_fname = fnFrom} as posFrom; loc_ghost = ghost2} as
           locFrom;
       } :
        Types.value_description) ) =
  if (not ghost1) && (not ghost2) && posTo <> posFrom then (
    let addFileReference = fileIsImplementationOf fnTo fnFrom in
    addValueReference ~config ~refs ~file_deps ~binding:Location.none
      ~addFileReference ~locFrom ~locTo;
    DeadOptionalArgs.addFunctionReference ~config ~decls ~cross_file ~locFrom
      ~locTo)

let processStructure ~config ~decls ~refs ~file_deps ~cross_file ~file
    ~cmt_value_dependencies ~doTypes ~doExternals
    (structure : Typedtree.structure) =
  traverseStructure ~config ~decls ~refs ~file_deps ~cross_file ~file ~doTypes
    ~doExternals structure;
  let valueDependencies = cmt_value_dependencies |> List.rev in
  valueDependencies
  |> List.iter
       (processValueDependency ~config ~decls ~refs ~file_deps ~cross_file)
