open DeadCommon

type values_builder = (Name.t, Exceptions.t) Hashtbl.t
(** Per-file mutable builder for exception values during AST processing *)

type values_table = (string, (Name.t, Exceptions.t) Hashtbl.t) Hashtbl.t
(** Merged immutable table for cross-file lookups *)

let create_values_builder () : values_builder = Hashtbl.create 15

let values_builder_add (builder : values_builder) ~module_path ~name exceptions =
  let path = (name |> Name.create) :: module_path.ModulePath.path in
  Hashtbl.replace builder (path |> DcePath.to_name) exceptions

(** Merge all per-file builders into a single lookup table *)
let merge_values_builders (builders : (string * values_builder) list) :
    values_table =
  let table = Hashtbl.create 15 in
  builders
  |> List.iter (fun (module_name, builder) ->
         Hashtbl.replace table module_name builder);
  table

module Values = struct
  let get_from_module (table : values_table) ~module_name ~module_path
      (path_ : DcePath.t) =
    let name = path_ @ module_path |> DcePath.to_name in
    match Hashtbl.find_opt table (String.capitalize_ascii module_name) with
    | Some tbl -> Hashtbl.find_opt tbl name
    | None -> (
      match Hashtbl.find_opt table (String.uncapitalize_ascii module_name) with
      | Some tbl -> Hashtbl.find_opt tbl name
      | None -> None)

  let rec find_local (table : values_table) ~module_name ~module_path path =
    match path |> get_from_module table ~module_name ~module_path with
    | Some exceptions -> Some exceptions
    | None -> (
      match module_path with
      | [] -> None
      | _ :: rest_module_path ->
        path |> find_local table ~module_name ~module_path:rest_module_path)

  let find_path (table : values_table) ~module_name ~module_path path =
    let find_external ~external_module_name ~path_rev =
      path_rev |> List.rev
      |> get_from_module table
           ~module_name:(external_module_name |> Name.to_string)
           ~module_path:[]
    in
    match path |> find_local table ~module_name ~module_path with
    | None -> (
      (* Search in another file *)
      match path |> List.rev with
      | external_module_name :: path_rev -> (
        match (find_external ~external_module_name ~path_rev, path_rev) with
        | (Some _ as found), _ -> found
        | None, external_module_name2 :: path_rev2
          when !Cli.cmt_command && path_rev2 <> [] ->
          (* Simplistic namespace resolution for dune namespace: skip the root of the path *)
          find_external ~external_module_name:external_module_name2 ~path_rev:path_rev2
        | None, _ -> None)
      | [] -> None)
    | Some exceptions -> Some exceptions
end

module Event = struct
  type kind =
    | Catches of t list (* with | E => ... *)
    | Call of {callee: DcePath.t; module_path: DcePath.t} (* foo() *)
    | DoesNotThrow of
        t list (* DoesNotThrow(events) where events come from an expression *)
    | Throws  (** throw E *)

  and t = {exceptions: Exceptions.t; kind: kind; loc: Location.t}

  let rec print ppf event =
    match event with
    | {kind = Call {callee; module_path}; exceptions; loc} ->
      Format.fprintf ppf "%s Call(%s, modulePath:%s) %a@."
        (loc.loc_start |> Pos.to_string)
        (callee |> DcePath.to_string)
        (module_path |> DcePath.to_string)
        (Exceptions.pp ~exn_table:None)
        exceptions
    | {kind = DoesNotThrow nested_events; loc} ->
      Format.fprintf ppf "%s DoesNotThrow(%a)@."
        (loc.loc_start |> Pos.to_string)
        (fun ppf () ->
          nested_events |> List.iter (fun e -> Format.fprintf ppf "%a " print e))
        ()
    | {kind = Throws; exceptions; loc} ->
      Format.fprintf ppf "%s throws %a@."
        (loc.loc_start |> Pos.to_string)
        (Exceptions.pp ~exn_table:None)
        exceptions
    | {kind = Catches nested_events; exceptions; loc} ->
      Format.fprintf ppf "%s Catches exceptions:%a nestedEvents:%a@."
        (loc.loc_start |> Pos.to_string)
        (Exceptions.pp ~exn_table:None)
        exceptions
        (fun ppf () ->
          nested_events |> List.iter (fun e -> Format.fprintf ppf "%a " print e))
        ()

  let combine ~(values_table : values_table) ~config ~module_name events =
    if config.DceConfig.cli.debug then (
      Log_.item "@.";
      Log_.item "Events combine: #events %d@." (events |> List.length));
    let exn_table = Hashtbl.create 1 in
    let extend_exn_table exn loc =
      match Hashtbl.find_opt exn_table exn with
      | Some loc_set -> Hashtbl.replace exn_table exn (LocSet.add loc loc_set)
      | None -> Hashtbl.replace exn_table exn (LocSet.add loc LocSet.empty)
    in
    let shrink_exn_table exn loc =
      match Hashtbl.find_opt exn_table exn with
      | Some loc_set -> Hashtbl.replace exn_table exn (LocSet.remove loc loc_set)
      | None -> ()
    in
    let rec loop exn_set events =
      match events with
      | ({kind = Throws; exceptions; loc} as ev) :: rest ->
        if config.DceConfig.cli.debug then Log_.item "%a@." print ev;
        exceptions |> Exceptions.iter (fun exn -> extend_exn_table exn loc);
        loop (Exceptions.union exn_set exceptions) rest
      | ({kind = Call {callee; module_path}; loc} as ev) :: rest ->
        if config.DceConfig.cli.debug then Log_.item "%a@." print ev;
        let exceptions =
          match
            callee |> Values.find_path values_table ~module_name ~module_path
          with
          | Some exceptions -> exceptions
          | _ -> (
            match ExnLib.find callee with
            | Some exceptions -> exceptions
            | None -> Exceptions.empty)
        in
        exceptions |> Exceptions.iter (fun exn -> extend_exn_table exn loc);
        loop (Exceptions.union exn_set exceptions) rest
      | ({kind = DoesNotThrow nested_events; loc} as ev) :: rest ->
        if config.DceConfig.cli.debug then Log_.item "%a@." print ev;
        let nested_exceptions = loop Exceptions.empty nested_events in
        (if Exceptions.is_empty nested_exceptions (* catch-all *) then
           let name =
             match nested_events with
             | {kind = Call {callee}} :: _ -> callee |> DcePath.to_name
             | _ -> "expression" |> Name.create
           in
           Log_.warning ~loc
             (Issue.ExceptionAnalysis
                {
                  message =
                    Format.asprintf
                      "@{<info>%s@} does not throw and is annotated with \
                       redundant @doesNotThrow"
                      (name |> Name.to_string);
                }));
        loop exn_set rest
      | ({kind = Catches nested_events; exceptions} as ev) :: rest ->
        if config.DceConfig.cli.debug then Log_.item "%a@." print ev;
        if Exceptions.is_empty exceptions then loop exn_set rest
        else
          let nested_exceptions = loop Exceptions.empty nested_events in
          let new_throws = Exceptions.diff nested_exceptions exceptions in
          exceptions
          |> Exceptions.iter (fun exn ->
                 nested_events
                 |> List.iter (fun event -> shrink_exn_table exn event.loc));
          loop (Exceptions.union exn_set new_throws) rest
      | [] -> exn_set
    in
    let exn_set = loop Exceptions.empty events in
    (exn_set, exn_table)
end

type checks_builder = check list ref
(** Per-file mutable builder for checks during AST processing *)

and check = {
  events: Event.t list;
  loc: Location.t;
  loc_full: Location.t;
  module_name: string;
  exn_name: string;
  exceptions: Exceptions.t;
}

let create_checks_builder () : checks_builder = ref []

let checks_builder_add (builder : checks_builder) ~events ~exceptions ~loc
    ?(loc_full = loc) ~module_name exn_name =
  builder := {events; exceptions; loc; loc_full; module_name; exn_name} :: !builder

let checks_builder_to_list (builder : checks_builder) : check list =
  !builder |> List.rev

module Checks = struct
  let do_check ~(values_table : values_table) ~config
      {events; exceptions; loc; loc_full; module_name; exn_name} =
    let throw_set, exn_table =
      events |> Event.combine ~values_table ~config ~module_name
    in
    let missing_annotations = Exceptions.diff throw_set exceptions in
    let redundant_annotations = Exceptions.diff exceptions throw_set in
    (if not (Exceptions.is_empty missing_annotations) then
       let description =
         Issue.ExceptionAnalysisMissing
           {exn_name; exn_table; throw_set; missing_annotations; loc_full}
       in
       Log_.warning ~loc description);
    if not (Exceptions.is_empty redundant_annotations) then
      Log_.warning ~loc
        (Issue.ExceptionAnalysis
           {
             message =
               (let throws_description ppf () =
                  if throw_set |> Exceptions.is_empty then
                    Format.fprintf ppf "throws nothing"
                  else
                    Format.fprintf ppf "might throw %a"
                      (Exceptions.pp ~exn_table:(Some exn_table))
                      throw_set
                in
                Format.asprintf
                  "@{<info>%s@} %a and is annotated with redundant @throws(%a)"
                  exn_name throws_description ()
                  (Exceptions.pp ~exn_table:None)
                  redundant_annotations);
           })

  let do_checks ~values_table ~config (checks : check list) =
    checks |> List.iter (do_check ~values_table ~config)
end

let traverse_ast ~file ~values_builder ~checks_builder () =
  let super = Tast_mapper.default in
  let current_id = ref "" in
  let current_events = ref [] in
  (* For local lookups during AST processing, we look up in the current file's builder *)
  let find_local_exceptions ~module_path path =
    let name = path @ module_path |> DcePath.to_name in
    Hashtbl.find_opt values_builder name
  in
  let rec find_local_path ~module_path path =
    match path |> find_local_exceptions ~module_path with
    | Some exceptions -> Some exceptions
    | None -> (
      match module_path with
      | [] -> None
      | _ :: rest_module_path -> path |> find_local_path ~module_path:rest_module_path)
  in
  let exceptions_of_patterns patterns =
    patterns
    |> List.fold_left
         (fun acc desc ->
           match desc with
           | Typedtree.Tpat_construct ({txt}, _, _) ->
             Exceptions.add (Exn.from_lid txt) acc
           | _ -> acc)
         Exceptions.empty
  in
  let iter_expr self e = self.Tast_mapper.expr self e |> ignore in
  let iter_expr_opt self eo =
    match eo with
    | None -> ()
    | Some e -> e |> iter_expr self
  in
  let iter_pat self p = self.Tast_mapper.pat self p |> ignore in
  let iter_cases self cases =
    cases
    |> List.iter (fun case ->
           case.Typedtree.c_lhs |> iter_pat self;
           case.c_guard |> iter_expr_opt self;
           case.c_rhs |> iter_expr self)
  in
  let is_throw s = s = "Pervasives.raise" || s = "Pervasives.throw" in
  let throw_args args =
    match args with
    | [(_, Some {Typedtree.exp_desc = Texp_construct ({txt}, _, _)})] ->
      [Exn.from_lid txt] |> Exceptions.from_list
    | [(_, Some {Typedtree.exp_desc = Texp_ident _})] ->
      [Exn.from_string "genericException"] |> Exceptions.from_list
    | _ -> [Exn.from_string "TODO_from_raise1"] |> Exceptions.from_list
  in
  let does_not_throw attributes =
    attributes
    |> Annotation.get_attribute_payload (function
         | "doesNotRaise" | "doesnotraise" | "DoesNoRaise" | "doesNotraise"
         | "doNotRaise" | "donotraise" | "DoNoRaise" | "doNotraise"
         | "doesNotThrow" | "doesnotthrow" | "DoesNoThrow" | "doesNotthrow"
         | "doNotThrow" | "donotthrow" | "DoNoThrow" | "doNotthrow" ->
           true
         | _ -> false)
    <> None
  in
  let expr ~(module_path : ModulePath.t) (self : Tast_mapper.mapper)
      (expr : Typedtree.expression) =
    let loc = expr.exp_loc in
    let is_does_no_throw = expr.exp_attributes |> does_not_throw in
    let old_events = !current_events in
    if is_does_no_throw then current_events := [];
    (match expr.exp_desc with
    | Texp_ident (callee_, _, _) ->
      let callee =
        callee_ |> DcePath.from_path_t |> ModulePath.resolve_alias module_path
      in
      let callee_name = callee |> DcePath.to_name in
      if callee_name |> Name.to_string |> is_throw then
        Log_.warning ~loc
          (Issue.ExceptionAnalysis
             {
               message =
                 Format.asprintf
                   "@{<info>%s@} can be analyzed only if called directly"
                   (callee_name |> Name.to_string);
             });
      current_events :=
        {
          Event.exceptions = Exceptions.empty;
          loc;
          kind = Call {callee; module_path = module_path.path};
        }
        :: !current_events
    | Texp_apply
        {
          funct = {exp_desc = Texp_ident (atat, _, _)};
          args = [(_lbl1, Some {exp_desc = Texp_ident (callee, _, _)}); arg];
        }
      when (* raise @@ Exn(...) *)
           atat |> Path.name = "Pervasives.@@" && callee |> Path.name |> is_throw
      ->
      let exceptions = [arg] |> throw_args in
      current_events := {Event.exceptions; loc; kind = Throws} :: !current_events;
      arg |> snd |> iter_expr_opt self
    | Texp_apply {funct = {exp_desc = Texp_ident (callee, _, _)} as e; args} ->
      let callee_name = Path.name callee in
      if callee_name |> is_throw then
        let exceptions = args |> throw_args in
        current_events :=
          {Event.exceptions; loc; kind = Throws} :: !current_events
      else e |> iter_expr self;
      args |> List.iter (fun (_, e_opt) -> e_opt |> iter_expr_opt self)
    | Texp_match (e, cases_ok, cases_exn, partial) ->
      let cases = cases_ok @ cases_exn in
      let exception_patterns =
        cases_exn
        |> List.map (fun (case : Typedtree.case) -> case.c_lhs.pat_desc)
      in
      let exceptions = exception_patterns |> exceptions_of_patterns in
      if exception_patterns <> [] then (
        let old_events = !current_events in
        current_events := [];
        e |> iter_expr self;
        current_events :=
          {Event.exceptions; loc; kind = Catches !current_events} :: old_events)
      else e |> iter_expr self;
      cases |> iter_cases self;
      if partial = Partial then
        current_events :=
          {
            Event.exceptions = [Exn.match_failure] |> Exceptions.from_list;
            loc;
            kind = Throws;
          }
          :: !current_events
    | Texp_try (e, cases) ->
      let exceptions =
        cases
        |> List.map (fun case -> case.Typedtree.c_lhs.pat_desc)
        |> exceptions_of_patterns
      in
      let old_events = !current_events in
      current_events := [];
      e |> iter_expr self;
      current_events :=
        {Event.exceptions; loc; kind = Catches !current_events} :: old_events;
      cases |> iter_cases self
    | _ -> super.expr self expr |> ignore);
    (if is_does_no_throw then
       let nested_events = !current_events in
       current_events :=
         {
           Event.exceptions = Exceptions.empty;
           loc;
           kind = DoesNotThrow nested_events;
         }
         :: old_events);
    expr
  in
  let get_exceptions_from_annotations attributes =
    let throws_annotation_payload =
      attributes
      |> Annotation.get_attribute_payload (fun s ->
             s = "throws" || s = "throw" || s = "raises" || s = "raise")
    in
    let rec get_exceptions payload =
      match payload with
      | Annotation.StringPayload s -> [Exn.from_string s] |> Exceptions.from_list
      | Annotation.ConstructPayload s when s <> "::" ->
        [Exn.from_string s] |> Exceptions.from_list
      | Annotation.IdentPayload s ->
        [Exn.from_string (s |> Longident.flatten |> String.concat ".")]
        |> Exceptions.from_list
      | Annotation.TuplePayload tuple ->
        tuple
        |> List.map (fun payload ->
               payload |> get_exceptions |> Exceptions.to_list)
        |> List.concat |> Exceptions.from_list
      | _ -> Exceptions.empty
    in
    match throws_annotation_payload with
    | None -> Exceptions.empty
    | Some payload -> payload |> get_exceptions
  in
  let toplevel_eval (self : Tast_mapper.mapper) (expr : Typedtree.expression)
      attributes =
    let old_id = !current_id in
    let old_events = !current_events in
    let name = "Toplevel expression" in
    current_id := name;
    current_events := [];
    let module_name = file.FileContext.module_name in
    self.expr self expr |> ignore;
    checks_builder_add checks_builder ~events:!current_events
      ~exceptions:(get_exceptions_from_annotations attributes)
      ~loc:expr.exp_loc ~module_name name;
    current_id := old_id;
    current_events := old_events
  in
  let value_binding ~(module_path : ModulePath.t) (self : Tast_mapper.mapper)
      (vb : Typedtree.value_binding) =
    let old_id = !current_id in
    let old_events = !current_events in
    let is_function =
      match vb.vb_expr.exp_desc with
      | Texp_function _ -> true
      | _ -> false
    in
    let is_toplevel = !current_id = "" in
    let process_binding name =
      current_id := name;
      current_events := [];
      let exceptions_from_annotations =
        get_exceptions_from_annotations vb.vb_attributes
      in
      values_builder_add values_builder ~module_path ~name
        exceptions_from_annotations;
      let res = super.value_binding self vb in
      let module_name = file.FileContext.module_name in
      let path = [name |> Name.create] in
      let exceptions =
        match path |> find_local_path ~module_path:module_path.path with
        | Some exceptions -> exceptions
        | _ -> Exceptions.empty
      in
      checks_builder_add checks_builder ~events:!current_events ~exceptions
        ~loc:vb.vb_pat.pat_loc ~loc_full:vb.vb_loc ~module_name name;
      current_id := old_id;
      current_events := old_events;
      res
    in
    match vb.vb_pat.pat_desc with
    | Tpat_any when is_toplevel && not vb.vb_loc.loc_ghost -> process_binding "_"
    | Tpat_construct ({txt}, _, _)
      when is_toplevel && (not vb.vb_loc.loc_ghost)
           && txt = Longident.Lident "()" ->
      process_binding "()"
    | Tpat_var (id, {loc = {loc_ghost}})
      when (is_function || is_toplevel) && (not loc_ghost)
           && not vb.vb_loc.loc_ghost ->
      process_binding (id |> Ident.name)
    | _ -> super.value_binding self vb
  in
  let make_mapper (module_path : ModulePath.t) : Tast_mapper.mapper =
    let open Tast_mapper in
    {
      super with
      expr = expr ~module_path;
      value_binding = value_binding ~module_path;
    }
  in
  let rec process_module_expr (module_path : ModulePath.t)
      (me : Typedtree.module_expr) =
    match me.mod_desc with
    | Tmod_structure structure -> process_structure module_path structure
    | Tmod_constraint (me1, _mty, _mtc, _coercion) ->
      process_module_expr module_path me1
    | Tmod_apply (me1, me2, _) ->
      process_module_expr module_path me1;
      process_module_expr module_path me2
    | _ ->
      let mapper = make_mapper module_path in
      super.module_expr mapper me |> ignore
  and process_structure (module_path : ModulePath.t)
      (structure : Typedtree.structure) =
    let rec loop (mp : ModulePath.t) (items : Typedtree.structure_item list) =
      match items with
      | [] -> ()
      | structure_item :: rest ->
        let mapper = make_mapper mp in
        let mp' =
          match structure_item.str_desc with
          | Tstr_eval (expr, attributes) ->
            toplevel_eval mapper expr attributes;
            mp
          | Tstr_module {mb_id; mb_loc; mb_expr} -> (
            let name = mb_id |> Ident.name |> Name.create in
            let mp_inside = ModulePath.enter_module mp ~name ~loc:mb_loc in
            process_module_expr mp_inside mb_expr;
            match mb_expr.mod_desc with
            | Tmod_ident (path_, _lid) ->
              ModulePath.add_alias mp ~name ~path:(path_ |> DcePath.from_path_t)
            | _ -> mp)
          | Tstr_recmodule mbs ->
            (* Process each module in the recursive group in the current scope; aliases are collected in the current scope too. *)
            List.fold_left
              (fun acc {Typedtree.mb_id; mb_loc; mb_expr} ->
                let name = mb_id |> Ident.name |> Name.create in
                let mp_inside = ModulePath.enter_module acc ~name ~loc:mb_loc in
                process_module_expr mp_inside mb_expr;
                match mb_expr.mod_desc with
                | Tmod_ident (path_, _lid) ->
                  ModulePath.add_alias acc ~name
                    ~path:(path_ |> DcePath.from_path_t)
                | _ -> acc)
              mp mbs
          | _ ->
            super.structure_item mapper structure_item |> ignore;
            mp
        in
        loop mp' rest
    in
    loop module_path structure.str_items
  in
  fun (structure : Typedtree.structure) ->
    process_structure ModulePath.initial structure

type file_result = {
  module_name: string;
  values_builder: values_builder;
  checks: check list;
}
(** Result of processing a single file *)

let process_structure ~file ~values_builder ~checks_builder
    (structure : Typedtree.structure) =
  let process = traverse_ast ~file ~values_builder ~checks_builder () in
  process structure

let process_cmt ~file (cmt_infos : Cmt_format.cmt_infos) : file_result option =
  match cmt_infos.cmt_annots with
  | Interface _ -> None
  | Implementation structure ->
    let values_builder = create_values_builder () in
    let checks_builder = create_checks_builder () in
    structure |> process_structure ~file ~values_builder ~checks_builder;
    Some
      {
        module_name = file.FileContext.module_name;
        values_builder;
        checks = checks_builder_to_list checks_builder;
      }
  | _ -> None

(** Process all accumulated checks using merged values table *)
let run_checks ~config (all_results : file_result list) =
  (* Merge all values builders *)
  let values_table =
    all_results
    |> List.map (fun r -> (r.module_name, r.values_builder))
    |> merge_values_builders
  in
  (* Collect all checks *)
  let all_checks = all_results |> List.concat_map (fun r -> r.checks) in
  (* Run checks with merged table *)
  Checks.do_checks ~values_table ~config all_checks
