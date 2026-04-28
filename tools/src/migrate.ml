open Analysis

module StringMap = Map.Make (String)
module StringSet = Set.Make (String)
module IntSet = Set.Make (Int)

(* Public API: migrate ~entryPointFile ~outputMode *)

let is_unit_expr (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_construct ({txt = Lident "()"}, None) -> true
  | _ -> false

module InsertExt = struct
  type placeholder = Labelled of string | Unlabelled of int

  let ext_labelled = "insert.labelledArgument"
  let ext_unlabelled = "insert.unlabelledArgument"

  (*
     Unlabelled argument placeholders use 0-based indexing.
     Pipe semantics: the pipe LHS occupies index 0 when resolving placeholders
     in piped templates. For inner calls that exclude the LHS (e.g. `lhs->f(x)`),
     we adjust drop positions at the call site to keep the generated call correct.
  *)
  let placeholder_of_expr = function
    | {
        Parsetree.pexp_desc =
          Pexp_extension
            ( {txt},
              PStr [{pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant c}, _)}]
            );
      } ->
      if txt = ext_labelled then
        match c with
        | Pconst_string (name, _) -> Some (Labelled name)
        | _ -> None
      else if txt = ext_unlabelled then
        match c with
        | Pconst_integer (s, _) -> (
          match int_of_string_opt s with
          | Some i -> Some (Unlabelled i)
          | None -> None)
        | _ -> None
      else None
    | _ -> None
end

module ArgUtils = struct
  let map_expr_args mapper args =
    args
    |> List.map (fun (label, arg) -> (label, mapper.Ast_mapper.expr mapper arg))
end

module ExprUtils = struct
  let rec is_pipe_apply (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_apply {funct = {pexp_desc = Pexp_ident {txt = Lident "->"}}; _} ->
      true
    | Pexp_construct (_, Some e)
    | Pexp_constraint (e, _)
    | Pexp_coerce (e, _, _)
    | Pexp_let (_, _, e)
    | Pexp_sequence (e, _)
    | Pexp_letmodule (_, _, e)
    | Pexp_open (_, _, e) ->
      is_pipe_apply e
    | _ -> false
end

type args = (Asttypes.arg_label * Parsetree.expression) list

module MapperUtils = struct
  module ApplyTransforms = struct
    let attr_name = "apply.transforms"

    let split_attrs (attrs : Parsetree.attributes) =
      List.partition (fun ({Location.txt}, _) -> txt = attr_name) attrs

    let names_of_payload (payload : Parsetree.payload) : string list =
      match payload with
      | Parsetree.PStr
          [
            {pstr_desc = Parsetree.Pstr_eval ({pexp_desc = Pexp_array elems}, _)};
          ] ->
        elems
        |> List.filter_map (fun (e : Parsetree.expression) ->
               match e.pexp_desc with
               | Pexp_constant (Pconst_string (s, _)) -> Some s
               | _ -> None)
      | _ -> []

    let apply_names (names : string list) (e : Parsetree.expression) :
        Parsetree.expression =
      List.fold_left
        (fun acc name ->
          match Transforms.get name with
          | Some f -> f acc
          | None -> acc)
        e names

    let attach_to_replacement ~(attrs : Parsetree.attributes)
        (e : Parsetree.expression) : Parsetree.expression =
      if Ext_list.is_empty attrs then e
      else {e with pexp_attributes = attrs @ e.pexp_attributes}

    let attach_attrs_to_pat ~attrs (pat : Parsetree.pattern) =
      if Ext_list.is_empty attrs then pat
      else {pat with ppat_attributes = attrs @ pat.ppat_attributes}

    (* Apply transforms attached to an expression itself and drop the
       transform attributes afterwards. *)
    let apply_on_self (e : Parsetree.expression) : Parsetree.expression =
      let transform_attrs, other_attrs = split_attrs e.pexp_attributes in
      if Ext_list.is_empty transform_attrs then e
      else
        let names =
          transform_attrs
          |> List.concat_map (fun (_id, payload) -> names_of_payload payload)
        in
        let e' = {e with pexp_attributes = other_attrs} in
        apply_names names e'
  end

  (* Collect placeholder usages anywhere inside an expression. *)
  let collect_placeholders (expr : Parsetree.expression) =
    let labelled = ref StringSet.empty in
    let unlabelled = ref IntSet.empty in
    let open Ast_iterator in
    let iter =
      {
        default_iterator with
        expr =
          (fun self e ->
            (match InsertExt.placeholder_of_expr e with
            | Some (InsertExt.Labelled name) ->
              labelled := StringSet.add name !labelled
            | Some (InsertExt.Unlabelled i) when i >= 0 ->
              unlabelled := IntSet.add i !unlabelled
            | _ -> ());
            default_iterator.expr self e);
      }
    in
    iter.expr iter expr;
    (!labelled, !unlabelled)

  (* Build lookup tables for labelled and unlabelled source args. *)
  let build_source_arg_tables (source_args : args) =
    let labelled = Hashtbl.create 8 in
    let unlabelled = Hashtbl.create 8 in
    let idx = ref 0 in
    source_args
    |> List.iter (fun (lbl, arg) ->
           match lbl with
           | Asttypes.Nolabel ->
             Hashtbl.replace unlabelled !idx arg;
             incr idx
           | Asttypes.Labelled {txt} | Optional {txt} ->
             Hashtbl.replace labelled txt arg);
    (labelled, unlabelled)

  (* Replace placeholders anywhere inside an expression using the given
     source arguments. *)
  let replace_placeholders_in_expr (expr : Parsetree.expression)
      (source_args : args) =
    let labelled, unlabelled = build_source_arg_tables source_args in
    let mapper =
      {
        Ast_mapper.default_mapper with
        expr =
          (fun mapper exp ->
            match InsertExt.placeholder_of_expr exp with
            | Some (InsertExt.Labelled name) -> (
              match Hashtbl.find_opt labelled name with
              | Some arg ->
                ApplyTransforms.attach_to_replacement ~attrs:exp.pexp_attributes
                  arg
              | None -> exp)
            | Some (InsertExt.Unlabelled i) -> (
              match Hashtbl.find_opt unlabelled i with
              | Some arg ->
                ApplyTransforms.attach_to_replacement ~attrs:exp.pexp_attributes
                  arg
              | None -> exp)
            | None -> Ast_mapper.default_mapper.expr mapper exp);
      }
    in
    mapper.expr mapper expr

  let build_labelled_args_map (template_args : args) =
    template_args
    |> List.filter_map (fun (label, arg) ->
           match (label, InsertExt.placeholder_of_expr arg) with
           | ( (Asttypes.Labelled {txt = label} | Optional {txt = label}),
               Some (InsertExt.Labelled arg_name) ) ->
             Some (arg_name, label)
           | _ -> None)
    |> List.fold_left (fun map (k, v) -> StringMap.add k v map) StringMap.empty

  (*
     Pure computation of which template args to insert and which source args
     are consumed by placeholders.

     Indexing is 0-based everywhere.
     For piped application, the pipe LHS occupies index 0 in the source list
     used for placeholder resolution. If the inner call excludes the LHS
     (e.g. `lhs -> f(args)`), adjust drop positions accordingly at the call site.

     Returns:
     - template_args_to_insert: args to append to the final call
     - labelled_names_to_drop: names of labelled source args consumed
     - unlabelled_positions_to_drop: 0-based indices of unlabelled source args to drop
  *)
  type template_resolution = {
    args_to_insert: args;
    labelled_to_drop: StringSet.t;
    unlabelled_to_drop: IntSet.t;
  }

  let get_template_args_to_insert mapper (template_args : args)
      (source_args : args) : template_resolution =
    (* For each template argument, decide whether it is a placeholder that
       should be substituted from the source call, or a concrete argument which
       should be preserved (after mapping through the mapper).
       Accumulator:
       - rev_args: arguments to append to the final call (in reverse order)
       - used_labelled: names of labelled args consumed from the source call
       - used_unlabelled: 0-based positions of unlabelled args consumed. *)
    let accumulate_template_arg (rev_args, used_labelled, used_unlabelled)
        (label, arg) =
      (* Always perform nested replacement inside the argument expression,
         and collect which placeholders were used so we can drop them from the
         original call's arguments. *)
      let labelled_used_here, unlabelled_used_here = collect_placeholders arg in
      let arg_replaced = replace_placeholders_in_expr arg source_args in
      ( (label, mapper.Ast_mapper.expr mapper arg_replaced) :: rev_args,
        StringSet.union used_labelled labelled_used_here,
        IntSet.union used_unlabelled unlabelled_used_here )
    in
    let rev_args, labelled_set, unlabelled_set =
      List.fold_left accumulate_template_arg
        ([], StringSet.empty, IntSet.empty)
        template_args
    in
    {
      args_to_insert = List.rev rev_args;
      labelled_to_drop = labelled_set;
      unlabelled_to_drop = unlabelled_set;
    }

  (* Drop consumed source arguments.
     - unlabelled_positions_to_drop: 0-based indices of Nolabel args to drop
     - labelled_names_to_drop: names of labelled/optional args to drop *)
  let drop_args (source_args : args) ~unlabelled_positions_to_drop
      ~labelled_names_to_drop =
    let _, rev =
      List.fold_left
        (fun (idx, acc) (label, arg) ->
          match label with
          | Asttypes.Nolabel ->
            let drop = IntSet.mem idx unlabelled_positions_to_drop in
            let idx' = idx + 1 in
            if drop then (idx', acc) else (idx', (label, arg) :: acc)
          | Asttypes.Labelled {txt} | Optional {txt} ->
            if StringSet.mem txt labelled_names_to_drop then (idx, acc)
            else (idx, (label, arg) :: acc))
        (0, []) source_args
    in
    List.rev rev

  let rename_labels (source_args : args) ~labelled_args_map =
    source_args
    |> List.map (fun (label, arg) ->
           match label with
           | Asttypes.Labelled ({loc; txt} as l) -> (
             match StringMap.find_opt txt labelled_args_map with
             | Some mapped -> (Asttypes.Labelled {loc; txt = mapped}, arg)
             | None -> (Asttypes.Labelled l, arg))
           | Optional ({loc; txt} as l) -> (
             match StringMap.find_opt txt labelled_args_map with
             | Some mapped -> (Optional {loc; txt = mapped}, arg)
             | None -> (Optional l, arg))
           | _ -> (label, arg))

  let apply_migration_template mapper (template_args : args)
      (source_args : args) =
    let labelled_args_map = build_labelled_args_map template_args in
    let resolution =
      get_template_args_to_insert mapper template_args source_args
    in
    let dropped =
      drop_args source_args
        ~unlabelled_positions_to_drop:resolution.unlabelled_to_drop
        ~labelled_names_to_drop:resolution.labelled_to_drop
    in
    let renamed = rename_labels dropped ~labelled_args_map in
    renamed @ resolution.args_to_insert

  (* Adjust unlabelled drop positions for piped calls where the LHS occupies
     position 0 in placeholder resolution, but is not part of the inner call's
     argument list. *)
  let shift_unlabelled_drop_for_piped set =
    IntSet.fold
      (fun i acc -> if i > 0 then IntSet.add (i - 1) acc else acc)
      set IntSet.empty

  let migrate_piped_args mapper ~template_args ~lhs ~pipe_args =
    let full_source_args = lhs :: pipe_args in
    let resolution =
      get_template_args_to_insert mapper template_args full_source_args
    in
    let labelled_args_map = build_labelled_args_map template_args in
    let adjusted_unlabelled_to_drop =
      shift_unlabelled_drop_for_piped resolution.unlabelled_to_drop
    in
    let dropped =
      drop_args pipe_args
        ~unlabelled_positions_to_drop:adjusted_unlabelled_to_drop
        ~labelled_names_to_drop:resolution.labelled_to_drop
    in
    let renamed = rename_labels dropped ~labelled_args_map in
    renamed @ resolution.args_to_insert
end

module TypeReplace = struct
  let ext_replace_type = "replace.type"

  (* Extract a core_type payload from an expression extension of the form
     %replace.type(: <core_type>) *)
  let core_type_of_expr_extension (expr : Parsetree.expression) =
    match expr.pexp_desc with
    | Pexp_extension ({txt}, payload) when txt = ext_replace_type -> (
      match payload with
      | PTyp ct -> Some ct
      | _ -> None)
    | _ -> None
end

module ConstructorReplace = struct
  type target = {lid: Longident.t Location.loc; attrs: Parsetree.attributes}

  let of_template (expr : Parsetree.expression) : target option =
    match expr.pexp_desc with
    | Pexp_extension
        ( {txt = "replace.constructor"},
          PStr
            [
              {
                pstr_desc =
                  Pstr_eval
                    ({pexp_desc = Pexp_construct (lid, _); pexp_attributes}, _);
              };
            ] ) ->
      let attrs =
        if Ext_list.is_empty expr.pexp_attributes then pexp_attributes
        else expr.pexp_attributes @ pexp_attributes
      in
      Some {lid; attrs}
    | _ -> None
end

let remap_needed_extensions (mapper : Ast_mapper.mapper)
    (ext : Parsetree.extension) : Parsetree.extension =
  match ext with
  | ({txt = "todo_"} as e), payload ->
    Ast_mapper.default_mapper.extension mapper ({e with txt = "todo"}, payload)
  | e -> Ast_mapper.default_mapper.extension mapper e

let migrate_reference_from_info (deprecated_info : Cmt_utils.deprecated_used)
    exp =
  match deprecated_info.migration_template with
  | None -> exp
  | Some e -> (
    (* For identifier references, treat templates of the form `f()` as the
       function reference `f` to avoid inserting a spurious unit call. *)
    match e.pexp_desc with
    | Pexp_apply
        {funct; args = [(_lbl, unit_arg)]; partial = _; transformed_jsx = _}
      when is_unit_expr unit_arg ->
      MapperUtils.ApplyTransforms.attach_to_replacement ~attrs:e.pexp_attributes
        funct
    | _ -> e)

module Template = struct
  type t =
    | Apply of {
        funct: Parsetree.expression;
        args: args;
        partial: bool;
        transformed_jsx: bool;
      }

  let attach attrs e =
    MapperUtils.ApplyTransforms.attach_to_replacement ~attrs e

  let of_expr = function
    | {Parsetree.pexp_desc = Pexp_apply {funct; args; partial; transformed_jsx}}
      ->
      (* Normalize templates like `f()` to just `f` by dropping a single unit
         argument. This treats `String.concat()` as the function reference
         `String.concat`, not a call with a unit argument. *)
      let args' =
        match args with
        | [(_lbl, e)] when is_unit_expr e -> []
        | _ -> args
      in
      Some (Apply {funct; args = args'; partial; transformed_jsx})
    | _ -> None

  let of_expr_with_attrs (e : Parsetree.expression) :
      (t * Parsetree.attributes) option =
    match of_expr e with
    | Some t -> Some (t, e.pexp_attributes)
    | None -> None

  let mk_apply (exp : Parsetree.expression) ~funct ~args ~partial
      ~transformed_jsx =
    {exp with pexp_desc = Pexp_apply {funct; args; partial; transformed_jsx}}

  let apply_direct ~mapper ~template ~template_attrs ~call_args
      (exp : Parsetree.expression) =
    match template with
    | Apply
        {funct = template_funct; args = template_args; partial; transformed_jsx}
      ->
      let migrated_args =
        MapperUtils.apply_migration_template mapper template_args call_args
      in
      let res =
        mk_apply exp ~funct:template_funct ~args:migrated_args ~partial
          ~transformed_jsx
      in
      attach template_attrs res

  let apply_piped ~mapper ~template ~template_attrs ~lhs ~pipe_args ~funct
      (exp : Parsetree.expression) =
    match template with
    | Apply
        {funct = template_funct; args = template_args; partial; transformed_jsx}
      ->
      let pipe_args_mapped = ArgUtils.map_expr_args mapper pipe_args in
      let migrated_args =
        MapperUtils.migrate_piped_args mapper ~template_args ~lhs
          ~pipe_args:pipe_args_mapped
      in
      let inner = Ast_helper.Exp.apply template_funct migrated_args in
      let inner_with_attrs = attach template_attrs inner in
      mk_apply exp ~funct
        ~args:[lhs; (Asttypes.Nolabel, inner_with_attrs)]
        ~partial ~transformed_jsx

  let apply_piped_maybe_empty ~mapper ~template ~template_attrs ~lhs ~pipe_args
      ~funct (exp : Parsetree.expression) =
    match template with
    | Apply
        {funct = template_funct; args = template_args; partial; transformed_jsx}
      ->
      if Ext_list.is_empty pipe_args then
        let resolution =
          MapperUtils.get_template_args_to_insert mapper template_args []
        in
        if Ext_list.is_empty resolution.args_to_insert then
          let res =
            mk_apply exp ~funct
              ~args:
                [lhs; (Asttypes.Nolabel, attach template_attrs template_funct)]
              ~partial ~transformed_jsx
          in
          res
        else
          let inner =
            Ast_helper.Exp.apply template_funct resolution.args_to_insert
          in
          let inner_with_attrs = attach template_attrs inner in
          mk_apply exp ~funct
            ~args:[lhs; (Asttypes.Nolabel, inner_with_attrs)]
            ~partial ~transformed_jsx
      else
        apply_piped ~mapper ~template ~template_attrs ~lhs ~pipe_args ~funct exp

  let apply_single_pipe_collapse ~mapper ~template ~template_attrs ~lhs_exp
      ~pipe_args (exp : Parsetree.expression) =
    match template with
    | Apply
        {
          funct = templ_f;
          args = templ_args;
          partial = tpartial;
          transformed_jsx = tjsx;
        } ->
      let pipe_args_mapped = ArgUtils.map_expr_args mapper pipe_args in
      let migrated_args =
        MapperUtils.apply_migration_template mapper templ_args
          ((Asttypes.Nolabel, lhs_exp) :: pipe_args_mapped)
      in
      let res =
        mk_apply exp ~funct:templ_f ~args:migrated_args ~partial:tpartial
          ~transformed_jsx:tjsx
      in
      attach template_attrs res
end

(* Apply a direct-call migration template to a call site. *)
let apply_template_direct mapper template_expr call_args exp =
  match Template.of_expr template_expr with
  | Some template ->
    Template.apply_direct ~mapper ~template
      ~template_attrs:template_expr.pexp_attributes ~call_args exp
  | None -> exp

(* Helper removed: inline selection logic where needed for clarity. *)

(* Apply migration for a single-step pipe if possible, else use the piped
   template. Mirrors the previous inline logic from the mapper. *)
let apply_single_step_or_piped ~mapper
    ~(deprecated_info : Cmt_utils.deprecated_used) ~lhs ~lhs_exp ~pipe_args
    ~funct exp =
  let is_single_pipe_step = not (ExprUtils.is_pipe_apply lhs_exp) in
  let in_pipe_template =
    match deprecated_info.migration_in_pipe_chain_template with
    | Some e -> Template.of_expr_with_attrs e
    | None -> None
  in
  let direct_template =
    match deprecated_info.migration_template with
    | Some e -> Template.of_expr_with_attrs e
    | None -> None
  in
  if is_single_pipe_step && Option.is_some in_pipe_template then
    match direct_template with
    | Some (t, attrs) ->
      Template.apply_single_pipe_collapse ~mapper ~template:t
        ~template_attrs:attrs ~lhs_exp ~pipe_args exp
    | None -> (
      match in_pipe_template with
      | Some (t, attrs) ->
        Template.apply_piped ~mapper ~template:t ~template_attrs:attrs ~lhs
          ~pipe_args ~funct exp
      | None -> exp)
  else
    let chosen =
      match in_pipe_template with
      | None -> direct_template
      | some_tpl -> some_tpl
    in
    match chosen with
    | Some (t, attrs) ->
      Template.apply_piped_maybe_empty ~mapper ~template:t ~template_attrs:attrs
        ~lhs ~pipe_args ~funct exp
    | None -> exp

let makeMapper (deprecated_used : Cmt_utils.deprecated_used list) =
  let deprecated_function_calls =
    deprecated_used
    |> List.filter (fun (d : Cmt_utils.deprecated_used) ->
           match d.context with
           | Some FunctionCall -> true
           | _ -> false)
  in
  let loc_to_deprecated_fn_call =
    Hashtbl.create (List.length deprecated_function_calls)
  in
  deprecated_function_calls
  |> List.iter (fun ({Cmt_utils.source_loc} as d) ->
         Hashtbl.replace loc_to_deprecated_fn_call source_loc d);

  let deprecated_references =
    deprecated_used
    |> List.filter (fun (d : Cmt_utils.deprecated_used) ->
           match d.context with
           | Some Reference -> true
           | _ -> false)
  in
  let loc_to_deprecated_reference =
    Hashtbl.create (List.length deprecated_references)
  in
  deprecated_references
  |> List.iter (fun ({Cmt_utils.source_loc} as d) ->
         Hashtbl.replace loc_to_deprecated_reference source_loc d);

  let deprecated_constructor_constructors =
    deprecated_used
    |> List.filter_map (fun (d : Cmt_utils.deprecated_used) ->
           match d.migration_template with
           | Some template -> (
             match ConstructorReplace.of_template template with
             | Some target -> Some (d.source_loc, target)
             | None -> None)
           | None -> None)
  in
  let loc_to_deprecated_constructor_constructor =
    Hashtbl.create (List.length deprecated_constructor_constructors)
  in
  deprecated_constructor_constructors
  |> List.iter (fun (loc, target) ->
         Hashtbl.replace loc_to_deprecated_constructor_constructor loc target);

  let find_constructor_target ~loc ~lid_loc =
    match Hashtbl.find_opt loc_to_deprecated_constructor_constructor loc with
    | Some _ as found -> found
    | None -> Hashtbl.find_opt loc_to_deprecated_constructor_constructor lid_loc
  in

  (* Helpers for type replacement lookups *)
  let loc_contains (a : Location.t) (b : Location.t) =
    let a_start = a.Location.loc_start.pos_cnum in
    let a_end = a.Location.loc_end.pos_cnum in
    let b_start = b.Location.loc_start.pos_cnum in
    let b_end = b.Location.loc_end.pos_cnum in
    a_start <= b_start && a_end >= b_end
  in
  (* Prefilter deprecations that have a %replace.type(: <core_type>) payload. *)
  let type_replace_deprecations :
      (Cmt_utils.deprecated_used * Parsetree.core_type) list =
    deprecated_used
    |> List.filter_map (fun (d : Cmt_utils.deprecated_used) ->
           match d.migration_template with
           | Some e -> (
             match TypeReplace.core_type_of_expr_extension e with
             | Some ct -> Some (d, ct)
             | None -> None)
           | None -> None)
  in
  let find_type_replace_template (loc : Location.t) : Parsetree.core_type option
      =
    type_replace_deprecations
    |> List.find_map (fun ((d : Cmt_utils.deprecated_used), ct) ->
           if loc_contains loc d.source_loc || loc_contains d.source_loc loc
           then Some ct
           else None)
  in

  let mapper =
    {
      Ast_mapper.default_mapper with
      extension = remap_needed_extensions;
      (* Replace deprecated type references when a %replace.type(: ...) template
         is provided. *)
      typ =
        (fun mapper (ct : Parsetree.core_type) ->
          match ct.ptyp_desc with
          | Ptyp_constr ({loc}, args) -> (
            match find_type_replace_template loc with
            | Some template_ct -> (
              (* Transfer all source type arguments as-is. *)
              let mapped_args = List.map (mapper.Ast_mapper.typ mapper) args in
              match template_ct.ptyp_desc with
              | Ptyp_constr (new_lid, templ_args) ->
                let new_args = templ_args @ mapped_args in
                let ct' =
                  {ct with ptyp_desc = Ptyp_constr (new_lid, new_args)}
                in
                mapper.Ast_mapper.typ mapper ct'
              | _ ->
                (* If the template isn't a constructor, fall back to the
                     template itself and drop the original args. *)
                let ct' = {template_ct with ptyp_loc = ct.ptyp_loc} in
                mapper.Ast_mapper.typ mapper ct')
            | None -> Ast_mapper.default_mapper.typ mapper ct)
          | _ -> Ast_mapper.default_mapper.typ mapper ct);
      expr =
        (fun mapper exp ->
          match exp with
          | {pexp_desc = Pexp_ident {loc}}
            when Hashtbl.mem loc_to_deprecated_reference loc ->
            let deprecated_info =
              Hashtbl.find loc_to_deprecated_reference loc
            in
            migrate_reference_from_info deprecated_info exp
          | {
           pexp_desc =
             Pexp_apply {funct = {pexp_loc = fn_loc}; args = call_args};
          }
            when Hashtbl.mem loc_to_deprecated_fn_call fn_loc -> (
            let deprecated_info =
              Hashtbl.find loc_to_deprecated_fn_call fn_loc
            in
            let call_args = ArgUtils.map_expr_args mapper call_args in
            match deprecated_info.migration_template with
            | Some e -> apply_template_direct mapper e call_args exp
            | None -> exp)
          | {pexp_desc = Pexp_construct (lid, arg); pexp_loc} -> (
            match find_constructor_target ~loc:pexp_loc ~lid_loc:lid.loc with
            | Some {ConstructorReplace.lid; attrs} ->
              let arg = Option.map (mapper.expr mapper) arg in
              let replaced = {exp with pexp_desc = Pexp_construct (lid, arg)} in
              MapperUtils.ApplyTransforms.attach_to_replacement ~attrs replaced
            | None -> Ast_mapper.default_mapper.expr mapper exp)
          | {
           pexp_desc =
             Pexp_apply
               {
                 funct = {pexp_desc = Pexp_ident {txt = Lident "->"}} as funct;
                 args = (lhs_label, lhs_exp) :: (Nolabel, rhs) :: _;
               };
          } -> (
            let lhs_exp = mapper.expr mapper lhs_exp in
            let lhs = (lhs_label, lhs_exp) in
            let fn_loc_opt, pipe_args =
              match rhs with
              | {pexp_loc = fn_loc; pexp_desc = Pexp_ident _} ->
                (Some fn_loc, [])
              | {
               pexp_desc =
                 Pexp_apply
                   {
                     funct = {pexp_loc = fn_loc; pexp_desc = Pexp_ident _};
                     args = pipe_args;
                   };
              } ->
                (Some fn_loc, pipe_args)
              | _ -> (None, [])
            in
            match fn_loc_opt with
            | None -> Ast_mapper.default_mapper.expr mapper exp
            | Some fn_loc when Hashtbl.mem loc_to_deprecated_fn_call fn_loc ->
              let deprecated_info =
                Hashtbl.find loc_to_deprecated_fn_call fn_loc
              in
              apply_single_step_or_piped ~mapper ~deprecated_info ~lhs ~lhs_exp
                ~pipe_args ~funct exp
            | Some _ -> Ast_mapper.default_mapper.expr mapper exp)
          | _ -> Ast_mapper.default_mapper.expr mapper exp);
      pat =
        (fun mapper pat ->
          match pat with
          | {ppat_desc = Ppat_construct (lid, arg); ppat_loc} -> (
            match find_constructor_target ~loc:ppat_loc ~lid_loc:lid.loc with
            | Some {ConstructorReplace.lid; attrs} ->
              let arg = Option.map (mapper.pat mapper) arg in
              let replaced = {pat with ppat_desc = Ppat_construct (lid, arg)} in
              MapperUtils.ApplyTransforms.attach_attrs_to_pat ~attrs replaced
            | None -> Ast_mapper.default_mapper.pat mapper pat)
          | _ -> Ast_mapper.default_mapper.pat mapper pat);
    }
  in
  mapper

let migrate ~entryPointFile ~outputMode =
  let path =
    match Filename.is_relative entryPointFile with
    | true -> Unix.realpath entryPointFile
    | false -> entryPointFile
  in
  let result =
    if Filename.check_suffix path ".res" then
      let parser =
        Res_driver.parsing_engine.parse_implementation ~for_printer:true
      in
      let {Res_driver.parsetree; comments; source} = parser ~filename:path in
      match Cmt.loadCmtInfosFromPath ~path with
      | None ->
        Error
          (Printf.sprintf
             "error: failed to run migration for %s because build artifacts \
              could not be found. try to build the project"
             path)
      | Some {cmt_extra_info = {deprecated_used}} ->
        let mapper = makeMapper deprecated_used in
        let astMapped = mapper.structure mapper parsetree in
        (* Second pass: apply any post-migration transforms signaled via @apply.transforms *)
        let apply_transforms =
          let expr mapper (e : Parsetree.expression) =
            let e = Ast_mapper.default_mapper.expr mapper e in
            MapperUtils.ApplyTransforms.apply_on_self e
          in
          {Ast_mapper.default_mapper with expr}
        in
        let astTransformed =
          apply_transforms.structure apply_transforms astMapped
        in
        Ok
          ( Res_printer.print_implementation
              ~width:Res_printer.default_print_width astTransformed ~comments,
            source )
    else if Filename.check_suffix path ".resi" then
      let parser =
        Res_driver.parsing_engine.parse_interface ~for_printer:true
      in
      let {Res_driver.parsetree = signature; comments; source} =
        parser ~filename:path
      in

      match Cmt.loadCmtInfosFromPath ~path with
      | None ->
        Error
          (Printf.sprintf
             "error: failed to run migration for %s because build artifacts \
              could not be found. try to build the project"
             path)
      | Some {cmt_extra_info = {deprecated_used}} ->
        let mapper = makeMapper deprecated_used in
        let astMapped = mapper.signature mapper signature in
        Ok (Res_printer.print_interface astMapped ~comments, source)
    else
      Error
        (Printf.sprintf
           "File extension not supported. This command accepts .res and .resi \
            files")
  in
  match result with
  | Error e -> Error e
  | Ok (contents, source) when contents <> source -> (
    match outputMode with
    | `Stdout -> Ok contents
    | `File ->
      let oc = open_out path in
      Printf.fprintf oc "%s" contents;
      close_out oc;
      Ok (Filename.basename path ^ ": File migrated successfully"))
  | Ok (contents, _) -> (
    match outputMode with
    | `Stdout -> Ok contents
    | `File -> Ok (Filename.basename path ^ ": File did not need migration"))
