open GenTypeCommon

type translation = {dependencies: dep list; type_: type_}

let rec remove_option ~(label : Asttypes.Noloc.arg_label)
    (type_expr : Types.type_expr) =
  match (type_expr.desc, label) with
  | Tconstr (Path.Pident id, [t], _), Optional lbl when Ident.name id = "option"
    ->
    Some (lbl, t)
  | Tconstr (Pdot (Path.Pident name_space, id, _), [t], _), Optional lbl
    when Ident.name name_space = "FB" && id = "option" ->
    Some (lbl, t)
  | Tlink t, _ -> t |> remove_option ~label
  | _ -> None

let rec path_to_list path =
  match path with
  | Path.Pident id -> [id |> Ident.name]
  | Path.Pdot (p, s, _) -> s :: (p |> path_to_list)
  | Path.Papply _ -> []

let translate_obj_type closed_flag fields_translations =
  let dependencies =
    fields_translations
    |> List.map (fun (_, {dependencies}) -> dependencies)
    |> List.concat
  in
  let rec check_mutable_field ?(acc = []) fields =
    match fields with
    | (previous_name, {type_ = _}) :: (name, {type_}) :: rest
      when Runtime.check_mutable_object_field ~previous_name ~name ->
      (* The field was annotated "@set" *)
      rest |> check_mutable_field ~acc:((name, type_, Mutable) :: acc)
    | (name, {type_}) :: rest ->
      rest |> check_mutable_field ~acc:((name, type_, Immutable) :: acc)
    | [] -> acc |> List.rev
  in
  let fields =
    fields_translations |> check_mutable_field
    |> List.map (fun (name, t, mutable_) ->
           let optional, type_ =
             match t with
             | Option t -> (Optional, t)
             | _ -> (Mandatory, t)
           in
           {
             mutable_;
             name_js = name;
             optional;
             type_;
             doc_string = DocString.empty;
           })
  in
  let type_ = Object (closed_flag, fields) in
  {dependencies; type_}

let translate_constr ~config ~params_translation ~(path : Path.t) ~type_env =
  let default_case () =
    let type_args =
      params_translation |> List.map (fun ({type_} : translation) -> type_)
    in
    let type_param_deps =
      params_translation
      |> List.map (fun {dependencies} -> dependencies)
      |> List.concat
    in
    match type_env |> TypeEnv.apply_type_equations ~config ~path with
    | Some type_ -> {dependencies = type_param_deps; type_}
    | None ->
      let dep = path |> Dependencies.from_path ~config ~type_env in
      {
        dependencies = dep :: type_param_deps;
        type_ = Ident {builtin = false; name = dep |> dep_to_string; type_args};
      }
  in
  match (path |> path_to_list |> List.rev, params_translation) with
  | (["FB"; "bool"] | ["bool"]), [] -> {dependencies = []; type_ = boolean_t}
  | (["FB"; "int"] | ["int"]), [] -> {dependencies = []; type_ = number_t}
  | (["FB"; "float"] | ["float"]), [] -> {dependencies = []; type_ = number_t}
  | ( ( ["FB"; "string"]
      | ["string"]
      | ["String"; "t"]
      | ["Stdlib"; "String"; "t"]
      | ["Js"; ("String" | "String2"); "t"] ),
      [] ) ->
    {dependencies = []; type_ = string_t}
  | ( ( ["Js"; "Types"; "bigint_val"]
      | ["BigInt"; "t"]
      | ["Stdlib"; "BigInt"; "t"] ),
      [] ) ->
    {dependencies = []; type_ = bigint_t}
  | (["Js"; "Date"; "t"] | ["Date"; "t"] | ["Stdlib"; "Date"; "t"]), [] ->
    {dependencies = []; type_ = date_t}
  | ( (["Map"; "t"] | ["Stdlib"; "Map"; "t"]),
      [param_translation1; param_translation2] ) ->
    {
      dependencies =
        param_translation1.dependencies @ param_translation2.dependencies;
      type_ = map_t (param_translation1.type_, param_translation2.type_);
    }
  | ( (["WeakMap"; "t"] | ["Stdlib"; "WeakMap"; "t"]),
      [param_translation1; param_translation2] ) ->
    {
      dependencies =
        param_translation1.dependencies @ param_translation2.dependencies;
      type_ = weakmap_t (param_translation1.type_, param_translation2.type_);
    }
  | (["Set"; "t"] | ["Stdlib"; "Set"; "t"]), [param_translation] ->
    {
      dependencies = param_translation.dependencies;
      type_ = set_t param_translation.type_;
    }
  | (["WeakSet"; "t"] | ["Stdlib"; "WeakSet"; "t"]), [param_translation] ->
    {
      dependencies = param_translation.dependencies;
      type_ = weakset_t param_translation.type_;
    }
  | (["Js"; "Re"; "t"] | ["RegExp"; "t"] | ["Stdlib"; "RegExp"; "t"]), [] ->
    {dependencies = []; type_ = regexp_t}
  | (["FB"; "unit"] | ["unit"]), [] -> {dependencies = []; type_ = unit_t}
  | ( (["FB"; "array"] | ["array"] | ["Js"; ("Array" | "Array2"); "t"]),
      [param_translation] ) ->
    {param_translation with type_ = Array (param_translation.type_, Mutable)}
  | ["ImmutableArray"; "t"], [param_translation] ->
    {param_translation with type_ = Array (param_translation.type_, Immutable)}
  | ["Pervasives"; "ref"], [param_translation] ->
    {
      dependencies = param_translation.dependencies;
      type_ =
        Object
          ( Closed,
            [
              {
                mutable_ = Mutable;
                name_js = "contents";
                optional = Mandatory;
                type_ = param_translation.type_;
                doc_string = DocString.empty;
              };
            ] );
    }
  | ( ( ["Pervasives"; "result"]
      | ["Belt"; "Result"; "t"]
      | ["result"]
      | ["Stdlib"; "Result"; "t"] ),
      [param_translation1; param_translation2] ) ->
    let case name type_ = {case = {label_js = StringLabel name}; t = type_} in
    let variant =
      create_variant ~inherits:[] ~no_payloads:[]
        ~payloads:
          [
            case "Ok" param_translation1.type_;
            case "Error" param_translation2.type_;
          ]
        ~polymorphic:false ~tag:None ~unboxed:false
    in
    {
      dependencies =
        param_translation1.dependencies @ param_translation2.dependencies;
      type_ = variant;
    }
  | ["React"; "callback"], [from_translation; to_translation] ->
    {
      dependencies = from_translation.dependencies @ to_translation.dependencies;
      type_ =
        Function
          {
            arg_types = [{a_name = ""; a_type = from_translation.type_}];
            ret_type = to_translation.type_;
            type_vars = [];
          };
    }
  | ["React"; "componentLike"], [props_translation; ret_translation] ->
    {
      dependencies =
        props_translation.dependencies @ ret_translation.dependencies;
      type_ =
        Function
          {
            arg_types = [{a_name = ""; a_type = props_translation.type_}];
            ret_type = ret_translation.type_;
            type_vars = [];
          };
    }
  | ["React"; "component"], [props_translation] ->
    {
      dependencies = props_translation.dependencies;
      type_ =
        Function
          {
            arg_types = [{a_name = ""; a_type = props_translation.type_}];
            ret_type = EmitType.type_react_element;
            type_vars = [];
          };
    }
  | ["React"; "Context"; "t"], [param_translation] ->
    {
      dependencies = param_translation.dependencies;
      type_ = EmitType.type_react_context ~type_:param_translation.type_;
    }
  | (["React"; "Ref"; "t"] | ["React"; "ref"]), [param_translation] ->
    {
      dependencies = param_translation.dependencies;
      type_ = EmitType.type_react_ref ~type_:param_translation.type_;
    }
  | (["ReactDOM"; "domRef"] | ["ReactDOM"; "Ref"; "t"]), [] ->
    {dependencies = []; type_ = EmitType.type_react_d_o_m_re_dom_ref}
  | ["ReactDOM"; "Ref"; "currentDomRef"], [] ->
    {dependencies = []; type_ = EmitType.type_any}
  | ["ReactDOMRe"; "domRef"], [] ->
    {dependencies = []; type_ = EmitType.type_react_d_o_m_re_dom_ref}
  | ["ReactDOMRe"; "Ref"; "currentDomRef"], [] ->
    {dependencies = []; type_ = EmitType.type_any}
  | ["ReactEvent"; "Mouse"; "t"], [] ->
    {dependencies = []; type_ = EmitType.type_react_event_mouse_t}
  | ( ( ["React"; "element"]
      | ["ReasonReact"; "reactElement"]
      | ["Pervasives"; "Jsx"; "element"]
      | ["Jsx"; "element"] ),
      [] ) ->
    {dependencies = []; type_ = EmitType.type_react_element}
  | (["FB"; "option"] | ["option"]), [param_translation] ->
    {param_translation with type_ = Option param_translation.type_}
  | ( (["Js"; "Undefined"; "t"] | ["Undefined"; "t"] | ["Js"; "undefined"]),
      [param_translation] ) ->
    {param_translation with type_ = Option param_translation.type_}
  | ( ( ["Js"; "Null"; "t"]
      | ["Null"; "t"]
      | ["Js"; "null"]
      | ["Stdlib"; "Null"; "t"] ),
      [param_translation] ) ->
    {param_translation with type_ = Null param_translation.type_}
  | ( ( ["Js"; "Nullable"; "t"]
      | ["Nullable"; "t"]
      | ["Js"; "nullable"]
      | ["Js"; "Null_undefined"; "t"]
      | ["Js"; "null_undefined"]
      | ["Stdlib"; "Nullable"; "t"] ),
      [param_translation] ) ->
    {param_translation with type_ = Nullable param_translation.type_}
  | ( ( ["Js"; "Promise"; "t"]
      | ["Promise"; "t"]
      | ["promise"]
      | ["Stdlib"; "Promise"; "t"] ),
      [param_translation] ) ->
    {param_translation with type_ = Promise param_translation.type_}
  | ( (["Js"; "Dict"; "t"] | ["Dict"; "t"] | ["dict"] | ["Stdlib"; "Dict"; "t"]),
      [param_translation] ) ->
    {param_translation with type_ = Dict param_translation.type_}
  | _ -> default_case ()

type process_variant = {
  no_payloads: string list;
  payloads: (string * Types.type_expr) list;
  unknowns: string list;
}

let process_variant row_fields =
  let rec loop ~no_payloads ~payloads ~unknowns fields =
    match fields with
    | ( label,
        ( Types.Rpresent (* no payload *) None
        | Reither ((* constant constructor *) true, _, _, _) ) )
      :: other_fields ->
      other_fields
      |> loop ~no_payloads:(label :: no_payloads) ~payloads ~unknowns
    | (label, Rpresent (Some payload)) :: other_fields ->
      other_fields
      |> loop ~no_payloads ~payloads:((label, payload) :: payloads) ~unknowns
    | (label, (Rabsent | Reither (false, _, _, _))) :: other_fields ->
      other_fields |> loop ~no_payloads ~payloads ~unknowns:(label :: unknowns)
    | [] ->
      {
        no_payloads = no_payloads |> List.rev;
        payloads = payloads |> List.rev;
        unknowns = unknowns |> List.rev;
      }
  in
  row_fields |> loop ~no_payloads:[] ~payloads:[] ~unknowns:[]

let rec translate_arrow_type ~config ~type_vars_gen ~type_env ~rev_arg_deps
    ~rev_args (type_expr : Types.type_expr) =
  match type_expr.desc with
  | Tlink t ->
    translate_arrow_type ~config ~type_vars_gen ~type_env ~rev_arg_deps
      ~rev_args t
  | Tarrow (Nolabel, type_expr1, type_expr2, _, arity)
    when arity = None || rev_args = [] ->
    let {dependencies; type_} =
      type_expr1 |> fun __x ->
      translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env __x
    in
    let next_rev_deps = List.rev_append dependencies rev_arg_deps in
    type_expr2
    |> translate_arrow_type ~config ~type_vars_gen ~type_env
         ~rev_arg_deps:next_rev_deps
         ~rev_args:((Nolabel, type_) :: rev_args)
  | Tarrow
      ( ((Labelled lbl | Optional lbl) as label),
        type_expr1,
        type_expr2,
        _,
        arity )
    when arity = None || rev_args = [] -> (
    match type_expr1 |> remove_option ~label with
    | None ->
      let {dependencies; type_ = type1} =
        type_expr1
        |> translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
      in
      let next_rev_deps = List.rev_append dependencies rev_arg_deps in
      type_expr2
      |> translate_arrow_type ~config ~type_vars_gen ~type_env
           ~rev_arg_deps:next_rev_deps
           ~rev_args:((Label lbl, type1) :: rev_args)
    | Some (lbl, t1) ->
      let {dependencies; type_ = type1} =
        t1 |> translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
      in
      let next_rev_deps = List.rev_append dependencies rev_arg_deps in
      type_expr2
      |> translate_arrow_type ~config ~type_vars_gen ~type_env
           ~rev_arg_deps:next_rev_deps
           ~rev_args:((OptLabel lbl, type1) :: rev_args))
  | _ ->
    let {dependencies; type_ = ret_type} =
      type_expr |> translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
    in
    let all_deps = List.rev_append rev_arg_deps dependencies in
    let labeled_convertable_types = rev_args |> List.rev in
    let arg_types = labeled_convertable_types |> NamedArgs.group in
    let function_type = Function {arg_types; ret_type; type_vars = []} in
    {dependencies = all_deps; type_ = function_type}

and translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
    (type_expr : Types.type_expr) =
  match type_expr.desc with
  | Tvar None ->
    let type_name =
      GenIdent.js_type_name_for_anonymous_type_id ~type_vars_gen type_expr.id
    in
    {dependencies = []; type_ = TypeVar type_name}
  | Tvar (Some s) -> {dependencies = []; type_ = TypeVar s}
  | Tconstr
      (Pdot (Pident {name = "Js"}, "t", _), [{desc = Tvar _ | Tconstr _}], _) ->
    (* Preserve some existing uses of Js.t(Obj.t) and Js.t('a). *)
    translate_obj_type Closed []
  | Tconstr (Pdot (Pident {name = "Js"}, "t", _), [t], _) ->
    t |> translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
  | Tobject (t_obj, _) ->
    let rec get_field_types (texp : Types.type_expr) =
      match texp.desc with
      | Tfield (name, _, t1, t2) ->
        let closed_flafg, fields = t2 |> get_field_types in
        ( closed_flafg,
          ( name,
            match name |> Runtime.is_mutable_object_field with
            | true -> {dependencies = []; type_ = ident ""}
            | false ->
              t1 |> translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
          )
          :: fields )
      | Tlink te -> te |> get_field_types
      | Tvar None -> (Open, [])
      | _ -> (Closed, [])
    in
    let closed_flag, fields_translations = t_obj |> get_field_types in
    translate_obj_type closed_flag fields_translations
  | Tconstr (path, [{desc = Tlink te}], r) ->
    {type_expr with desc = Types.Tconstr (path, [te], r)}
    |> translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
  | Tconstr (path, type_params, _) ->
    let params_translation =
      type_params
      |> translateTypeExprsFromTypes_ ~config ~type_vars_gen ~type_env
    in
    translate_constr ~config ~params_translation ~path ~type_env
  | Tpoly (t, []) ->
    t |> translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
  | Tarrow _ ->
    type_expr
    |> translate_arrow_type ~config ~type_vars_gen ~type_env ~rev_arg_deps:[]
         ~rev_args:[]
  | Ttuple list_exp ->
    let inner_types_translation =
      list_exp |> translateTypeExprsFromTypes_ ~config ~type_vars_gen ~type_env
    in
    let inner_types =
      inner_types_translation |> List.map (fun {type_} -> type_)
    in
    let inner_types_deps =
      inner_types_translation
      |> List.map (fun {dependencies} -> dependencies)
      |> List.concat
    in
    let tuple_type = Tuple inner_types in
    {dependencies = inner_types_deps; type_ = tuple_type}
  | Tlink t -> t |> translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
  | Tvariant row_desc -> (
    match row_desc.row_fields |> process_variant with
    | {no_payloads; payloads = []; unknowns = []} ->
      let no_payloads =
        no_payloads
        |> List.map (fun label ->
               {
                 label_js =
                   (if is_number label then IntLabel label
                    else StringLabel label);
               })
      in
      let type_ =
        create_variant ~inherits:[] ~no_payloads ~payloads:[] ~polymorphic:true
          ~tag:None ~unboxed:false
      in
      {dependencies = []; type_}
    | {no_payloads = []; payloads = [(_label, t)]; unknowns = []} ->
      (* Handle ReScript's "Arity_" encoding in first argument of Js.Internal.fn(_,_) for uncurried functions.
         Return the argument tuple. *)
      t |> translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
    | {no_payloads; payloads; unknowns = []} ->
      let no_payloads =
        no_payloads |> List.map (fun label -> {label_js = StringLabel label})
      in
      let payload_translations =
        payloads
        |> List.map (fun (label, payload) ->
               ( label,
                 payload
                 |> translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
               ))
      in
      let payloads =
        payload_translations
        |> List.map (fun (label, translation) ->
               {case = {label_js = StringLabel label}; t = translation.type_})
      in
      let type_ =
        create_variant ~inherits:[] ~no_payloads ~payloads ~polymorphic:true
          ~tag:None ~unboxed:false
      in
      let dependencies =
        payload_translations
        |> List.map (fun (_, {dependencies}) -> dependencies)
        |> List.concat
      in
      {dependencies; type_}
    | {unknowns = _ :: _} -> {dependencies = []; type_ = unknown})
  | Tpackage (path, ids, types) -> (
    match type_env |> TypeEnv.lookup_module_type_signature ~path with
    | Some (signature, type_env) ->
      let type_equations_translation =
        (List.combine ids types [@doesNotRaise])
        |> List.map (fun (x, t) ->
               ( x,
                 t
                 |> translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
               ))
      in
      let type_equations =
        type_equations_translation
        |> List.map (fun (x, translation) -> (x, translation.type_))
      in
      let dependencies_from_type_equations =
        type_equations_translation
        |> List.map (fun (_, translation) -> translation.dependencies)
        |> List.flatten
      in
      let type_env1 = type_env |> TypeEnv.add_type_equations ~type_equations in
      let dependencies_from_record_type, type_ =
        signature.sig_type
        |> signature_to_module_runtime_representation ~config ~type_vars_gen
             ~type_env:type_env1
      in
      {
        dependencies =
          dependencies_from_type_equations @ dependencies_from_record_type;
        type_;
      }
    | None -> {dependencies = []; type_ = unknown})
  | Tfield _ | Tnil | Tpoly _ | Tsubst _ | Tunivar _ ->
    {dependencies = []; type_ = unknown}

and translateTypeExprsFromTypes_ ~config ~type_vars_gen ~type_env type_exprs :
    translation list =
  type_exprs
  |> List.map (translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env)

and signature_to_module_runtime_representation ~config ~type_vars_gen ~type_env
    signature =
  let dependencies_and_fields =
    signature
    |> List.map (fun signature_item ->
           match signature_item with
           | Types.Sig_value (_id, {val_kind = Val_prim _}) -> ([], [])
           | Types.Sig_value (id, {val_type = type_expr; val_attributes}) ->
             let {dependencies; type_} =
               type_expr
               |> translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
             in
             let field =
               {
                 mutable_ = Immutable;
                 name_js = id |> Ident.name;
                 optional = Mandatory;
                 type_;
                 doc_string = Annotation.doc_string_from_attrs val_attributes;
               }
             in
             (dependencies, [field])
           | Types.Sig_module (id, module_declaration, _recStatus) ->
             let type_env1 =
               match
                 type_env |> TypeEnv.get_module ~name:(id |> Ident.name)
               with
               | Some type_env1 -> type_env1
               | None -> type_env
             in
             let dependencies, type_ =
               match module_declaration.md_type with
               | Mty_signature signature ->
                 signature
                 |> signature_to_module_runtime_representation ~config
                      ~type_vars_gen ~type_env:type_env1
               | Mty_ident _ | Mty_functor _ | Mty_alias _ -> ([], unknown)
             in
             let field =
               {
                 mutable_ = Immutable;
                 name_js = id |> Ident.name;
                 optional = Mandatory;
                 type_;
                 doc_string =
                   Annotation.doc_string_from_attrs
                     module_declaration.md_attributes;
               }
             in
             (dependencies, [field])
           | Types.Sig_type _ | Types.Sig_typext _ | Types.Sig_modtype _
           | Types.Sig_class _ | Types.Sig_class_type _ ->
             ([], []))
  in
  let dependencies, fields =
    let dl, fl = dependencies_and_fields |> List.split in
    (dl |> List.concat, fl |> List.concat)
  in
  (dependencies, Object (Closed, fields))

let translate_type_expr_from_types ~config ~type_env type_expr =
  let type_vars_gen = GenIdent.create_type_vars_gen () in
  let translation =
    type_expr |> translateTypeExprFromTypes_ ~config ~type_vars_gen ~type_env
  in
  if !Debug.dependencies then
    translation.dependencies
    |> List.iter (fun dep ->
           Log_.item "Dependency: %s\n" (dep |> dep_to_string));
  translation

let translate_type_exprs_from_types ~config ~type_env type_exprs =
  let type_vars_gen = GenIdent.create_type_vars_gen () in
  let translations =
    type_exprs |> translateTypeExprsFromTypes_ ~config ~type_vars_gen ~type_env
  in
  if !Debug.dependencies then
    translations
    |> List.iter (fun translation ->
           translation.dependencies
           |> List.iter (fun dep ->
                  Log_.item "Dependency: %s\n" (dep |> dep_to_string)));
  translations
