open! Ast_helper
open Ast_mapper
open Asttypes
open Parsetree
open Longident

let module_access_name config value =
  String.capitalize_ascii config.Jsx_common.module_ ^ "." ^ value
  |> Longident.parse

let nolabel = Nolabel

let is_optional str =
  match str with
  | Optional _ -> true
  | _ -> false

let is_labelled str =
  match str with
  | Labelled _ -> true
  | _ -> false

let is_forward_ref = function
  | {pexp_desc = Pexp_ident {txt = Ldot (Lident "React", "forwardRef")}} -> true
  | _ -> false

let get_label str =
  match str with
  | Optional {txt = str} | Labelled {txt = str} -> str
  | Nolabel -> ""

let constant_string ~loc str =
  Ast_helper.Exp.constant ~loc (Pconst_string (str, None))

let unit_expr ~loc = Exp.construct ~loc (Location.mkloc (Lident "()") loc) None

let safe_type_from_value value_str =
  let value_str = get_label value_str in
  if value_str = "" || (value_str.[0] [@doesNotRaise]) <> '_' then value_str
  else "T" ^ value_str

let ref_type_var loc = Typ.var ~loc "ref"

let ref_type loc =
  Typ.constr ~loc
    {loc; txt = Ldot (Ldot (Lident "Js", "Nullable"), "t")}
    [ref_type_var loc]

let merlin_focus = ({loc = Location.none; txt = "merlin.focus"}, PStr [])

(* Helper method to filter out any attribute that isn't [@react.component] *)
let other_attrs_pure (loc, _) =
  match loc.txt with
  | "react.component" | "jsx.component" | "react.componentWithProps"
  | "jsx.componentWithProps" ->
    false
  | _ -> true

(* Finds the name of the variable the binding is assigned to, otherwise raises Invalid_argument *)
let rec get_fn_name binding =
  match binding with
  | {ppat_desc = Ppat_var {txt}} -> txt
  | {ppat_desc = Ppat_constraint (pat, _)} -> get_fn_name pat
  | {ppat_loc} ->
    Jsx_common.raise_error ~loc:ppat_loc
      "JSX component calls cannot be destructured."

let make_new_binding binding expression new_name =
  match binding with
  | {pvb_pat = {ppat_desc = Ppat_var ppat_var} as pvb_pat} ->
    {
      binding with
      pvb_pat =
        {pvb_pat with ppat_desc = Ppat_var {ppat_var with txt = new_name}};
      pvb_expr = expression;
      pvb_attributes = [merlin_focus];
    }
  | {pvb_loc} ->
    Jsx_common.raise_error ~loc:pvb_loc
      "JSX component calls cannot be destructured."

(* Lookup the filename from the location information on the AST node and turn it into a valid module identifier *)
let filename_from_loc (pstr_loc : Location.t) =
  let file_name =
    match pstr_loc.loc_start.pos_fname with
    | "" -> !Location.input_name
    | file_name -> file_name
  in
  let file_name =
    try Filename.chop_extension (Filename.basename file_name)
    with Invalid_argument _ -> file_name
  in
  let file_name = String.capitalize_ascii file_name in
  file_name

(* Build a string representation of a module name with segments separated by $ *)
let make_module_name file_name nested_modules fn_name =
  let full_module_name =
    match (file_name, nested_modules, fn_name) with
    (* TODO: is this even reachable? It seems like the fileName always exists *)
    | "", nested_modules, "make" -> nested_modules
    | "", nested_modules, fn_name -> List.rev (fn_name :: nested_modules)
    | file_name, nested_modules, "make" -> file_name :: List.rev nested_modules
    | file_name, nested_modules, fn_name ->
      file_name :: List.rev (fn_name :: nested_modules)
  in
  let full_module_name = String.concat "$" full_module_name in
  full_module_name

(* make type params for make fn arguments *)
(* let make = ({id, name, children}: props<'id, 'name, 'children>) *)
let make_props_type_params_tvar named_type_list =
  named_type_list
  |> List.filter_map (fun (_isOptional, label, _, loc, _interiorType) ->
         if label = "key" then None
         else
           Some
             (Typ.var ~loc
             @@ safe_type_from_value
                  (Labelled {txt = label; loc = Location.none})))

let strip_option core_type =
  match core_type with
  | {ptyp_desc = Ptyp_constr ({txt = Lident "option"}, core_types)} ->
    List.nth_opt core_types 0 [@doesNotRaise]
  | _ -> Some core_type

let strip_js_nullable core_type =
  match core_type with
  | {
   ptyp_desc =
     Ptyp_constr ({txt = Ldot (Ldot (Lident "Js", "Nullable"), "t")}, core_types);
  } ->
    List.nth_opt core_types 0 [@doesNotRaise]
  | _ -> Some core_type

(* Make type params of the props type *)
(* (Sig) let make: React.componentLike<props<string>, React.element> *)
(* (Str) let make = ({x, _}: props<'x>) => body *)
(* (Str) external make: React.componentLike<props< .. >, React.element> = "default" *)
let make_props_type_params ?(strip_explicit_option = false)
    ?(strip_explicit_js_nullable_of_ref = false) named_type_list =
  named_type_list
  |> List.filter_map (fun (is_optional, label, _, loc, interior_type) ->
         if label = "key" then None
           (* TODO: Worth thinking how about "ref_" or "_ref" usages *)
         else if label = "ref" then
           (*
                If ref has a type annotation then use it, else 'ref.
                For example, if JSX ppx is used for React Native, type would be different.
             *)
           match interior_type with
           | {ptyp_desc = Ptyp_any} -> Some (ref_type_var loc)
           | _ ->
             (* Strip explicit Js.Nullable.t in case of forwardRef *)
             if strip_explicit_js_nullable_of_ref then
               strip_js_nullable interior_type
             else Some interior_type
           (* Strip the explicit option type in implementation *)
           (* let make = (~x: option<string>=?) => ... *)
         else if is_optional && strip_explicit_option then
           strip_option interior_type
         else Some interior_type)

let make_label_decls named_type_list =
  let rec check_duplicated_label l =
    let rec mem_label ((_, (la : string), _, _, _) as x) = function
      | [] -> false
      | (_, (lb : string), _, _, _) :: l -> lb = la || mem_label x l
    in
    match l with
    | [] -> ()
    | hd :: tl ->
      if mem_label hd tl then
        let _, label, _, loc, _ = hd in
        Jsx_common.raise_error ~loc
          "The prop `%s` is defined several times in this component." label
      else check_duplicated_label tl
  in
  let () = named_type_list |> List.rev |> check_duplicated_label in

  named_type_list
  |> List.map (fun (is_optional, label, attrs, loc, interior_type) ->
         if label = "key" then
           Type.field ~loc ~attrs ~optional:true {txt = label; loc}
             interior_type
         else if is_optional then
           Type.field ~loc ~attrs ~optional:true {txt = label; loc}
             (Typ.var @@ safe_type_from_value
             @@ Labelled {txt = label; loc = Location.none})
         else
           Type.field ~loc ~attrs {txt = label; loc}
             (Typ.var @@ safe_type_from_value
             @@ Labelled {txt = label; loc = Location.none}))

let make_type_decls ~attrs props_name loc named_type_list =
  let label_decl_list = make_label_decls named_type_list in
  (* 'id, 'className, ... *)
  let params =
    make_props_type_params_tvar named_type_list
    |> List.map (fun core_type -> (core_type, Invariant))
  in
  [
    Type.mk ~attrs ~loc ~params {txt = props_name; loc}
      ~kind:(Ptype_record label_decl_list);
  ]

let make_type_decls_with_core_type props_name loc core_type typ_vars =
  [
    Type.mk ~loc {txt = props_name; loc} ~kind:Ptype_abstract
      ~params:(typ_vars |> List.map (fun v -> (v, Invariant)))
      ~manifest:core_type;
  ]

let live_attr = ({txt = "live"; loc = Location.none}, PStr [])
let jsx_component_props_attr =
  ({txt = "res.jsxComponentProps"; loc = Location.none}, PStr [])

(* type props<'x, 'y, ...> = { x: 'x, y?: 'y, ... } *)
let make_props_record_type ~core_type_of_attr ~external_ ~typ_vars_of_core_type
    props_name loc named_type_list =
  let attrs =
    if external_ then [jsx_component_props_attr; live_attr]
    else [jsx_component_props_attr]
  in
  Str.type_ Nonrecursive
    (match core_type_of_attr with
    | None -> make_type_decls ~attrs props_name loc named_type_list
    | Some core_type ->
      make_type_decls_with_core_type props_name loc core_type
        typ_vars_of_core_type)

(* type props<'x, 'y, ...> = { x: 'x, y?: 'y, ... } *)
let make_props_record_type_sig ~core_type_of_attr ~external_
    ~typ_vars_of_core_type props_name loc named_type_list =
  let attrs =
    if external_ then [jsx_component_props_attr; live_attr]
    else [jsx_component_props_attr]
  in
  Sig.type_ Nonrecursive
    (match core_type_of_attr with
    | None -> make_type_decls ~attrs props_name loc named_type_list
    | Some core_type ->
      make_type_decls_with_core_type props_name loc core_type
        typ_vars_of_core_type)

let rec recursively_transform_named_args_for_make expr args newtypes core_type =
  match expr.pexp_desc with
  (* TODO: make this show up with a loc. *)
  | Pexp_fun {arg_label = Labelled {txt = "key"} | Optional {txt = "key"}} ->
    Jsx_common.raise_error ~loc:expr.pexp_loc
      "Key cannot be accessed inside of a component. Don't worry - you can \
       always key a component from its parent!"
  | Pexp_fun {arg_label = Labelled {txt = "ref"} | Optional {txt = "ref"}} ->
    Jsx_common.raise_error ~loc:expr.pexp_loc
      "Ref cannot be passed as a normal prop. Please use `forwardRef` API \
       instead."
  | Pexp_fun {arg_label = arg; default; lhs = pattern; rhs = expression}
    when is_optional arg || is_labelled arg ->
    let () =
      match (is_optional arg, pattern, default) with
      | true, {ppat_desc = Ppat_constraint (_, {ptyp_desc})}, None -> (
        match ptyp_desc with
        | Ptyp_constr ({txt = Lident "option"}, [_]) -> ()
        | _ ->
          let current_type =
            match ptyp_desc with
            | Ptyp_constr ({txt}, []) ->
              String.concat "." (Longident.flatten txt)
            | Ptyp_constr ({txt}, _innerTypeArgs) ->
              String.concat "." (Longident.flatten txt) ^ "(...)"
            | _ -> "..."
          in
          Location.prerr_warning pattern.ppat_loc
            (Preprocessor
               (Printf.sprintf
                  "React: optional argument annotations must have explicit \
                   `option`. Did you mean `option<%s>=?`?"
                  current_type)))
      | _ -> ()
    in
    let alias =
      match pattern with
      | {
       ppat_desc =
         ( Ppat_alias (_, {txt})
         | Ppat_var {txt}
         | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _) );
      } ->
        txt
      | {ppat_desc = Ppat_any} -> "_"
      | _ -> get_label arg
    in
    let type_ =
      match pattern with
      | {ppat_desc = Ppat_constraint (_, {ptyp_desc = Ptyp_package _})} -> None
      | {ppat_desc = Ppat_constraint (_, type_)} -> Some type_
      | _ -> None
    in

    recursively_transform_named_args_for_make expression
      ((arg, default, pattern, alias, pattern.ppat_loc, type_) :: args)
      newtypes core_type
  | Pexp_fun
      {
        arg_label = Nolabel;
        lhs = {ppat_desc = Ppat_construct ({txt = Lident "()"}, _) | Ppat_any};
      } ->
    (args, newtypes, core_type)
  | Pexp_fun
      {
        arg_label = Nolabel;
        lhs =
          {
            ppat_desc =
              Ppat_var {txt} | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _);
          } as pattern;
      } ->
    if txt = "ref" then
      let type_ =
        match pattern with
        | {ppat_desc = Ppat_constraint (_, type_)} -> Some type_
        | _ -> None
      in
      (* The ref arguement of forwardRef should be optional *)
      ( ( Optional {txt = "ref"; loc = Location.none},
          None,
          pattern,
          txt,
          pattern.ppat_loc,
          type_ )
        :: args,
        newtypes,
        core_type )
    else (args, newtypes, core_type)
  | Pexp_fun {arg_label = Nolabel; lhs = pattern} ->
    Location.raise_errorf ~loc:pattern.ppat_loc
      "React: react.component refs only support plain arguments and type \
       annotations."
  | Pexp_newtype (label, expression) ->
    recursively_transform_named_args_for_make expression args
      (label :: newtypes) core_type
  | Pexp_constraint (expression, core_type) ->
    recursively_transform_named_args_for_make expression args newtypes
      (Some core_type)
  | _ -> (args, newtypes, core_type)

let arg_to_type types
    ((name, default, {ppat_attributes = attrs}, _alias, loc, type_) :
      arg_label * expression option * pattern * label * 'loc * core_type option)
    =
  match (type_, name, default) with
  | Some type_, name, _ when is_optional name ->
    (true, get_label name, attrs, loc, type_) :: types
  | Some type_, name, _ -> (false, get_label name, attrs, loc, type_) :: types
  | None, name, _ when is_optional name ->
    (true, get_label name, attrs, loc, Typ.any ~loc ()) :: types
  | None, name, _ when is_labelled name ->
    (false, get_label name, attrs, loc, Typ.any ~loc ()) :: types
  | _ -> types

let has_default_value name_arg_list =
  name_arg_list
  |> List.exists (fun (name, default, _, _, _, _) ->
         Option.is_some default && is_optional name)

let arg_to_concrete_type types (name, attrs, loc, type_) =
  match name with
  | name when is_labelled name ->
    (false, get_label name, attrs, loc, type_) :: types
  | name when is_optional name ->
    (true, get_label name, attrs, loc, type_) :: types
  | _ -> types

let check_string_int_attribute_iter =
  let attribute _ ({txt; loc}, _) =
    if txt = "string" || txt = "int" then
      Jsx_common.raise_error ~loc
        "@string and @int attributes not supported. See \
         https://github.com/rescript-lang/rescript-compiler/issues/5724"
  in

  {Ast_iterator.default_iterator with attribute}

let check_multiple_components ~config ~loc =
  (* If there is another component, throw error *)
  if config.Jsx_common.has_component then
    Jsx_common.raise_error_multiple_component ~loc
  else config.has_component <- true

let modified_binding_old binding =
  let expression = binding.pvb_expr in
  (* TODO: there is a long-tail of unsupported features inside of blocks - Pexp_letmodule , Pexp_letexception , Pexp_ifthenelse *)
  let rec spelunk_for_fun_expression expression =
    match expression with
    (* let make = (~prop) => ... *)
    | {pexp_desc = Pexp_fun _} | {pexp_desc = Pexp_newtype _} -> expression
    (* let make = {let foo = bar in (~prop) => ...} *)
    | {pexp_desc = Pexp_let (_recursive, _vbs, return_expression)} ->
      (* here's where we spelunk! *)
      spelunk_for_fun_expression return_expression
    (* let make = React.forwardRef((~prop) => ...) *)
    | {pexp_desc = Pexp_apply {args = [(Nolabel, inner_function_expression)]}}
      ->
      spelunk_for_fun_expression inner_function_expression
    | {
     pexp_desc = Pexp_sequence (_wrapperExpression, inner_function_expression);
    } ->
      spelunk_for_fun_expression inner_function_expression
    | {pexp_desc = Pexp_constraint (inner_function_expression, _typ)} ->
      spelunk_for_fun_expression inner_function_expression
    | {pexp_loc} ->
      Jsx_common.raise_error ~loc:pexp_loc
        "JSX component calls can only be on function definitions or component \
         wrappers (forwardRef, memo)."
  in
  spelunk_for_fun_expression expression

let modified_binding ~binding_loc ~binding_pat_loc ~fn_name binding =
  let has_application = ref false in
  let wrap_expression_with_binding expression_fn expression =
    Vb.mk ~loc:binding_loc ~attrs:binding.pvb_attributes
      (Pat.var ~loc:binding_pat_loc {loc = binding_pat_loc; txt = fn_name})
      (expression_fn expression)
  in
  let expression = binding.pvb_expr in
  (* TODO: there is a long-tail of unsupported features inside of blocks - Pexp_letmodule , Pexp_letexception , Pexp_ifthenelse *)
  let rec spelunk_for_fun_expression expression =
    match expression with
    (* let make = (~prop) => ... with no final unit *)
    | {
     pexp_desc =
       Pexp_fun
         ({
            arg_label = Labelled _ | Optional _;
            rhs = {pexp_desc = Pexp_fun _} as internal_expression;
          } as f);
    } ->
      let wrap, has_forward_ref, exp =
        spelunk_for_fun_expression internal_expression
      in
      ( wrap,
        has_forward_ref,
        {expression with pexp_desc = Pexp_fun {f with rhs = exp}} )
    (* let make = (()) => ... *)
    (* let make = (_) => ... *)
    | {
     pexp_desc =
       Pexp_fun
         {
           arg_label = Nolabel;
           lhs =
             {ppat_desc = Ppat_construct ({txt = Lident "()"}, _) | Ppat_any};
         };
    } ->
      ((fun a -> a), false, expression)
    (* let make = (~prop) => ... *)
    | {pexp_desc = Pexp_fun {arg_label = Labelled _ | Optional _}} ->
      ((fun a -> a), false, expression)
    (* let make = (prop) => ... *)
    | {pexp_desc = Pexp_fun {lhs = pattern}} ->
      if !has_application then ((fun a -> a), false, expression)
      else
        Location.raise_errorf ~loc:pattern.ppat_loc
          "React: props need to be labelled arguments.\n\
          \  If you are working with refs be sure to wrap with React.forwardRef.\n\
          \  If your component doesn't have any props use () or _ instead of a \
           name."
    (* let make = {let foo = bar in (~prop) => ...} *)
    | {pexp_desc = Pexp_let (recursive, vbs, internal_expression)} ->
      (* here's where we spelunk! *)
      let wrap, has_forward_ref, exp =
        spelunk_for_fun_expression internal_expression
      in
      ( wrap,
        has_forward_ref,
        {expression with pexp_desc = Pexp_let (recursive, vbs, exp)} )
    (* let make = React.forwardRef((~prop) => ...) *)
    | {
     pexp_desc =
       Pexp_apply
         {funct = wrapper_expression; args = [(Nolabel, internal_expression)]};
    } ->
      let () = has_application := true in
      let _, _, exp = spelunk_for_fun_expression internal_expression in
      let has_forward_ref = is_forward_ref wrapper_expression in
      ( (fun exp -> Exp.apply wrapper_expression [(nolabel, exp)]),
        has_forward_ref,
        exp )
    | {pexp_desc = Pexp_sequence (wrapper_expression, internal_expression)} ->
      let wrap, has_forward_ref, exp =
        spelunk_for_fun_expression internal_expression
      in
      ( wrap,
        has_forward_ref,
        {expression with pexp_desc = Pexp_sequence (wrapper_expression, exp)} )
    | e -> ((fun a -> a), false, e)
  in
  let wrap_expression, has_forward_ref, expression =
    spelunk_for_fun_expression expression
  in
  (wrap_expression_with_binding wrap_expression, has_forward_ref, expression)

let vb_match ~expr (name, default, _, alias, loc, _) =
  let label = get_label name in
  match default with
  | Some default ->
    let value_binding =
      Vb.mk
        (Pat.var (Location.mkloc alias loc))
        (Exp.match_
           (Exp.ident {txt = Lident ("__" ^ alias); loc = Location.none})
           [
             Exp.case
               (Pat.construct
                  (Location.mknoloc @@ Lident "Some")
                  (Some (Pat.var (Location.mknoloc label))))
               (Exp.ident (Location.mknoloc @@ Lident label));
             Exp.case
               (Pat.construct (Location.mknoloc @@ Lident "None") None)
               default;
           ])
    in
    Exp.let_ Nonrecursive [value_binding] expr
  | None -> expr

let vb_match_expr named_arg_list expr =
  let rec aux named_arg_list =
    match named_arg_list with
    | [] -> expr
    | named_arg :: rest -> vb_match named_arg ~expr:(aux rest)
  in
  aux (List.rev named_arg_list)

let map_binding ~config ~empty_loc ~pstr_loc ~file_name ~rec_flag binding =
  if Jsx_common.has_attr_on_binding Jsx_common.has_attr binding then (
    check_multiple_components ~config ~loc:pstr_loc;
    let core_type_of_attr =
      Jsx_common.core_type_of_attrs binding.pvb_attributes
    in
    let typ_vars_of_core_type =
      core_type_of_attr
      |> Option.map Jsx_common.typ_vars_of_core_type
      |> Option.value ~default:[]
    in
    let binding_loc = binding.pvb_loc in
    let binding_pat_loc = binding.pvb_pat.ppat_loc in
    let binding =
      {
        binding with
        pvb_pat = {binding.pvb_pat with ppat_loc = empty_loc};
        pvb_loc = empty_loc;
        pvb_attributes = binding.pvb_attributes |> List.filter other_attrs_pure;
      }
    in
    let fn_name = get_fn_name binding.pvb_pat in
    let internal_fn_name = fn_name ^ "$Internal" in
    let full_module_name =
      make_module_name file_name config.nested_modules fn_name
    in
    let binding_wrapper, has_forward_ref, expression =
      modified_binding ~binding_loc ~binding_pat_loc ~fn_name binding
    in
    let is_async = Ast_async.dig_async_payload_from_function binding.pvb_expr in
    let named_arg_list, newtypes, _typeConstraints =
      recursively_transform_named_args_for_make
        (modified_binding_old binding)
        [] [] None
    in
    let named_type_list = List.fold_left arg_to_type [] named_arg_list in
    (* type props = { ... } *)
    let props_record_type =
      make_props_record_type ~core_type_of_attr ~external_:false
        ~typ_vars_of_core_type "props" pstr_loc named_type_list
    in
    let inner_expression =
      Exp.apply
        (Exp.ident
           (Location.mknoloc
           @@ Lident
                (match rec_flag with
                | Recursive -> internal_fn_name
                | Nonrecursive -> fn_name)))
        ([(Nolabel, Exp.ident (Location.mknoloc @@ Lident "props"))]
        @
        match has_forward_ref with
        | true -> [(Nolabel, Exp.ident (Location.mknoloc @@ Lident "ref"))]
        | false -> [])
    in
    let make_props_pattern = function
      | [] -> Pat.var @@ Location.mknoloc "props"
      | _ ->
        Pat.constraint_
          (Pat.var @@ Location.mknoloc "props")
          (Typ.constr (Location.mknoloc @@ Lident "props") [Typ.any ()])
    in
    let inner_expression =
      Jsx_common.async_component ~async:is_async inner_expression
    in
    let full_expression =
      (* React component name should start with uppercase letter *)
      (* let make = { let \"App" = props => make(props); \"App" } *)
      (* let make = React.forwardRef({
           let \"App" = (props, ref) => make({...props, ref: @optional (Js.Nullabel.toOption(ref))})
         })*)
      Exp.fun_ ~arity:None Nolabel None
        (match core_type_of_attr with
        | None -> make_props_pattern named_type_list
        | Some _ -> make_props_pattern typ_vars_of_core_type)
        (if has_forward_ref then
           Exp.fun_ ~arity:None Nolabel None
             (Pat.var @@ Location.mknoloc "ref")
             inner_expression
         else inner_expression)
        ~attrs:binding.pvb_expr.pexp_attributes
    in
    let full_expression =
      full_expression
      |> Ast_uncurried.uncurried_fun ~arity:(if has_forward_ref then 2 else 1)
    in
    let full_expression =
      match full_module_name with
      | "" -> full_expression
      | txt ->
        Exp.let_ Nonrecursive
          [
            Vb.mk ~loc:empty_loc
              (Pat.var ~loc:empty_loc {loc = empty_loc; txt})
              full_expression;
          ]
          (Exp.ident ~loc:pstr_loc {loc = empty_loc; txt = Lident txt})
    in
    let rec strip_constraint_unpack ~label pattern =
      match pattern with
      | {ppat_desc = Ppat_constraint (_, {ptyp_desc = Ptyp_package _})} ->
        pattern
      | {ppat_desc = Ppat_constraint (pattern, _)} ->
        strip_constraint_unpack ~label pattern
      | _ -> pattern
    in
    let safe_pattern_label pattern =
      match pattern with
      | {ppat_desc = Ppat_var {txt; loc}} ->
        {pattern with ppat_desc = Ppat_var {txt = "__" ^ txt; loc}}
      | {ppat_desc = Ppat_alias (p, {txt; loc})} ->
        {pattern with ppat_desc = Ppat_alias (p, {txt = "__" ^ txt; loc})}
      | _ -> pattern
    in
    let rec returned_expression patterns_with_label patterns_with_nolabel
        ({pexp_desc} as expr) =
      match pexp_desc with
      | Pexp_newtype (_, expr) ->
        returned_expression patterns_with_label patterns_with_nolabel expr
      | Pexp_constraint (expr, _) ->
        returned_expression patterns_with_label patterns_with_nolabel expr
      | Pexp_fun
          {
            lhs = {ppat_desc = Ppat_construct ({txt = Lident "()"}, _)};
            rhs = expr;
          } ->
        (patterns_with_label, patterns_with_nolabel, expr)
      | Pexp_fun
          {
            arg_label;
            default;
            lhs = {ppat_loc; ppat_desc} as pattern;
            rhs = expr;
          } -> (
        let pattern_without_constraint =
          strip_constraint_unpack ~label:(get_label arg_label) pattern
        in
        (*
           If prop has the default value as Ident, it will get a build error
           when the referenced Ident value and the prop have the same name.
           So we add a "__" to label to resolve the build error.
        *)
        let pattern_with_safe_label =
          match default with
          | Some _ -> safe_pattern_label pattern_without_constraint
          | _ -> pattern_without_constraint
        in
        if is_labelled arg_label || is_optional arg_label then
          returned_expression
            (( {loc = ppat_loc; txt = Lident (get_label arg_label)},
               {
                 pattern_with_safe_label with
                 ppat_attributes = pattern.ppat_attributes;
               },
               is_optional arg_label )
            :: patterns_with_label)
            patterns_with_nolabel expr
        else
          (* Special case of nolabel arg "ref" in forwardRef fn *)
          (* let make = React.forwardRef(ref => body) *)
          match ppat_desc with
          | Ppat_var {txt} | Ppat_constraint ({ppat_desc = Ppat_var {txt}}, _)
            ->
            returned_expression patterns_with_label
              (( {loc = ppat_loc; txt = Lident txt},
                 {pattern with ppat_attributes = pattern.ppat_attributes},
                 true )
              :: patterns_with_nolabel)
              expr
          | _ ->
            returned_expression patterns_with_label patterns_with_nolabel expr)
      | _ -> (patterns_with_label, patterns_with_nolabel, expr)
    in
    let patterns_with_label, patterns_with_nolabel, expression =
      returned_expression [] [] expression
    in
    (* add pattern matching for optional prop value *)
    let expression =
      if has_default_value named_arg_list then
        vb_match_expr named_arg_list expression
      else expression
    in
    (* (ref) => expr *)
    let expression =
      List.fold_left
        (fun expr (_, pattern, _opt) ->
          let pattern =
            match pattern.ppat_desc with
            | Ppat_var {txt} when txt = "ref" ->
              Pat.constraint_ pattern (ref_type Location.none)
            | _ -> pattern
          in
          Exp.fun_ ~arity:None Nolabel None pattern expr)
        expression patterns_with_nolabel
    in
    (* ({a, b, _}: props<'a, 'b>) *)
    let record_pattern =
      match patterns_with_label with
      | [] -> Pat.any ()
      | _ -> Pat.record (List.rev patterns_with_label) Open
    in
    let expression =
      Exp.fun_ ~arity:(Some 1) ~async:is_async Nolabel None
        (Pat.constraint_ record_pattern
           (Typ.constr ~loc:empty_loc
              {txt = Lident "props"; loc = empty_loc}
              (match core_type_of_attr with
              | None ->
                make_props_type_params ~strip_explicit_option:true
                  ~strip_explicit_js_nullable_of_ref:has_forward_ref
                  named_type_list
              | Some _ -> (
                match typ_vars_of_core_type with
                | [] -> []
                | _ -> [Typ.any ()]))))
        expression
    in
    let expression =
      (* Add new tupes (type a,b,c) to make's definition *)
      newtypes
      |> List.fold_left (fun e newtype -> Exp.newtype newtype e) expression
    in
    (* let make = ({id, name, ...}: props<'id, 'name, ...>) => { ... } *)
    let binding, new_binding =
      match rec_flag with
      | Recursive ->
        ( binding_wrapper
            (Exp.let_ ~loc:empty_loc Nonrecursive
               [make_new_binding binding expression internal_fn_name]
               (Exp.let_ ~loc:empty_loc Nonrecursive
                  [
                    Vb.mk
                      (Pat.var {loc = empty_loc; txt = fn_name})
                      full_expression;
                  ]
                  (Exp.ident {loc = empty_loc; txt = Lident fn_name}))),
          None )
      | Nonrecursive ->
        ( {
            binding with
            pvb_expr = expression;
            pvb_pat = Pat.var {txt = fn_name; loc = Location.none};
          },
          Some (binding_wrapper full_expression) )
    in
    (Some props_record_type, binding, new_binding))
  else if Jsx_common.has_attr_on_binding Jsx_common.has_attr_with_props binding
  then
    let modified_binding =
      {
        binding with
        pvb_attributes = binding.pvb_attributes |> List.filter other_attrs_pure;
      }
    in
    let fn_name = get_fn_name modified_binding.pvb_pat in
    let internal_fn_name = fn_name ^ "$Internal" in
    let full_module_name =
      make_module_name file_name config.nested_modules fn_name
    in

    let is_async =
      Ast_async.dig_async_payload_from_function modified_binding.pvb_expr
    in

    let make_new_binding ~loc ~full_module_name binding =
      let props_pattern =
        match binding.pvb_expr with
        | {
         pexp_desc =
           Pexp_apply {funct = wrapper_expr; args = [(Nolabel, func_expr)]};
        }
          when is_forward_ref wrapper_expr ->
          (* Case when using React.forwardRef *)
          let rec check_invalid_forward_ref expr =
            match expr.pexp_desc with
            | Pexp_fun {arg_label = Labelled _ | Optional _} ->
              Location.raise_errorf ~loc:expr.pexp_loc
                "Components using React.forwardRef cannot use \
                 @react.componentWithProps. Please use @react.component \
                 instead."
            | Pexp_fun {arg_label = Nolabel; rhs = body} ->
              check_invalid_forward_ref body
            | _ -> ()
          in
          check_invalid_forward_ref func_expr;
          Pat.var {txt = "props"; loc}
        | {pexp_desc = Pexp_fun {lhs = {ppat_desc = Ppat_constraint (_, typ)}}}
          -> (
          match typ with
          | {ptyp_desc = Ptyp_constr ({txt = Lident "props"}, args)} ->
            (* props<_> *)
            if List.length args > 0 then
              Pat.constraint_
                (Pat.var {txt = "props"; loc})
                (Typ.constr {txt = Lident "props"; loc} [Typ.any ()])
              (* props *)
            else
              Pat.constraint_
                (Pat.var {txt = "props"; loc})
                (Typ.constr {txt = Lident "props"; loc} [])
          | _ -> Pat.var {txt = "props"; loc})
        | _ -> Pat.var {txt = "props"; loc}
      in

      let wrapper_expr =
        Exp.fun_ ~arity:None Nolabel None props_pattern
          ~attrs:binding.pvb_expr.pexp_attributes
          (Jsx_common.async_component ~async:is_async
             (Exp.apply
                (Exp.ident
                   {
                     txt =
                       Lident
                         (match rec_flag with
                         | Recursive -> internal_fn_name
                         | Nonrecursive -> fn_name);
                     loc;
                   })
                [(Nolabel, Exp.ident {txt = Lident "props"; loc})]))
      in

      let wrapper_expr = Ast_uncurried.uncurried_fun ~arity:1 wrapper_expr in

      let internal_expression =
        Exp.let_ Nonrecursive
          [Vb.mk (Pat.var {txt = full_module_name; loc}) wrapper_expr]
          (Exp.ident {txt = Lident full_module_name; loc})
      in

      Vb.mk ~attrs:modified_binding.pvb_attributes
        (Pat.var {txt = fn_name; loc})
        internal_expression
    in

    let new_binding =
      match rec_flag with
      | Recursive -> None
      | Nonrecursive ->
        Some
          (make_new_binding ~loc:empty_loc ~full_module_name modified_binding)
    in
    ( None,
      {
        binding with
        pvb_attributes = binding.pvb_attributes |> List.filter other_attrs_pure;
        pvb_expr =
          {
            binding.pvb_expr with
            (* moved to wrapper_expr *)
            pexp_attributes = [];
          };
      },
      new_binding )
  else (None, binding, None)

let transform_structure_item ~config item =
  match item with
  (* external *)
  | {
      pstr_loc;
      pstr_desc =
        Pstr_primitive ({pval_attributes; pval_type} as value_description);
    } as pstr -> (
    match
      ( List.filter Jsx_common.has_attr pval_attributes,
        List.filter Jsx_common.has_attr_with_props pval_attributes )
    with
    | [], [] -> [item]
    | _, [_] ->
      Jsx_common.raise_error ~loc:pstr_loc
        "Components cannot be defined as externals when using \
         @react.componentWithProps.\n\n\
         If you intended to define an external for a React component using a \
         props type,\n\
         use the type React.component<props> instead.\n\
         Alternatively, use @react.component for an external definition with \
         labeled arguments."
    | [_], [] ->
      check_multiple_components ~config ~loc:pstr_loc;
      check_string_int_attribute_iter.structure_item
        check_string_int_attribute_iter item;
      let core_type_of_attr = Jsx_common.core_type_of_attrs pval_attributes in
      let typ_vars_of_core_type =
        core_type_of_attr
        |> Option.map Jsx_common.typ_vars_of_core_type
        |> Option.value ~default:[]
      in
      let rec get_prop_types types
          ({ptyp_loc; ptyp_desc; ptyp_attributes} as full_type) =
        match ptyp_desc with
        | Ptyp_arrow {lbl = name; arg; ret = {ptyp_desc = Ptyp_arrow _} as typ2}
          when is_labelled name || is_optional name ->
          get_prop_types ((name, ptyp_attributes, ptyp_loc, arg) :: types) typ2
        | Ptyp_arrow {lbl = Nolabel; ret} -> get_prop_types types ret
        | Ptyp_arrow {lbl = name; arg; ret = return_value}
          when is_labelled name || is_optional name ->
          ( return_value,
            (name, ptyp_attributes, return_value.ptyp_loc, arg) :: types )
        | _ -> (full_type, types)
      in
      let inner_type, prop_types = get_prop_types [] pval_type in
      let named_type_list = List.fold_left arg_to_concrete_type [] prop_types in
      let ret_props_type =
        Typ.constr ~loc:pstr_loc
          (Location.mkloc (Lident "props") pstr_loc)
          (match core_type_of_attr with
          | None -> make_props_type_params named_type_list
          | Some _ -> (
            match typ_vars_of_core_type with
            | [] -> []
            | _ -> [Typ.any ()]))
      in
      (* type props<'x, 'y> = { x: 'x, y?: 'y, ... } *)
      let props_record_type =
        make_props_record_type ~core_type_of_attr ~external_:true
          ~typ_vars_of_core_type "props" pstr_loc named_type_list
      in
      (* can't be an arrow because it will defensively uncurry *)
      let new_external_type =
        Ptyp_constr
          ( {loc = pstr_loc; txt = module_access_name config "componentLike"},
            [ret_props_type; inner_type] )
      in
      let new_structure =
        {
          pstr with
          pstr_desc =
            Pstr_primitive
              {
                value_description with
                pval_type = {pval_type with ptyp_desc = new_external_type};
                pval_attributes = List.filter other_attrs_pure pval_attributes;
              };
        }
      in
      [props_record_type; new_structure]
    | _ ->
      Jsx_common.raise_error ~loc:pstr_loc
        "Only one JSX component call can exist on a component at one time")
  (* let component = ... *)
  | {pstr_loc; pstr_desc = Pstr_value (rec_flag, value_bindings)} -> (
    let file_name = filename_from_loc pstr_loc in
    let empty_loc = Location.in_file file_name in
    let process_binding binding (new_items, bindings, new_bindings) =
      let new_item, binding, new_binding =
        map_binding ~config ~empty_loc ~pstr_loc ~file_name ~rec_flag binding
      in
      let new_items =
        match new_item with
        | Some item -> item :: new_items
        | None -> new_items
      in
      let new_bindings =
        match new_binding with
        | Some new_binding -> new_binding :: new_bindings
        | None -> new_bindings
      in
      (new_items, binding :: bindings, new_bindings)
    in
    let new_items, bindings, new_bindings =
      List.fold_right process_binding value_bindings ([], [], [])
    in
    new_items
    @ [{pstr_loc; pstr_desc = Pstr_value (rec_flag, bindings)}]
    @
    match new_bindings with
    | [] -> []
    | new_bindings ->
      [{pstr_loc = empty_loc; pstr_desc = Pstr_value (rec_flag, new_bindings)}])
  | _ -> [item]

let transform_signature_item ~config item =
  match item with
  | {
      psig_loc;
      psig_desc = Psig_value ({pval_attributes; pval_type} as psig_desc);
    } as psig -> (
    match List.filter Jsx_common.has_attr pval_attributes with
    | [] -> [item]
    | [_] ->
      check_multiple_components ~config ~loc:psig_loc;
      check_string_int_attribute_iter.signature_item
        check_string_int_attribute_iter item;
      let core_type_of_attr = Jsx_common.core_type_of_attrs pval_attributes in
      let typ_vars_of_core_type =
        core_type_of_attr
        |> Option.map Jsx_common.typ_vars_of_core_type
        |> Option.value ~default:[]
      in
      let rec get_prop_types types ({ptyp_loc; ptyp_desc} as full_type) =
        match ptyp_desc with
        | Ptyp_arrow
            {
              lbl;
              arg = {ptyp_attributes = attrs} as type_;
              ret = {ptyp_desc = Ptyp_arrow _} as rest;
            }
          when is_optional lbl || is_labelled lbl ->
          get_prop_types ((lbl, attrs, ptyp_loc, type_) :: types) rest
        | Ptyp_arrow
            {
              lbl = Nolabel;
              arg = {ptyp_desc = Ptyp_constr ({txt = Lident "unit"}, _)};
              ret = rest;
            } ->
          get_prop_types types rest
        | Ptyp_arrow {lbl = Nolabel; ret = rest} -> get_prop_types types rest
        | Ptyp_arrow
            {
              lbl = name;
              arg = {ptyp_attributes = attrs} as type_;
              ret = return_value;
            }
          when is_optional name || is_labelled name ->
          (return_value, (name, attrs, return_value.ptyp_loc, type_) :: types)
        | _ -> (full_type, types)
      in
      let inner_type, prop_types = get_prop_types [] pval_type in
      let named_type_list = List.fold_left arg_to_concrete_type [] prop_types in
      let ret_props_type =
        Typ.constr
          (Location.mkloc (Lident "props") psig_loc)
          (match core_type_of_attr with
          | None -> make_props_type_params named_type_list
          | Some _ -> (
            match typ_vars_of_core_type with
            | [] -> []
            | _ -> [Typ.any ()]))
      in
      let external_ = psig_desc.pval_prim <> [] in
      let props_record_type =
        make_props_record_type_sig ~core_type_of_attr ~external_
          ~typ_vars_of_core_type "props" psig_loc named_type_list
      in
      (* can't be an arrow because it will defensively uncurry *)
      let new_external_type =
        Ptyp_constr
          ( {loc = psig_loc; txt = module_access_name config "componentLike"},
            [ret_props_type; inner_type] )
      in
      let new_structure =
        {
          psig with
          psig_desc =
            Psig_value
              {
                psig_desc with
                pval_type = {pval_type with ptyp_desc = new_external_type};
                pval_attributes = List.filter other_attrs_pure pval_attributes;
              };
        }
      in
      [props_record_type; new_structure]
    | _ ->
      Jsx_common.raise_error ~loc:psig_loc
        "Only one JSX component call can exist on a component at one time")
  | _ -> [item]

let starts_with_lowercase s =
  if String.length s = 0 then false
  else
    let c = s.[0] in
    Char.lowercase_ascii c = c

let starts_with_uppercase s =
  if String.length s = 0 then false
  else
    let c = s.[0] in
    Char.uppercase_ascii c = c

(* There appear to be slightly different rules of transformation whether the component is upper-, lowercase or a fragment *)
type componentDescription =
  | LowercasedComponent
  | UppercasedComponent
  | FragmentComponent

let loc_from_prop = function
  | JSXPropPunning (_, name) -> name.loc
  | JSXPropValue (name, _, value) ->
    {name.loc with loc_end = value.pexp_loc.loc_end}
  | JSXPropSpreading (loc, _) -> loc

let mk_record_from_props mapper (jsx_expr_loc : Location.t) (props : jsx_props)
    : expression =
  (* Create an artificial range from the first till the last prop *)
  let loc =
    match props with
    | [] -> jsx_expr_loc
    | head :: tail ->
      let rec visit props =
        match props with
        | [] -> head
        | [last] -> last
        | _ :: rest -> visit rest
      in
      let first_item = head |> loc_from_prop in
      let last_item = visit tail |> loc_from_prop in
      {
        loc_start = first_item.loc_start;
        loc_end = last_item.loc_end;
        loc_ghost = true;
      }
  in
  (* key should be filtered out *)
  let props =
    props
    |> List.filter (function
         | JSXPropPunning (_, {txt = "key"}) | JSXPropValue ({txt = "key"}, _, _)
           ->
           false
         | _ -> true)
  in
  let props, spread_props =
    match props with
    | JSXPropSpreading (_, expr) :: rest ->
      (rest, Some (mapper.expr mapper expr))
    | _ -> (props, None)
  in

  let record_fields =
    props
    |> List.map (function
         | JSXPropPunning (is_optional, name) ->
           ( {txt = Lident name.txt; loc = name.loc},
             Exp.ident {txt = Lident name.txt; loc = name.loc},
             is_optional )
         | JSXPropValue (name, is_optional, value) ->
           ( {txt = Lident name.txt; loc = name.loc},
             mapper.expr mapper value,
             is_optional )
         | JSXPropSpreading (loc, _) ->
           (* There can only be one spread expression and it is expected to be the first prop *)
           Jsx_common.raise_error ~loc
             "JSX: use {...p} {x: v} not {x: v} {...p} \n\
             \     multiple spreads {...p} {...p} not allowed.")
  in
  match (record_fields, spread_props) with
  | [], Some spread_props ->
    {pexp_desc = spread_props.pexp_desc; pexp_loc = loc; pexp_attributes = []}
  | record_fields, spread_props ->
    {
      pexp_desc = Pexp_record (record_fields, spread_props);
      pexp_loc = loc;
      pexp_attributes = [];
    }

let try_find_key_prop (props : jsx_props) : (arg_label * expression) option =
  props
  |> List.find_map (function
       | JSXPropPunning (is_optional, ({txt = "key"} as name)) ->
         let arg_label = if is_optional then Optional name else Labelled name in
         Some (arg_label, Exp.ident {txt = Lident "key"; loc = name.loc})
       | JSXPropValue (({txt = "key"} as name), is_optional, expr) ->
         let arg_label = if is_optional then Optional name else Labelled name in
         Some (arg_label, expr)
       | _ -> None)

let append_children_prop (config : Jsx_common.jsx_config) mapper
    (component_description : componentDescription) (props : jsx_props)
    (children : jsx_children) : jsx_props =
  match children with
  | JSXChildrenItems [] -> props
  | JSXChildrenItems [child] | JSXChildrenSpreading child ->
    let expr =
      (* I don't quite know why fragment and uppercase don't do this additional ReactDOM.someElement wrapping *)
      match component_description with
      | FragmentComponent | UppercasedComponent -> mapper.expr mapper child
      | LowercasedComponent ->
        let element_binding =
          match config.module_ |> String.lowercase_ascii with
          | "react" -> Lident "ReactDOM"
          | _generic -> module_access_name config "Elements"
        in
        Exp.apply
          (Exp.ident
             {txt = Ldot (element_binding, "someElement"); loc = Location.none})
          [(Nolabel, child)]
    in
    let is_optional =
      match component_description with
      | LowercasedComponent -> true
      | FragmentComponent | UppercasedComponent -> false
    in
    props
    @ [
        JSXPropValue ({txt = "children"; loc = Location.none}, is_optional, expr);
      ]
  | JSXChildrenItems xs ->
    (* this is a hack to support react components that introspect into their children *)
    props
    @ [
        JSXPropValue
          ( {txt = "children"; loc = Location.none},
            false,
            Exp.apply
              (Exp.ident
                 {txt = module_access_name config "array"; loc = Location.none})
              [(Nolabel, Exp.array (List.map (mapper.expr mapper) xs))] );
      ]

let mk_react_jsx (config : Jsx_common.jsx_config) mapper loc attrs
    (component_description : componentDescription) (elementTag : expression)
    (props : jsx_props) (children : jsx_children) : expression =
  let more_than_one_children =
    match children with
    | JSXChildrenSpreading _ -> false
    | JSXChildrenItems xs -> List.length xs > 1
  in
  let props_with_children =
    append_children_prop config mapper component_description props children
  in
  let props_record = mk_record_from_props mapper loc props_with_children in
  let jsx_expr, key_and_unit =
    let mk_element_bind (jsx_part : string) : Longident.t =
      match component_description with
      | FragmentComponent | UppercasedComponent ->
        module_access_name config jsx_part
      | LowercasedComponent ->
        let element_binding =
          match config.module_ |> String.lowercase_ascii with
          | "react" -> Lident "ReactDOM"
          | _generic -> module_access_name config "Elements"
        in
        Ldot (element_binding, jsx_part)
    in
    match try_find_key_prop props with
    | None ->
      ( Exp.ident
          {
            loc = Location.none;
            txt =
              mk_element_bind (if more_than_one_children then "jsxs" else "jsx");
          },
        [] )
    | Some key_prop ->
      ( Exp.ident
          {
            loc = Location.none;
            txt =
              mk_element_bind
                (if more_than_one_children then "jsxsKeyed" else "jsxKeyed");
          },
        [key_prop; (nolabel, unit_expr ~loc:Location.none)] )
  in
  let args = [(nolabel, elementTag); (nolabel, props_record)] @ key_and_unit in
  Exp.apply ~loc ~attrs jsx_expr args

(* In most situations, the component name is the make function from a module. 
    However, if the name contains a lowercase letter, it means it probably an external component.
    In this case, we use the name as is.
    See tests/syntax_tests/data/ppx/react/externalWithCustomName.res
*)
let mk_uppercase_tag_name_expr tag_name =
  let tag_identifier : Longident.t =
    if Longident.flatten tag_name.txt |> List.for_all starts_with_uppercase then
      (* All parts are uppercase, so we append .make *)
      Ldot (tag_name.txt, "make")
    else tag_name.txt
  in
  Exp.ident ~loc:tag_name.loc {txt = tag_identifier; loc = tag_name.loc}

let expr ~(config : Jsx_common.jsx_config) mapper expression =
  match expression with
  | {
   pexp_desc = Pexp_jsx_element jsx_element;
   pexp_loc = loc;
   pexp_attributes = attrs;
  } -> (
    let loc = {loc with loc_ghost = true} in
    match jsx_element with
    | Jsx_fragment {jsx_fragment_children = children} ->
      let fragment =
        Exp.ident ~loc {loc; txt = module_access_name config "jsxFragment"}
      in
      mk_react_jsx config mapper loc attrs FragmentComponent fragment []
        children
    | Jsx_unary_element
        {jsx_unary_element_tag_name = tag_name; jsx_unary_element_props = props}
      ->
      let name = Longident.flatten tag_name.txt |> String.concat "." in
      if starts_with_lowercase name then
        (* For example 'input' *)
        let component_name_expr = constant_string ~loc:tag_name.loc name in
        mk_react_jsx config mapper loc attrs LowercasedComponent
          component_name_expr props (JSXChildrenItems [])
      else if starts_with_uppercase name then
        (* MyModule.make *)
        let make_id = mk_uppercase_tag_name_expr tag_name in
        mk_react_jsx config mapper loc attrs UppercasedComponent make_id props
          (JSXChildrenItems [])
      else
        Jsx_common.raise_error ~loc
          "JSX: element name is neither upper- or lowercase, got \"%s\""
          (Longident.flatten tag_name.txt |> String.concat ".")
    | Jsx_container_element
        {
          jsx_container_element_tag_name_start = tag_name;
          jsx_container_element_props = props;
          jsx_container_element_children = children;
        } ->
      let name = Longident.flatten tag_name.txt |> String.concat "." in
      (* For example: <div> <h1></h1> <br /> </div>
         This has an impact if we want to use ReactDOM.jsx or ReactDOM.jsxs
           *)
      if starts_with_lowercase name then
        let component_name_expr = constant_string ~loc:tag_name.loc name in
        mk_react_jsx config mapper loc attrs LowercasedComponent
          component_name_expr props children
      else if starts_with_uppercase name then
        (* MyModule.make *)
        let make_id = mk_uppercase_tag_name_expr tag_name in
        mk_react_jsx config mapper loc attrs UppercasedComponent make_id props
          children
      else
        Jsx_common.raise_error ~loc
          "JSX: element name is neither upper- or lowercase, got \"%s\""
          (Longident.flatten tag_name.txt |> String.concat "."))
  | e -> default_mapper.expr mapper e

let module_binding ~(config : Jsx_common.jsx_config) mapper module_binding =
  config.nested_modules <- module_binding.pmb_name.txt :: config.nested_modules;
  let mapped = default_mapper.module_binding mapper module_binding in
  let () =
    match config.nested_modules with
    | _ :: rest -> config.nested_modules <- rest
    | [] -> ()
  in
  mapped

(* TODO: some line number might still be wrong *)
let jsx_mapper ~config =
  let expr = expr ~config in
  let module_binding = module_binding ~config in
  let transform_structure_item = transform_structure_item ~config in
  let transform_signature_item = transform_signature_item ~config in
  (expr, module_binding, transform_signature_item, transform_structure_item)
