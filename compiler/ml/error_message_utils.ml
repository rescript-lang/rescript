type extract_concrete_typedecl =
  Env.t -> Types.type_expr -> Path.t * Path.t * Types.type_declaration

let configured_jsx_module : string option ref = ref None

let with_configured_jsx_module s =
  match !configured_jsx_module with
  | None -> s
  | Some module_name -> module_name ^ "." ^ s

module Parser : sig
  type comment

  val extract_text_at_loc : Location.t -> string

  val parse_source : (string -> Parsetree.structure * comment list) ref

  val reprint_source : (Parsetree.structure -> comment list -> string) ref

  val parse_expr_at_loc :
    Warnings.loc -> (Parsetree.expression * comment list) option

  val reprint_expr_at_loc :
    ?mapper:(Parsetree.expression -> Parsetree.expression option) ->
    Warnings.loc ->
    string option
end = struct
  type comment

  let parse_source : (string -> Parsetree.structure * comment list) ref =
    ref (fun _ -> ([], []))

  let reprint_source : (Parsetree.structure -> comment list -> string) ref =
    ref (fun _ _ -> "")

  let extract_location_string ~src (loc : Location.t) =
    let start_pos = loc.loc_start in
    let end_pos = loc.loc_end in
    let start_offset = start_pos.pos_cnum in
    let end_offset = end_pos.pos_cnum in
    String.sub src start_offset (end_offset - start_offset)

  let extract_text_at_loc loc =
    if loc.Location.loc_start.pos_fname = "_none_" then ""
    else
      try
        (* TODO: Maybe cache later on *)
        let src = Ext_io.load_file loc.Location.loc_start.pos_fname in
        extract_location_string ~src loc
      with _ -> ""

  let parse_expr_at_loc loc =
    let sub_src = extract_text_at_loc loc in
    let parsed, comments = !parse_source sub_src in
    match parsed with
    | [{Parsetree.pstr_desc = Pstr_eval (exp, _)}] -> Some (exp, comments)
    | _ -> None

  let wrap_in_structure exp =
    [{Parsetree.pstr_desc = Pstr_eval (exp, []); pstr_loc = Location.none}]

  let reprint_expr_at_loc ?(mapper = fun _ -> None) loc =
    match parse_expr_at_loc loc with
    | Some (exp, comments) -> (
      match mapper exp with
      | Some exp -> Some (!reprint_source (wrap_in_structure exp) comments)
      | None -> None)
    | None -> None
end

let type_expr ppf typ =
  (* print a type and avoid infinite loops *)
  Printtyp.reset_and_mark_loops typ;
  Printtyp.type_expr ppf typ

type jsx_prop_error_info = {
  fields: Types.label_declaration list;
  props_record_path: Path.t;
  jsx_type: [`Fragment | `CustomComponent | `LowercaseComponent];
}

type type_clash_statement = FunctionCall
type type_clash_context =
  | SetRecordField of string (* field name *)
  | RecordField of {
      jsx: jsx_prop_error_info option;
      record_type: Types.type_expr;
      field_name: string;
      optional: bool;
    }
  | ArrayValue
  | MaybeUnwrapOption
  | IfCondition
  | AssertCondition
  | IfReturn
  | SwitchReturn
  | TryReturn
  | StringConcat
  | ComparisonOperator
  | WhileCondition
  | MathOperator of {
      for_float: bool;
      operator: string;
      is_constant: string option;
    }
  | FunctionArgument of {optional: bool; name: string option}
  | Statement of type_clash_statement
  | ForLoopCondition
  | Await

let context_to_string = function
  | Some WhileCondition -> "WhileCondition"
  | Some ForLoopCondition -> "ForLoopCondition"
  | Some AssertCondition -> "AssertCondition"
  | Some IfCondition -> "IfCondition"
  | Some (Statement _) -> "Statement"
  | Some (MathOperator _) -> "MathOperator"
  | Some ArrayValue -> "ArrayValue"
  | Some (SetRecordField _) -> "SetRecordField"
  | Some (RecordField _) -> "RecordField"
  | Some MaybeUnwrapOption -> "MaybeUnwrapOption"
  | Some SwitchReturn -> "SwitchReturn"
  | Some TryReturn -> "TryReturn"
  | Some StringConcat -> "StringConcat"
  | Some (FunctionArgument _) -> "FunctionArgument"
  | Some ComparisonOperator -> "ComparisonOperator"
  | Some IfReturn -> "IfReturn"
  | Some Await -> "Await"
  | None -> "None"

let fprintf = Format.fprintf

let error_type_text ppf type_clash_context =
  let text =
    match type_clash_context with
    | Some (Statement FunctionCall) -> "This function call returns:"
    | Some (MathOperator {is_constant = Some _}) -> "This value has type:"
    | Some ArrayValue -> "This array item has type:"
    | Some (SetRecordField _) ->
      "You're assigning something to this field that has type:"
    | _ -> "This has type:"
  in
  fprintf ppf "%s" text

let error_expected_type_text ppf type_clash_context =
  match type_clash_context with
  | Some (FunctionArgument {optional; name}) ->
    fprintf ppf "But this%s function argument"
      (match optional with
      | false -> ""
      | true -> " optional");

    (match name with
    | Some name -> fprintf ppf " @{<info>~%s@}" name
    | None -> ());

    fprintf ppf " is expecting:"
  | Some ComparisonOperator ->
    fprintf ppf "But it's being compared to something of type:"
  | Some SwitchReturn -> fprintf ppf "But this switch is expected to return:"
  | Some TryReturn -> fprintf ppf "But this try/catch is expected to return:"
  | Some WhileCondition ->
    fprintf ppf "But a @{<info>while@} loop condition must always be of type:"
  | Some ForLoopCondition ->
    fprintf ppf "But a @{<info>for@} loop bounds must always be of type:"
  | Some IfCondition ->
    fprintf ppf "But @{<info>if@} conditions must always be of type:"
  | Some AssertCondition -> fprintf ppf "But assertions must always be of type:"
  | Some IfReturn ->
    fprintf ppf "But this @{<info>if@} statement is expected to return:"
  | Some ArrayValue ->
    fprintf ppf "But this array is expected to have items of type:"
  | Some (SetRecordField _) -> fprintf ppf "But the record field is of type:"
  | Some
      (RecordField {field_name = "children"; jsx = Some {jsx_type = `Fragment}})
    ->
    fprintf ppf "But children of JSX fragments must be of type:"
  | Some
      (RecordField
         {field_name = "children"; jsx = Some {jsx_type = `CustomComponent}}) ->
    fprintf ppf "But children passed to this component must be of type:"
  | Some (RecordField {field_name; jsx = Some _}) ->
    fprintf ppf "But the component prop @{<info>%s@} is expected to have type:"
      field_name
  | Some (RecordField {field_name}) ->
    fprintf ppf "But the record field @{<info>%s@} is expected to have type:"
      field_name
  | Some (Statement FunctionCall) -> fprintf ppf "But it's expected to return:"
  | Some (MathOperator {operator}) ->
    fprintf ppf
      "But it's being used with the @{<info>%s@} operator, which works on:"
      operator
  | Some StringConcat -> fprintf ppf "But string concatenation is expecting:"
  | Some Await ->
    fprintf ppf
      "But you're using @{<info>await@} on this expression, so it is expected \
       to be of type:"
  | Some MaybeUnwrapOption | None ->
    fprintf ppf "But it's expected to have type:"

let is_record_type ~(extract_concrete_typedecl : extract_concrete_typedecl) ~env
    ty =
  try
    match extract_concrete_typedecl env ty with
    | _, _, {Types.type_kind = Type_record _; _} -> true
    | _ -> false
  with _ -> false

let is_variant_type ~(extract_concrete_typedecl : extract_concrete_typedecl)
    ~env ty =
  try
    match extract_concrete_typedecl env ty with
    | _, _, {Types.type_kind = Type_variant _; _} -> true
    | _ -> false
  with _ -> false

let get_variant_constructors
    ~(extract_concrete_typedecl : extract_concrete_typedecl) ~env ty =
  match extract_concrete_typedecl env ty with
  | _, _, {Types.type_kind = Type_variant constructors; _} -> constructors
  | _ -> []

let extract_string_constant text =
  match !Parser.parse_source text with
  | ( [
        {
          Parsetree.pstr_desc =
            Pstr_eval ({pexp_desc = Pexp_constant (Pconst_string (s, _))}, _);
        };
      ],
      _ ) ->
    Some s
  | _ -> None

let print_extra_type_clash_help ~extract_concrete_typedecl ~env loc ppf
    (bottom_aliases : (Types.type_expr * Types.type_expr) option) trace
    type_clash_context =
  match (type_clash_context, bottom_aliases) with
  | Some (MathOperator {for_float; operator; is_constant}), _ -> (
    let operator_for_other_type =
      match operator with
      | "+" -> "+."
      | "+." -> "+"
      | "/" -> "/."
      | "/." -> "/"
      | "-" -> "-."
      | "*" -> "*."
      | "*." -> "*"
      | v -> v
    in
    let operator_text =
      match operator.[0] with
      | '+' -> "add"
      | '-' -> "subtract"
      | '/' -> "divide"
      | '*' -> "multiply"
      | _ -> "compute"
    in
    (* TODO check int vs float explicitly before showing this *)
    (match (operator, bottom_aliases) with
    | "+", Some ({Types.desc = Tconstr (p1, _, _)}, {desc = Tconstr (p2, _, _)})
      when Path.same Predef.path_string p1 || Path.same Predef.path_string p2 ->
      fprintf ppf
        "\n\n\
        \  Are you looking to concatenate strings? Use the operator \
         @{<info>++@}, which concatenates strings.\n\n\
        \  Possible solutions:\n\
        \  - Change the @{<info>+@} operator to @{<info>++@} to concatenate \
         strings instead."
    | _ ->
      fprintf ppf
        "\n\n\
        \  Floats and ints have their own mathematical operators. This means \
         you cannot %s a float and an int without converting between the two.\n\n\
        \  Possible solutions:\n\
        \  - Ensure all values in this calculation have the type @{<info>%s@}. \
         You can convert between floats and ints via @{<info>Float.toInt@} and \
         @{<info>Int.fromFloat@}."
        operator_text
        (if for_float then "float" else "int"));
    match (is_constant, bottom_aliases) with
    | Some constant, _ ->
      if for_float then
        fprintf ppf
          "\n\
          \  - Make @{<info>%s@} a @{<info>float@} by adding a trailing dot: \
           @{<info>%s.@}"
          constant constant
      else
        fprintf ppf
          "\n\
          \  - Make @{<info>%s@} an @{<info>int@} by removing the dot or \
           explicitly converting to int"
          constant
    | _, Some ({Types.desc = Tconstr (p1, _, _)}, {desc = Tconstr (p2, _, _)})
      -> (
      match (Path.name p1, Path.name p2) with
      | "float", "int" | "int", "float" ->
        fprintf ppf
          "\n\
          \  - Change the operator to @{<info>%s@}, which works on @{<info>%s@}"
          operator_for_other_type
          (if for_float then "int" else "float")
      | _ -> ())
    | _ -> ())
  | Some SwitchReturn, _ ->
    fprintf ppf
      "\n\n\
      \  All branches in a @{<info>switch@} must return the same type.@,\
       To fix this, change your branch to return the expected type."
  | Some TryReturn, _ ->
    fprintf ppf
      "\n\n\
      \  The @{<info>try@} body and the @{<info>catch@} block must return the \
       same type.@,\
       To fix this, change your try/catch blocks to return the expected type."
  | Some IfCondition, _ ->
    fprintf ppf
      "\n\n\
      \  To fix this, change the highlighted code so it evaluates to a \
       @{<info>bool@}."
  | Some Await, _ ->
    fprintf ppf
      "\n\n\
      \  You're trying to await something that is not a promise.\n\n\
       Possible solutions:\n\
      \  - Remove the @{<info>await@} if this is not expected to be a promise\n\
      \  - Wrap the expression in @{<info>Promise.resolve@} to convert the \
       expression to a promise"
  | Some IfReturn, _ ->
    fprintf ppf
      "\n\n\
      \  @{<info>if@} expressions must return the same type in all branches \
       (@{<info>if@}, @{<info>else if@}, @{<info>else@})."
  | Some MaybeUnwrapOption, _ ->
    fprintf ppf
      "\n\n\
      \  Possible solutions:\n\
      \  - Unwrap the option to its underlying value using \
       `yourValue->Option.getOr(someDefaultValue)`"
  | Some ComparisonOperator, _ ->
    fprintf ppf "\n\n  You can only compare things of the same type."
  | Some ArrayValue, _ ->
    fprintf ppf
      "\n\n\
      \  Arrays can only contain items of the same type.\n\n\
      \  Possible solutions:\n\
      \  - Convert all values in the array to the same type.\n\
      \  - Use a tuple, if your array is of fixed length. Tuples can mix types \
       freely, and compiles to a JavaScript array. Example of a tuple: `let \
       myTuple = (10, \"hello\", 15.5, true)"
  | _, Some (_, {desc = Tconstr (p2, _, _)}) when Path.same Predef.path_dict p2
    ->
    fprintf ppf
      "@,@,Dicts are written like: @{<info>dict{\"a\": 1, \"b\": 2}@}@,"
  | ( _,
      Some
        (({Types.desc = Tconstr (_p1, _, _)} as ty), {desc = Tconstr (p2, _, _)})
    )
    when Path.same Predef.path_unit p2 ->
    fprintf ppf "\n\n";
    let is_jsx_element =
      match Ctype.expand_head env ty with
      | {desc = Tconstr (Pdot (Pident {name = "Jsx"}, "element", _), _, _)} ->
        true
      | _ -> false
    in
    if is_jsx_element then
      fprintf ppf
        "  - Did you forget to wrap this + adjacent JSX in a JSX fragment \
         (@{<info><></>@})?\n\
        \  - Did you mean to assign this to a variable?\n\n"
    else
      fprintf ppf
        "  - Did you mean to assign this to a variable?\n\
        \  - If you don't care about the result of this expression, you can \
         assign it to @{<info>_@} via @{<info>let _ = ...@} or pipe it to \
         @{<info>ignore@} via @{<info>expression->ignore@}\n\n"
  | _, Some ({desc = Tobject _}, ({Types.desc = Tconstr _} as t1))
    when is_record_type ~extract_concrete_typedecl ~env t1 ->
    fprintf ppf
      "@,\
       @,\
       You're passing a @{<error>ReScript object@} where a @{<info>record@} is \
       expected. Objects are written with quoted keys, and records with \
       unquoted keys.";

    let suggested_rewrite =
      Parser.reprint_expr_at_loc loc ~mapper:(fun exp ->
          match exp.Parsetree.pexp_desc with
          | Pexp_extension
              ( {txt = "obj"},
                PStr
                  [
                    {
                      pstr_desc =
                        Pstr_eval (({pexp_desc = Pexp_record _} as record), _);
                    };
                  ] ) ->
            Some record
          | _ -> None)
    in
    fprintf ppf
      "@,\
       @,\
       Possible solutions: @,\
       - Rewrite the object to a record%s@{<info>%s@}@,"
      (match suggested_rewrite with
      | Some _ -> ", like: "
      | None -> "")
      (match suggested_rewrite with
      | Some rewrite -> rewrite
      | None -> "")
  | _, Some ({Types.desc = Tconstr (p1, _, _)}, _)
    when Path.same p1 Predef.path_promise ->
    fprintf ppf "\n\n  - Did you mean to await this promise before using it?\n"
  | _, Some ({Types.desc = Tconstr (p1, _, _)}, {Types.desc = Ttuple _})
    when Path.same p1 Predef.path_array ->
    let suggested_rewrite =
      Parser.reprint_expr_at_loc loc ~mapper:(fun exp ->
          match exp.Parsetree.pexp_desc with
          | Pexp_array items ->
            Some {exp with Parsetree.pexp_desc = Pexp_tuple items}
          | _ -> None)
    in
    fprintf ppf
      "\n\n  - Fix this by passing a tuple instead of an array%s@{<info>%s@}\n"
      (match suggested_rewrite with
      | Some _ -> ", like: "
      | None -> "")
      (match suggested_rewrite with
      | Some rewrite -> rewrite
      | None -> "")
  | ( _,
      Some
        ( {desc = Tconstr (p, type_params, _)},
          {desc = Tconstr (Pdot (Pident {name = "Jsx"}, "element", _), _, _)} )
    ) -> (
    (* Looking for a JSX element but got something else *)
    let is_jsx_element ty =
      match Ctype.expand_head env ty with
      | {desc = Tconstr (Pdot (Pident {name = "Jsx"}, "element", _), _, _)} ->
        true
      | _ -> false
    in

    let print_jsx_msg ?(extra = "") name target_fn =
      fprintf ppf
        "@,\
         @,\
         In JSX, all content must be JSX elements. You can convert %s to a JSX \
         element with @{<info>%s@}%s.@,"
        name target_fn extra
    in

    match type_params with
    | _ when Path.same p Predef.path_int ->
      print_jsx_msg "int" (with_configured_jsx_module "int")
    | _ when Path.same p Predef.path_string ->
      print_jsx_msg "string" (with_configured_jsx_module "string")
    | _ when Path.same p Predef.path_float ->
      print_jsx_msg "float" (with_configured_jsx_module "float")
    | [_] when Path.same p Predef.path_option ->
      fprintf ppf
        "@,\
         @,\
         You need to unwrap this option to its underlying value first, then \
         turn that value into a JSX element.@,\
         For @{<info>None@}, you can use @{<info>%s@} to output nothing into \
         JSX.@,"
        (with_configured_jsx_module "null")
    | [tp] when Path.same p Predef.path_array && is_jsx_element tp ->
      print_jsx_msg
        ~extra:
          (" (for example by using a pipe: ->"
          ^ with_configured_jsx_module "array"
          ^ ".")
        "array"
        (with_configured_jsx_module "array")
    | [_] when Path.same p Predef.path_array ->
      fprintf ppf
        "@,\
         @,\
         You need to convert each item in this array to a JSX element first, \
         then use @{<info>%s@} to convert the array of JSX elements into a \
         single JSX element.@,"
        (with_configured_jsx_module "array")
    | _ -> ())
  | ( Some (RecordField {optional = true; field_name; jsx = None}),
      Some ({desc = Tconstr (p, _, _)}, _) )
    when Path.same Predef.path_option p ->
    fprintf ppf
      "@,\
       @,\
       @{<info>%s@} is an optional record field, and you're passing an \
       optional value to it.@,\
       Values passed to an optional record field don't need to be wrapped in \
       an option. You might need to adjust the type of the value supplied.\n\
      \       @,\
       Possible solutions: @,\
       - Unwrap the option from the value you're passing in@,\
       - If you really do want to pass the optional value, prepend the value \
       with @{<info>?@} to show you want to pass the option, like: \
       @{<info>{%s: ?%s@}}"
      field_name field_name
      (Parser.extract_text_at_loc loc)
  | ( Some (RecordField {optional = true; field_name; jsx = Some _}),
      Some ({desc = Tconstr (p, _, _)}, _) )
    when Path.same Predef.path_option p ->
    fprintf ppf
      "@,\
       @,\
       @{<info>%s@} is an optional component prop, and you're passing an \
       optional value to it.@,\
       Values passed to an optional component prop don't need to be wrapped in \
       an option. You might need to adjust the type of the value supplied.\n\
      \       @,\
       Possible solutions: @,\
       - Unwrap the option from the value you're passing in@,\
       - If you really do want to pass the optional value, prepend the value \
       with @{<info>?@} to show you want to pass the option, like: \
       @{<info>%s=?%s@}"
      field_name field_name
      (Parser.extract_text_at_loc loc)
  | ( Some (FunctionArgument {optional = true}),
      Some ({desc = Tconstr (p, _, _)}, _) )
    when Path.same Predef.path_option p ->
    fprintf ppf
      "@,\
       @,\
       You're passing an optional value into an optional function argument.@,\
       Values passed to an optional function argument don't need to be wrapped \
       in an option. You might need to adjust the type of the value supplied.\n\
      \       @,\
       Possible solutions: @,\
       - Unwrap the option from the value you're passing in@,\
       - If you really do want to pass the optional value, prepend the value \
       with @{<info>?@} to show you want to pass the option, like: \
       @{<info>?%s@}"
      (Parser.extract_text_at_loc loc)
  | _, Some ({Types.desc = Tconstr (p1, _, _)}, {desc = Tvariant row_desc})
    when Path.same Predef.path_string p1 -> (
    (* Check if we have a string constant that could be a polymorphic variant constructor *)
    let target_expr_text = Parser.extract_text_at_loc loc in
    match extract_string_constant target_expr_text with
    | Some string_value -> (
      let variant_constructors = List.map fst row_desc.row_fields in
      let reprinted =
        Parser.reprint_expr_at_loc loc ~mapper:(fun exp ->
            match exp.Parsetree.pexp_desc with
            | Pexp_constant (Pconst_string (s, _)) ->
              Some {exp with Parsetree.pexp_desc = Pexp_variant (s, None)}
            | _ -> None)
      in
      match (reprinted, List.mem string_value variant_constructors) with
      | Some reprinted, true ->
        fprintf ppf
          "\n\n\
          \  Possible solutions:\n\
          \  - The constant passed matches one of the expected polymorphic \
           variant constructors. Did you mean to pass this as a polymorphic \
           variant? If so, rewrite @{<info>\"%s\"@} to @{<info>%s@}"
          string_value reprinted
      | _ -> ())
    | None -> ())
  | _, Some ({Types.desc = Tconstr (p1, _, _)}, ({desc = Tconstr _} as t2))
    when Path.same Predef.path_string p1
         && is_variant_type ~extract_concrete_typedecl ~env t2 -> (
    (* Check if we have a string constant that could be a regular variant constructor *)
    let target_expr_text = Parser.extract_text_at_loc loc in
    match extract_string_constant target_expr_text with
    | Some string_value -> (
      let constructors =
        get_variant_constructors ~extract_concrete_typedecl ~env t2
      in
      (* Extract runtime representations from constructor declarations *)
      let constructor_mappings =
        List.filter_map
          (fun (cd : Types.constructor_declaration) ->
            let constructor_name = Ident.name cd.cd_id in
            let runtime_repr =
              match Ast_untagged_variants.process_tag_type cd.cd_attributes with
              | Some (String s) -> Some s (* @as("string_value") *)
              | Some _ -> None (* @as with non-string values *)
              | None -> Some constructor_name (* No @as, use constructor name *)
            in
            match runtime_repr with
            | Some repr -> Some (repr, constructor_name)
            | None -> None)
          constructors
      in
      let matching_constructor =
        List.find_opt
          (fun (runtime_repr, _) -> runtime_repr = string_value)
          constructor_mappings
      in
      match matching_constructor with
      | Some (_, constructor_name) -> (
        let reprinted =
          Parser.reprint_expr_at_loc loc ~mapper:(fun exp ->
              match exp.Parsetree.pexp_desc with
              | Pexp_constant (Pconst_string (_, _)) ->
                Some
                  {
                    exp with
                    Parsetree.pexp_desc =
                      Pexp_construct
                        ( {txt = Lident constructor_name; loc = exp.pexp_loc},
                          None );
                  }
              | _ -> None)
        in
        match reprinted with
        | Some reprinted ->
          fprintf ppf
            "\n\n\
            \  Possible solutions:\n\
            \  - The constant passed matches the runtime representation of one \
             of the expected variant constructors. Did you mean to pass this \
             as a variant constructor? If so, rewrite @{<info>\"%s\"@} to \
             @{<info>%s@}"
            string_value reprinted
        | None -> ())
      | None -> ())
    | _ -> ())
  | _, Some (_supplied_type, target_type) ->
    (* Coercion should always target the top level types. *)
    let top_level_types =
      match trace with
      | (_, t1_top) :: (_, t2_top) :: _ -> Some (t1_top, t2_top)
      | _ -> None
    in
    let can_show_coercion_message =
      match top_level_types with
      | Some ({Types.desc = Tvariant _}, {Types.desc = Tvariant _}) ->
        (* Subtyping polymorphic variants give some weird messages sometimes,
        so let's turn it off for now. For an example, turn them on again and try:
        ```
        let a: [#Resize | #KeyDown] = #Resize
        let b: [#Click] = a
        ```
        *)
        false
      | Some (t1, t2) -> (
        try
          Ctype.subtype env t1 t2 ();
          true
        with _ -> false)
      | None -> false
    in
    let target_type_string = Format.asprintf "%a" type_expr target_type in
    let target_expr_text = Parser.extract_text_at_loc loc in
    let suggested_rewrite =
      match
        !Parser.parse_source
          (Printf.sprintf "(%s :> %s)" target_expr_text target_type_string)
      with
      | [], _ -> None
      | structure, comments -> Some (!Parser.reprint_source structure comments)
    in

    (* Suggesting coercion only makes sense for non-constant values. *)
    let is_constant =
      match !Parser.parse_source target_expr_text with
      | ( [{Parsetree.pstr_desc = Pstr_eval ({pexp_desc = Pexp_constant _}, _)}],
          _ ) ->
        true
      | _ -> false
    in

    if can_show_coercion_message && not is_constant then (
      fprintf ppf
        "@,\
         @,\
         Possible solutions: @,\
         - These types are compatible at runtime. You can use the coercion \
         operator to convert to the expected type";
      match suggested_rewrite with
      | Some rewrite -> fprintf ppf ": @{<info>%s@}@," rewrite
      | None -> fprintf ppf ": @{<info>:>@}@,")
  | _ -> ()

let type_clash_context_from_function sexp sfunct =
  let is_constant =
    match sexp.Parsetree.pexp_desc with
    | Pexp_constant (Pconst_integer (txt, _) | Pconst_float (txt, _)) ->
      Some txt
    | _ -> None
  in
  match sfunct.Parsetree.pexp_desc with
  | Pexp_ident
      {txt = Lident ("==" | "===" | "!=" | "!==" | ">" | ">=" | "<" | "<=")} ->
    Some ComparisonOperator
  | Pexp_ident {txt = Lident "++"} -> Some StringConcat
  | Pexp_ident {txt = Lident (("/." | "*." | "+." | "-.") as operator)} ->
    Some (MathOperator {for_float = true; operator; is_constant})
  | Pexp_ident {txt = Lident (("/" | "*" | "+" | "-") as operator)} ->
    Some (MathOperator {for_float = false; operator; is_constant})
  | _ -> if Ast_await.is_await_expr sexp then Some Await else None

let type_clash_context_for_function_argument ~label type_clash_context sarg0 =
  match type_clash_context with
  | Some (MathOperator {for_float; operator}) ->
    Some
      (MathOperator
         {
           for_float;
           operator;
           is_constant =
             (match sarg0.Parsetree.pexp_desc with
             | Pexp_constant (Pconst_integer (txt, _) | Pconst_float (txt, _))
               ->
               Some txt
             | _ -> None);
         })
  | None ->
    Some
      (FunctionArgument
         {
           optional = false;
           name =
             (match label with
             | Asttypes.Nolabel -> None
             | Optional {txt = l} | Labelled {txt = l} -> Some l);
         })
  | type_clash_context -> type_clash_context

let type_clash_context_maybe_option ty_expected ty_res =
  match (ty_expected, ty_res) with
  | ( {Types.desc = Tconstr (expected_path, _, _)},
      {Types.desc = Tconstr (type_path, _, _)} )
    when Path.same Predef.path_option type_path
         && Path.same expected_path Predef.path_option = false ->
    Some MaybeUnwrapOption
  | _ -> None

let type_clash_context_in_statement sexp =
  match sexp.Parsetree.pexp_desc with
  | Pexp_apply {transformed_jsx = false} -> Some (Statement FunctionCall)
  | _ -> None

let print_contextual_unification_error ppf t1 t2 =
  (* TODO: Maybe we should do the same for Null.t and Nullable.t as we do for options
     below, now that they also are more first class for values that might not exist? *)
  match (t1.Types.desc, t2.Types.desc) with
  | Tconstr (p1, _, _), Tconstr (p2, _, _)
    when Path.same p1 Predef.path_option
         && Path.same p2 Predef.path_option <> true ->
    fprintf ppf
      "@,\
       @\n\
       @[<v 0>You're expecting the value you're pattern matching on to be an \
       @{<info>option@}, but the value is actually not an option.@ Change your \
       pattern match to work on the concrete value (remove @{<error>Some(_)@} \
       or @{<error>None@} from the pattern) to make it work.@]"
  | Tconstr (p1, _, _), Tconstr (p2, _, _)
    when Path.same p2 Predef.path_option
         && Path.same p1 Predef.path_option <> true ->
    fprintf ppf
      "@,\
       @\n\
       @[<v 0>The value you're pattern matching on here is wrapped in an \
       @{<info>option@}, but you're trying to match on the actual value.@ Wrap \
       the highlighted pattern in @{<info>Some()@} to make it work.@]"
  | _ -> ()

let attributes_include_jsx_component_props (attrs : Parsetree.attributes) =
  attrs
  |> List.exists (fun ({Location.txt}, _) -> txt = "res.jsxComponentProps")

let path_to_jsx_component_name p =
  match p |> Path.name |> String.split_on_char '.' |> List.rev with
  | "props" :: component_name :: _ -> Some component_name
  | _ -> None

let get_jsx_component_props
    ~(extract_concrete_typedecl : extract_concrete_typedecl) env ty p =
  match p with
  | Path.Pdot (Path.Pident {Ident.name = jsx_module_name}, "fragmentProps", _)
    when Some jsx_module_name = !configured_jsx_module ->
    Some {props_record_path = p; fields = []; jsx_type = `Fragment}
  | _ -> (
    (* TODO: handle lowercase components using JSXDOM.domProps *)
    match Path.last p with
    | "props" -> (
      try
        match extract_concrete_typedecl env ty with
        | ( _p0,
            _p,
            {Types.type_kind = Type_record (fields, _repr); type_attributes} )
          when attributes_include_jsx_component_props type_attributes ->
          Some {props_record_path = p; fields; jsx_type = `CustomComponent}
        | _ -> None
      with _ -> None)
    | _ -> None)

let print_component_name ppf (p : Path.t) =
  match path_to_jsx_component_name p with
  | Some component_name -> fprintf ppf "@{<info><%s />@} " component_name
  | None -> ()

let print_component_wrong_prop_error ppf (p : Path.t)
    (_fields : Types.label_declaration list) name =
  fprintf ppf "@[<v>";
  (match name with
  | "children" ->
    fprintf ppf
      "@[<2>This JSX component does not accept child elements. It has no \
       @{<error>children@} prop "
  | _ ->
    fprintf ppf
      "@[<2>The prop @{<error>%s@} does not belong to the JSX component " name);
  print_component_name ppf p;
  fprintf ppf "@]@,@,"

let print_component_labels_missing_error ppf labels
    (error_info : jsx_prop_error_info) =
  fprintf ppf "@[<hov>The component ";
  print_component_name ppf error_info.props_record_path;
  fprintf ppf "is missing these required props:@\n";
  labels |> List.iter (fun lbl -> fprintf ppf "@ %s" lbl);
  fprintf ppf "@]"

let get_jsx_component_error_info ~extract_concrete_typedecl opath env ty_record
    () =
  match opath with
  | Some (p, _) ->
    get_jsx_component_props ~extract_concrete_typedecl env ty_record p
  | None -> None
