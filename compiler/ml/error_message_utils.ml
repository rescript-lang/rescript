type extract_concrete_typedecl =
  Env.t -> Types.type_expr -> Path.t * Path.t * Types.type_declaration

type type_clash_statement = FunctionCall
type type_clash_context =
  | SetRecordField
  | ArrayValue
  | MaybeUnwrapOption
  | IfCondition
  | IfReturn
  | Switch
  | StringConcat
  | ComparisonOperator
  | MathOperator of {
      for_float: bool;
      operator: string;
      is_constant: string option;
    }
  | FunctionArgument
  | Statement of type_clash_statement

let fprintf = Format.fprintf

let error_type_text ppf type_clash_context =
  let text =
    match type_clash_context with
    | Some (Statement FunctionCall) -> "This function call returns:"
    | Some (MathOperator {is_constant = Some _}) -> "This value has type:"
    | Some ArrayValue -> "This array item has type:"
    | Some SetRecordField ->
      "You're assigning something to this field that has type:"
    | _ -> "This has type:"
  in
  fprintf ppf "%s" text

let error_expected_type_text ppf type_clash_context =
  match type_clash_context with
  | Some FunctionArgument ->
    fprintf ppf "But this function argument is expecting:"
  | Some ComparisonOperator ->
    fprintf ppf "But it's being compared to something of type:"
  | Some Switch -> fprintf ppf "But this switch is expected to return:"
  | Some IfCondition ->
    fprintf ppf "But @{<info>if@} conditions must always be of type:"
  | Some IfReturn ->
    fprintf ppf "But this @{<info>if@} statement is expected to return:"
  | Some ArrayValue ->
    fprintf ppf "But this array is expected to have items of type:"
  | Some SetRecordField -> fprintf ppf "But this record field is of type:"
  | Some (Statement FunctionCall) -> fprintf ppf "But it's expected to return:"
  | Some (MathOperator {operator}) ->
    fprintf ppf
      "But it's being used with the @{<info>%s@} operator, which works on:"
      operator
  | Some StringConcat -> fprintf ppf "But string concatenation is expecting:"
  | _ -> fprintf ppf "But it's expected to have type:"

let is_record_type ~extract_concrete_typedecl ~env ty =
  try
    match extract_concrete_typedecl env ty with
    | _, _, {Types.type_kind = Type_record _; _} -> true
    | _ -> false
  with _ -> false

let print_extra_type_clash_help ~extract_concrete_typedecl ~env ppf trace
    type_clash_context =
  match (type_clash_context, trace) with
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
    (match (operator, trace) with
    | ( "+",
        [
          ({Types.desc = Tconstr (p1, _, _)}, _);
          ({desc = Tconstr (p2, _, _)}, _);
        ] )
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
        \  - Ensure all values in this calculation has the type @{<info>%s@}. \
         You can convert between floats and ints via \
         @{<info>Belt.Float.toInt@} and @{<info>Belt.Int.fromFloat@}."
        operator_text
        (if for_float then "float" else "int"));
    match (is_constant, trace) with
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
    | ( _,
        [
          ({Types.desc = Tconstr (p1, _, _)}, _);
          ({desc = Tconstr (p2, _, _)}, _);
        ] ) -> (
      match (Path.name p1, Path.name p2) with
      | "float", "int" | "int", "float" ->
        fprintf ppf
          "\n\
          \  - Change the operator to @{<info>%s@}, which works on @{<info>%s@}"
          operator_for_other_type
          (if for_float then "int" else "float")
      | _ -> ())
    | _ -> ())
  | Some Switch, _ ->
    fprintf ppf
      "\n\n\
      \  All branches in a @{<info>switch@} must return the same type. To fix \
       this, change your branch to return the expected type."
  | Some IfCondition, _ ->
    fprintf ppf
      "\n\n\
      \  To fix this, change the highlighted code so it evaluates to a \
       @{<info>bool@}."
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
       `yourValue->Belt.Option.getWithDefault(someDefaultValue)`"
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
  | ( _,
      [
        ({Types.desc = Tconstr (_p1, _, _)}, _); ({desc = Tconstr (p2, _, _)}, _);
      ] )
    when Path.same Predef.path_unit p2 ->
    fprintf ppf
      "\n\n\
      \  - Did you mean to assign this to a variable?\n\
      \  - If you don't care about the result of this expression, you can \
       assign it to @{<info>_@} via @{<info>let _ = ...@} or pipe it to \
       @{<info>ignore@} via @{<info>expression->ignore@}\n\n"
  | _, [({desc = Tobject _}, _); (({Types.desc = Tconstr _} as t1), _)]
    when is_record_type ~extract_concrete_typedecl ~env t1 ->
    fprintf ppf
      "\n\n\
      \  You're passing a @{<error>ReScript object@} where a @{<info>record@} \
       is expected. \n\n\
      \  - Did you mean to pass a record instead of an object? Objects are \
       written with quoted keys, and records with unquoted keys. Remove the \
       quotes from the object keys to pass it as a record instead of object. \n\n"
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
  | _ -> Some FunctionArgument

let type_clash_context_for_function_argument type_clash_context sarg0 =
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
  | type_clash_context -> type_clash_context

let type_clash_context_maybe_option ty_expected ty_res =
  match (ty_expected, ty_res) with
  | ( {Types.desc = Tconstr (expected_path, _, _)},
      {Types.desc = Tconstr (type_path, _, _)} )
    when Path.same Predef.path_option type_path
         && Path.same expected_path Predef.path_option = false
         && Path.same expected_path Predef.path_uncurried = false ->
    Some MaybeUnwrapOption
  | _ -> None

let type_clash_context_in_statement sexp =
  match sexp.Parsetree.pexp_desc with
  | Pexp_apply _ -> Some (Statement FunctionCall)
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

type jsx_prop_error_info = {
  fields: Types.label_declaration list;
  props_record_path: Path.t;
}

let attributes_include_jsx_component_props (attrs : Parsetree.attributes) =
  attrs
  |> List.exists (fun ({Location.txt}, _) -> txt = "res.jsxComponentProps")

let path_to_jsx_component_name p =
  match p |> Path.name |> String.split_on_char '.' |> List.rev with
  | "props" :: component_name :: _ -> Some component_name
  | _ -> None

let get_jsx_component_props
    ~(extract_concrete_typedecl : extract_concrete_typedecl) env ty p =
  match Path.last p with
  | "props" -> (
    try
      match extract_concrete_typedecl env ty with
      | ( _p0,
          _p,
          {Types.type_kind = Type_record (fields, _repr); type_attributes} )
        when attributes_include_jsx_component_props type_attributes ->
        Some {props_record_path = p; fields}
      | _ -> None
    with _ -> None)
  | _ -> None

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
