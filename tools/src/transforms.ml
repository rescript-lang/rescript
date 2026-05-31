let labelled_to_unlabelled_arguments_in_fn_definition (e : Parsetree.expression) :
    Parsetree.expression =
  (* `(~a, ~b, ~c) => ...` to `(a, b, c) => ...` *)
  let rec drop_labels (e : Parsetree.expression) : Parsetree.expression =
    match e.pexp_desc with
    | Pexp_fun
        {arg_label = Labelled _ | Optional _; default; lhs; rhs; arity; async}
      ->
      {
        e with
        pexp_desc =
          Pexp_fun
            {
              arg_label = Nolabel;
              default;
              lhs;
              rhs = drop_labels rhs;
              arity;
              async;
            };
      }
    | Pexp_fun {arg_label; default; lhs; rhs; arity; async} ->
      {
        e with
        pexp_desc =
          Pexp_fun {arg_label; default; lhs; rhs = drop_labels rhs; arity; async};
      }
    | _ -> e
  in
  drop_labels e

let maker_fn_to_record (e : Parsetree.expression) : Parsetree.expression =
  (* `ReactDOM.Style.make(~width="12px", ~height="12px", ())` to `{height: "12px", width: "12px"}` *)
  e

let dict_from_array_to_dict_literal_syntax (e : Parsetree.expression) :
    Parsetree.expression =
  (* `Dict.fromArray([("a", 1), ("b", 2)])` to `dict{"a": 1, "b": 2}` *)
  (* Elgible if all keys are strings *)
  e

let converted_literal_to_pure_literal (e : Parsetree.expression) :
    Parsetree.expression =
  (* `Float.fromInt(1)` to `1.`,  *)
  e

let drop_unit_arguments_in_apply (e : Parsetree.expression) : Parsetree.expression =
  (* Drop only unlabelled unit arguments from an application expression. *)
  let is_unit_expr (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_construct ({txt = Lident "()"}, None) -> true
    | _ -> false
  in
  match e.pexp_desc with
  | Pexp_apply {funct; args; partial; transformed_jsx} ->
    let args' =
      List.filter
        (fun (label, arg) ->
          match label with
          | Asttypes.Nolabel -> not (is_unit_expr arg)
          | _ -> true)
        args
    in
    {
      e with
      pexp_desc = Pexp_apply {funct; args = args'; partial; transformed_jsx};
    }
  | _ -> e

(* Registry of available transforms *)
type transform = Parsetree.expression -> Parsetree.expression

let registry : (string * transform) list =
  [
    ( "labelledToUnlabelledArgumentsInFnDefinition",
      labelled_to_unlabelled_arguments_in_fn_definition );
    ("makerFnToRecord", maker_fn_to_record);
    ("dictFromArrayToDictLiteralSyntax", dict_from_array_to_dict_literal_syntax);
    ("convertedLiteralToPureLiteral", converted_literal_to_pure_literal);
    ("dropUnitArgumentsInApply", drop_unit_arguments_in_apply);
  ]

let get (id : string) : transform option = List.assoc_opt id registry
