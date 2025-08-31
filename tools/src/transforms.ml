let labelledToUnlabelledArgumentsInFnDefinition (e : Parsetree.expression) :
    Parsetree.expression =
  (* `(~a, ~b, ~c) => ...` to `(a, b, c) => ...` *)
  let rec dropLabels (e : Parsetree.expression) : Parsetree.expression =
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
              rhs = dropLabels rhs;
              arity;
              async;
            };
      }
    | Pexp_fun {arg_label; default; lhs; rhs; arity; async} ->
      {
        e with
        pexp_desc =
          Pexp_fun {arg_label; default; lhs; rhs = dropLabels rhs; arity; async};
      }
    | _ -> e
  in
  dropLabels e

let makerFnToRecord (e : Parsetree.expression) : Parsetree.expression =
  (* `ReactDOM.Style.make(~width="12px", ~height="12px", ())` to `{height: "12px", width: "12px"}` *)
  e

let dictFromArrayToDictLiteralSyntax (e : Parsetree.expression) :
    Parsetree.expression =
  (* `Dict.fromArray([("a", 1), ("b", 2)])` to `dict{"a": 1, "b": 2}` *)
  (* Elgible if all keys are strings *)
  e

let convertedLiteralToPureLiteral (e : Parsetree.expression) :
    Parsetree.expression =
  (* `Float.fromInt(1)` to `1.`,  *)
  e

(* Registry of available transforms *)
type transform = Parsetree.expression -> Parsetree.expression

let registry : (string * transform) list =
  [
    ( "labelledToUnlabelledArgumentsInFnDefinition",
      labelledToUnlabelledArgumentsInFnDefinition );
    ("makerFnToRecord", makerFnToRecord);
    ("dictFromArrayToDictLiteralSyntax", dictFromArrayToDictLiteralSyntax);
    ("convertedLiteralToPureLiteral", convertedLiteralToPureLiteral);
  ]

let get (id : string) : transform option = List.assoc_opt id registry
