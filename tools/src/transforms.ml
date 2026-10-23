let labelled_to_unlabelled_arguments_in_fn_definition (e : Parsetree.expression)
    : Parsetree.expression =
  (* `(~a, ~b, ~c) => ...` to `(a, b, c) => ...` *)
  let rec drop_labels (e : Parsetree.expression) : Parsetree.expression =
    match e.pexp_desc with
    | Pexp_fun {newtypes; params; body; async} ->
      {
        e with
        pexp_desc =
          Pexp_fun
            {
              newtypes;
              params =
                List.map
                  (fun (p : Parsetree.fun_param) -> {p with p_lbl = Nolabel})
                  params;
              body = drop_labels body;
              async;
            };
      }
    | _ -> e
  in
  drop_labels e

let drop_unit_arguments_in_apply (e : Parsetree.expression) :
    Parsetree.expression =
  (* Drop only unlabelled unit arguments from an application expression. *)
  let is_unit_expr (e : Parsetree.expression) =
    match e.pexp_desc with
    | Pexp_construct ({txt = Lident "()"}, {txt = []}) -> true
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
    ("dropUnitArgumentsInApply", drop_unit_arguments_in_apply);
  ]

let get (id : string) : transform option = List.assoc_opt id registry
