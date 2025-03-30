let rec dig_async_payload_from_function (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_fun {async} -> async
  | Pexp_newtype (_, body) -> dig_async_payload_from_function body
  | _ -> false

let add_promise_type ?(loc = Location.none) ~async
    (result : Parsetree.expression) =
  if async then
    let unsafe_async =
      Ast_helper.Exp.ident ~loc
        {txt = Ldot (Lident Primitive_modules.promise, "unsafe_async"); loc}
    in
    Ast_helper.Exp.apply ~loc unsafe_async [(Nolabel, result)]
  else result

let rec add_promise_to_result ~loc (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_fun f ->
    let rhs = add_promise_to_result ~loc f.rhs in
    {e with pexp_desc = Pexp_fun {f with rhs}}
  | _ -> add_promise_type ~loc ~async:true e

let make_function_async ~async (e : Parsetree.expression) =
  if async then
    match e.pexp_desc with
    | Pexp_fun {lhs = {ppat_loc}} -> add_promise_to_result ~loc:ppat_loc e
    | _ -> assert false
  else e
