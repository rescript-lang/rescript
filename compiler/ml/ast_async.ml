let dig_async_payload_from_function (expr : Parsetree.expression) =
  match expr.pexp_desc with
  | Pexp_fun {async} -> async
  | Pexp_newtype _ ->
    let rec dig (e : Parsetree.expression) =
      match e.pexp_desc with
      | Pexp_newtype (_, body) -> dig body
      | Pexp_fun {async} -> async
      | _ -> false
    in
    dig expr
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

let add_promise_to_result ~loc (e : Parsetree.expression) =
  match e.pexp_desc with
  | Pexp_fun f ->
    let body = add_promise_type ~loc ~async:true f.body in
    {e with pexp_desc = Pexp_fun {f with body}}
  | _ -> add_promise_type ~loc ~async:true e

let make_function_async ~async (e : Parsetree.expression) =
  if async then
    match e.pexp_desc with
    | Pexp_fun {params = {p_pat = {ppat_loc}} :: _} ->
      add_promise_to_result ~loc:ppat_loc e
    | _ -> assert false
  else e
