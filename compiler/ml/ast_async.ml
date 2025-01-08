let has_async_payload attrs =
  Ext_list.exists attrs (fun ({Location.txt}, _) -> txt = "res.async")

let add_async_attribute ~async (body : Parsetree.expression) =
  let add (exp : Parsetree.expression) =
    if has_async_payload exp.pexp_attributes then exp
    else
      {
        exp with
        pexp_attributes =
          ({txt = "res.async"; loc = Location.none}, PStr [])
          :: exp.pexp_attributes;
      }
  in
  if async then
    let rec add_to_fun (exp : Parsetree.expression) =
      match exp.pexp_desc with
      | Pexp_newtype (txt, e) ->
        {exp with pexp_desc = Pexp_newtype (txt, add_to_fun e)}
      | Pexp_fun _ -> add exp
      | _ -> exp
    in
    add (add_to_fun body)
  else body

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
