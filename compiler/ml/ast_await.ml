let create_await_expression (e : Parsetree.expression) =
  let loc = {e.pexp_loc with loc_ghost = true} in
  let unsafe_await =
    Ast_helper.Exp.ident ~loc
      {txt = Ldot (Lident Primitive_modules.promise, "unsafe_await"); loc}
  in
  Ast_helper.Exp.apply ~loc unsafe_await [(Nolabel, e)]

let is_await_expr (e : Parsetree.expression) =
  match e with
  | {
   pexp_loc = {loc_ghost = true};
   pexp_desc =
     Pexp_apply
       {
         funct =
           {
             pexp_loc = {loc_ghost = true};
             pexp_desc = Pexp_ident {txt = Ldot (Lident ident, "unsafe_await")};
           };
         args = [(Nolabel, _)];
       };
  }
    when ident = Primitive_modules.promise ->
    true
  | _ -> false

(* An awaited module path: [await M], and [await (M: S)] or [(await M: S)].
   Returns [M], and [S] for the constrained forms. It is a dynamic import
   when [S] is absent or a module type path. *)
let awaited_module_path (e : Parsetree.module_expr) =
  match e.pmod_desc with
  | Pmod_await {pmod_desc = Pmod_ident lid} -> Some (lid, None)
  | Pmod_await {pmod_desc = Pmod_constraint ({pmod_desc = Pmod_ident lid}, mty)}
  | Pmod_constraint ({pmod_desc = Pmod_await {pmod_desc = Pmod_ident lid}}, mty)
    ->
    Some (lid, Some mty)
  | _ -> None

(* Transform a dynamic import form [await M] to
   unpack(await import(module(M: __M0__))) *)
let create_await_module_expression ~module_type_lid (e : Parsetree.module_expr)
    =
  let open Ast_helper in
  let rec remove_await (m : Parsetree.module_expr) =
    match m.pmod_desc with
    | Pmod_await inner -> inner
    | Pmod_constraint (inner, mty) ->
      {m with pmod_desc = Pmod_constraint (remove_await inner, mty)}
    | _ -> m
  in
  {
    e with
    pmod_desc =
      Pmod_unpack
        (create_await_expression
           (Exp.apply ~loc:e.pmod_loc
              (Exp.ident ~loc:e.pmod_loc
                 {
                   txt =
                     Longident.Ldot (Lident Primitive_modules.module_, "import");
                   loc = e.pmod_loc;
                 })
              [
                ( Nolabel,
                  Exp.constraint_ ~loc:e.pmod_loc
                    (Exp.pack ~loc:e.pmod_loc (remove_await e))
                    (Typ.package ~loc:e.pmod_loc module_type_lid []) );
              ]));
  }
