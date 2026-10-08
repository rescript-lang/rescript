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

(* [m] without its outer awaits, e.g. [await (await M)], whose attributes
   (e.g. [@warning]) stay on the module they wrap, after its own as in the v0
   encoding, and whether it had any ([awaited] for the awaits already
   removed) *)
let rec remove_awaits awaited (m : Parsetree.module_expr) =
  match m.pmod_desc with
  | Pmod_await inner ->
    remove_awaits true
      {inner with pmod_attributes = inner.pmod_attributes @ m.pmod_attributes}
  | _ -> (awaited, m)

(* An awaited module path: [await M], and [await (M: S)] or [(await M: S)],
   where [await] may be repeated, e.g. [await (await M)]. Returns the path of
   [M]; the module to import, which is [e] without its awaits, whose
   attributes (e.g. [@warning]) stay on the module they wrap; and [S] for the
   constrained forms. It is a dynamic import when [S] is absent or a module
   type path. *)
let awaited_module_path (e : Parsetree.module_expr) =
  match remove_awaits false e with
  | true, ({pmod_desc = Pmod_ident lid} as imported) ->
    Some (lid, imported, None)
  | awaited, ({pmod_desc = Pmod_constraint (m, mty)} as constrained) -> (
    match remove_awaits awaited m with
    | true, ({pmod_desc = Pmod_ident lid} as path) ->
      Some
        ( lid,
          {constrained with pmod_desc = Pmod_constraint (path, mty)},
          Some mty )
    | _ -> None)
  | _ -> None

(* Transform the dynamic import [e] of [imported], as returned by
   [awaited_module_path], to unpack(await import(module(M: __M0__))) *)
let create_await_module_expression ~module_type_lid (e : Parsetree.module_expr)
    imported =
  let open Ast_helper in
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
                    (Exp.pack ~loc:e.pmod_loc imported)
                    (Typ.package ~loc:e.pmod_loc module_type_lid []) );
              ]));
  }
