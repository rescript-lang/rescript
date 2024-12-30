open Asttypes
open Parsetree

type jsx_config = {
  mutable version: int;
  mutable module_: string;
  mutable mode: string;
  mutable nested_modules: string list;
  mutable has_component: bool;
}

(* Helper method to look up the [@react.component] attribute *)
let has_attr (loc, _) =
  match loc.txt with
  | "react.component" | "jsx.component" -> true
  | _ -> false

let has_attr_with_props (loc, _) =
  match loc.txt with
  | "react.componentWithProps" | "jsx.componentWithProps" -> true
  | _ -> false

(* Iterate over the attributes and try to find the [@react.component] attribute *)
let has_attr_on_binding pred {pvb_attributes} =
  List.find_opt pred pvb_attributes <> None

let core_type_of_attrs attributes =
  List.find_map
    (fun ({txt}, payload) ->
      match (txt, payload) with
      | ("react.component" | "jsx.component"), PTyp core_type -> Some core_type
      | _ -> None)
    attributes

let typ_vars_of_core_type {ptyp_desc} =
  match ptyp_desc with
  | Ptyp_constr (_, core_types) ->
    List.filter
      (fun {ptyp_desc} ->
        match ptyp_desc with
        | Ptyp_var _ -> true
        | _ -> false)
      core_types
  | _ -> []

let raise_error ~loc msg = Location.raise_errorf ~loc msg

let raise_error_multiple_component ~loc =
  raise_error ~loc
    "Only one component definition is allowed for each module. Move to a \
     submodule or other file if necessary."

let remove_arity binding =
  let rec remove_arity_record expr =
    match expr.pexp_desc with
    | _ when Ast_uncurried.expr_is_uncurried_fun expr ->
      Ast_uncurried.expr_extract_uncurried_fun expr
    | Pexp_newtype (label, e) ->
      {expr with pexp_desc = Pexp_newtype (label, remove_arity_record e)}
    | Pexp_apply (forward_ref, [(label, e)]) ->
      {
        expr with
        pexp_desc = Pexp_apply (forward_ref, [(label, remove_arity_record e)]);
      }
    | _ -> expr
  in
  {binding with pvb_expr = remove_arity_record binding.pvb_expr}

let async_component ~async expr =
  if async then
    let open Ast_helper in
    Exp.apply
      (Exp.ident
         {
           loc = Location.none;
           txt = Ldot (Lident "JsxPPXReactSupport", "asyncComponent");
         })
      [(Nolabel, expr)]
  else expr
