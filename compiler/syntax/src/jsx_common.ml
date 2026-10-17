open Asttypes
open Parsetree

type jsx_config = {
  mutable version: int;
  mutable module_: string;
  mutable nested_modules: string list;
  mutable has_component: bool;
}

(* [@react.component] and [@react.componentWithProps] that are still among the
   attributes: [Ast_helper] takes the first one into the binding's or
   description's component node and leaves any other. *)
let is_component_attr (({txt}, _) : attribute) =
  match txt with
  | "react.component" | "jsx.component" -> true
  | _ -> false

let is_component_with_props_attr (({txt}, _) : attribute) =
  match txt with
  | "react.componentWithProps" | "jsx.componentWithProps" -> true
  | _ -> false

(* A component attribute with a payload [Ast_helper] cannot read stays an
   attribute, so formatting keeps it; reject it here instead of silently
   ignoring the payload. *)
let check_component_payloads attributes =
  List.iter
    (fun ((({txt; loc}, _) : attribute) as attr) ->
      if Ast_helper.Jsx_component.of_attribute attr = None then
        if is_component_attr attr then
          Location.raise_errorf ~loc
            "@@%s takes no payload, or the props type to share, as in @@%s(: \
             props)."
            txt txt
        else if is_component_with_props_attr attr then
          Location.raise_errorf ~loc "@@%s takes no payload." txt)
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

let async_component ~async expr =
  if async then
    let open Ast_helper in
    Exp.apply
      (Exp.ident {loc = Location.none; txt = Ldot (Lident "Jsx", "promise")})
      [(Nolabel, expr)]
  else expr
