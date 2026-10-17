open Asttypes
open Parsetree

type jsx_config = {
  mutable version: int;
  mutable module_: string;
  mutable nested_modules: string list;
  mutable has_component: bool;
}

(* [Ast_helper] takes the first [@react.component], or failing that the first
   [@react.componentWithProps], into the binding's or description's component
   node. One still among the attributes is a second annotation, or one with a
   payload the node cannot hold; it stays an attribute so that formatting keeps
   it, and the JSX transform rejects it here. *)
let reject_leftover_component_attrs attributes =
  List.iter
    (fun ((({txt; loc}, _) : attribute) as attr) ->
      match txt with
      | "react.component" | "jsx.component" | "react.componentWithProps"
      | "jsx.componentWithProps" -> (
        match Ast_helper.Jsx_component.of_attribute attr with
        | Some _ ->
          Location.raise_errorf ~loc
            "A definition can have only one JSX component annotation."
        | None -> (
          match txt with
          | "react.component" | "jsx.component" ->
            Location.raise_errorf ~loc
              "@@%s takes no payload, or the props type to share, as in @@%s(: \
               props)."
              txt txt
          | _ -> Location.raise_errorf ~loc "@@%s takes no payload." txt))
      | _ -> ())
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
