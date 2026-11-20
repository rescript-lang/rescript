open Asttypes
open Parsetree

type jsx_config = {
  mutable version: int;
  mutable module_: string;
  mutable nested_modules: string list;
  mutable has_component: bool;
}

(* The JSX transform used to ignore a payload on [@react.componentWithProps],
   so code may carry one, such as [(: props)] copied from [@react.component].
   The node cannot hold it, so the attribute stays for formatting to keep; take
   it here as the component, with a deprecation warning, unless the definition
   already has one. *)
let take_component_with_props_payload jsx_component attributes =
  match jsx_component with
  | Some _ -> (jsx_component, attributes)
  | None ->
    let rec go seen = function
      | [] -> (None, attributes)
      | ((({txt; loc}, _) : attribute) as attr) :: rest -> (
        let namespace =
          match txt with
          | "react.componentWithProps" -> Some Pjc_react
          | "jsx.componentWithProps" -> Some Pjc_jsx
          | _ -> None
        in
        match namespace with
        | Some pjc_namespace
          when Option.is_none (Ast_helper.Jsx_component.of_attribute attr) ->
          Location.deprecated ~use:loc loc
            (Printf.sprintf
               "@%s takes no payload. The payload has no effect; remove it." txt);
          ( Some {pjc_kind = Pjc_with_props; pjc_namespace; pjc_loc = loc},
            List.rev_append seen rest )
        | _ -> go (attr :: seen) rest)
    in
    go [] attributes

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
        match (txt, Ast_helper.Jsx_component.of_attribute attr) with
        | ("react.component" | "jsx.component"), None ->
          Location.raise_errorf ~loc
            "@@%s takes no payload, or the props type to share, as in @@%s(: \
             props)."
            txt txt
        | _ ->
          (* [take_component_with_props_payload] has taken a lone
             [@react.componentWithProps] whatever its payload. *)
          Location.raise_errorf ~loc
            "A definition can have only one JSX component annotation.")
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
