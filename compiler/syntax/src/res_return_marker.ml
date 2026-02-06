open Parsetree

let rec mark_expression ~is_tail expr =
  if is_tail then expr.pexp_is_return <- true;
  match expr.pexp_desc with
  | Pexp_ident _ | Pexp_constant _ -> ()
  | Pexp_fun {default; rhs; _} ->
    Option.iter (mark_expression ~is_tail:false) default;
    mark_expression ~is_tail:true rhs
  | Pexp_let (_, bindings, body) ->
    List.iter mark_value_binding bindings;
    mark_expression ~is_tail body
  | Pexp_apply {funct; args; _} ->
    mark_expression ~is_tail:false funct;
    List.iter (fun (_, arg) -> mark_expression ~is_tail:false arg) args
  | Pexp_match (scrutinee, cases) ->
    mark_expression ~is_tail:false scrutinee;
    List.iter (mark_case ~is_tail) cases
  | Pexp_try (body, handlers) ->
    mark_expression ~is_tail body;
    List.iter (mark_case ~is_tail) handlers
  | Pexp_tuple items | Pexp_array items ->
    List.iter (mark_expression ~is_tail:false) items
  | Pexp_construct (_, payload) | Pexp_variant (_, payload) ->
    Option.iter (mark_expression ~is_tail:false) payload
  | Pexp_record (fields, base) ->
    List.iter (fun {x} -> mark_expression ~is_tail:false x) fields;
    Option.iter (mark_expression ~is_tail:false) base
  | Pexp_field (record, _) | Pexp_send (record, _) ->
    mark_expression ~is_tail:false record
  | Pexp_setfield (record, _, value) ->
    mark_expression ~is_tail:false record;
    mark_expression ~is_tail:false value
  | Pexp_ifthenelse (cond, ifso, ifnot_opt) ->
    mark_expression ~is_tail:false cond;
    mark_expression ~is_tail ifso;
    Option.iter (mark_expression ~is_tail) ifnot_opt
  | Pexp_sequence (first, second) ->
    mark_expression ~is_tail:false first;
    mark_expression ~is_tail second
  | Pexp_while (cond, body) ->
    mark_expression ~is_tail:false cond;
    mark_expression ~is_tail:false body
  | Pexp_for (_pat, start, stop, _dir, body) ->
    mark_expression ~is_tail:false start;
    mark_expression ~is_tail:false stop;
    mark_expression ~is_tail:false body
  | Pexp_constraint (body, _)
  | Pexp_coerce (body, _, _)
  | Pexp_open (_, _, body)
  | Pexp_newtype (_, body)
  | Pexp_await body ->
    mark_expression ~is_tail body
  | Pexp_assert cond -> mark_expression ~is_tail:false cond
  | Pexp_letmodule (_, me, body) ->
    mark_module_expr me;
    mark_expression ~is_tail body
  | Pexp_letexception (_, body) -> mark_expression ~is_tail body
  | Pexp_pack me -> mark_module_expr me
  | Pexp_extension ext -> mark_extension ext
  | Pexp_jsx_element element -> mark_jsx_element element

and mark_case ~is_tail case =
  Option.iter (mark_expression ~is_tail:false) case.pc_guard;
  mark_expression ~is_tail case.pc_rhs

and mark_value_binding vb = mark_expression ~is_tail:false vb.pvb_expr

and mark_module_expr me =
  match me.pmod_desc with
  | Pmod_structure str -> mark_structure str
  | Pmod_functor (_, param, body) ->
    Option.iter mark_module_type param;
    mark_module_expr body
  | Pmod_apply (me1, me2) ->
    mark_module_expr me1;
    mark_module_expr me2
  | Pmod_constraint (me', mt) ->
    mark_module_expr me';
    mark_module_type mt
  | Pmod_unpack expr -> mark_expression ~is_tail:false expr
  | Pmod_extension ext -> mark_extension ext
  | Pmod_ident _ -> ()

and mark_module_type mt =
  match mt.pmty_desc with
  | Pmty_signature sig_ -> mark_signature sig_
  | Pmty_functor (_, param, res) ->
    Option.iter mark_module_type param;
    mark_module_type res
  | Pmty_with (mt', _) -> mark_module_type mt'
  | Pmty_typeof me -> mark_module_expr me
  | Pmty_extension ext -> mark_extension ext
  | Pmty_alias _ | Pmty_ident _ -> ()

and mark_signature sig_items = List.iter mark_signature_item sig_items

and mark_signature_item item =
  match item.psig_desc with
  | Psig_module md -> mark_module_type md.pmd_type
  | Psig_recmodule mds -> List.iter (fun md -> mark_module_type md.pmd_type) mds
  | Psig_modtype mtd -> Option.iter mark_module_type mtd.pmtd_type
  | Psig_include incl -> mark_module_type incl.pincl_mod
  | Psig_extension (ext, _) -> mark_extension ext
  | Psig_attribute attr -> mark_attribute attr
  | Psig_open _ | Psig_value _ | Psig_type _ | Psig_typext _ | Psig_exception _
    ->
    ()

and mark_extension (_id, payload) = mark_payload payload

and mark_attribute (_id, payload) = mark_payload payload

and mark_payload = function
  | PStr str -> mark_structure str
  | PSig sig_ -> mark_signature sig_
  | PTyp _ -> ()
  | PPat (_, expr_opt) -> Option.iter (mark_expression ~is_tail:false) expr_opt

and mark_structure_item item =
  match item.pstr_desc with
  | Pstr_value (_, bindings) -> List.iter mark_value_binding bindings
  | Pstr_eval (expr, _) -> mark_expression ~is_tail:false expr
  | Pstr_module mb -> mark_module_binding mb
  | Pstr_recmodule mbs -> List.iter mark_module_binding mbs
  | Pstr_include incl -> mark_module_expr incl.pincl_mod
  | Pstr_extension (ext, _) -> mark_extension ext
  | Pstr_attribute attr -> mark_attribute attr
  | Pstr_exception _ | Pstr_primitive _ | Pstr_type _ | Pstr_typext _
  | Pstr_open _ | Pstr_modtype _ ->
    ()

and mark_module_binding mb = mark_module_expr mb.pmb_expr

and mark_structure items = List.iter mark_structure_item items

and mark_jsx_element = function
  | Jsx_fragment fragment ->
    List.iter (mark_expression ~is_tail:false) fragment.jsx_fragment_children
  | Jsx_unary_element element -> mark_jsx_props element.jsx_unary_element_props
  | Jsx_container_element element ->
    mark_jsx_props element.jsx_container_element_props;
    List.iter
      (mark_expression ~is_tail:false)
      element.jsx_container_element_children

and mark_jsx_props props =
  List.iter
    (function
      | JSXPropPunning _ -> ()
      | JSXPropValue (_, _, expr) -> mark_expression ~is_tail:false expr
      | JSXPropSpreading (_, expr) -> mark_expression ~is_tail:false expr)
    props

let structure = mark_structure
let signature = mark_signature

let expression_is_return expr = expr.pexp_is_return
