(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type attr = Parsetree.attribute
type t = attr list
type ('a, 'b) st = {get: 'a option; set: 'b option}

let process_method_attributes_rev (attrs : t) =
  Ext_list.fold_left attrs
    ({get = None; set = None}, [])
    (fun (st, acc) (({txt; loc}, payload) as attr) ->
      match txt with
      | "get" (* @get{null; undefined}*) ->
        let result =
          Ext_list.fold_left (Ast_payload.ident_or_record_as_config loc payload)
            (false, false) (fun (null, undefined) ({txt; loc}, opt_expr) ->
              match txt with
              | "null" ->
                ( (match opt_expr with
                  | None -> true
                  | Some e -> Ast_payload.assert_bool_lit e),
                  undefined )
              | "undefined" ->
                ( null,
                  match opt_expr with
                  | None -> true
                  | Some e -> Ast_payload.assert_bool_lit e )
              | "nullable" -> (
                match opt_expr with
                | None -> (true, true)
                | Some e ->
                  let v = Ast_payload.assert_bool_lit e in
                  (v, v))
              | _ -> Bs_syntaxerr.err loc Unsupported_predicates)
        in

        ({st with get = Some result}, acc)
      | "set" ->
        let result =
          Ext_list.fold_left (Ast_payload.ident_or_record_as_config loc payload)
            `Get (fun _st ({txt; loc}, opt_expr) ->
              (*FIXME*)
              if txt = "no_get" then
                match opt_expr with
                | None -> `No_get
                | Some e ->
                  if Ast_payload.assert_bool_lit e then `No_get else `Get
              else Bs_syntaxerr.err loc Unsupported_predicates)
        in
        (* properties -- void
              [@@set{only}]
        *)
        ({st with set = Some result}, acc)
      | _ -> (st, attr :: acc))

type attr_kind = Nothing | Meth_callback of attr | Method of attr

let process_attributes_rev (attrs : t) : attr_kind * t =
  Ext_list.fold_left attrs (Nothing, [])
    (fun (st, acc) (({txt; loc}, _) as attr) ->
      match (txt, st) with
      | "this", (Nothing | Meth_callback _) -> (Meth_callback attr, acc)
      | "meth", (Nothing | Method _) -> (Method attr, acc)
      | "this", _ -> Bs_syntaxerr.err loc Conflict_bs_bs_this_bs_meth
      | _, _ -> (st, attr :: acc))

let external_attrs =
  [|
    "get";
    "set";
    "get_index";
    "return";
    "obj";
    "val";
    "module";
    "scope";
    "variadic";
    "send";
    "new";
    "set_index";
    Literals.gentype_import1;
    Literals.gentype_import2;
  |]

let first_char_special (x : string) =
  match String.unsafe_get x 0 with
  | '#' | '?' | '%' -> true
  | _ -> false

let prims_to_be_encoded (attrs : string list) =
  match attrs with
  | [] -> assert false (* normal val declaration *)
  | x :: _ when first_char_special x -> false
  | _ :: x :: _ when Ext_string.first_marshal_char x -> false
  | _ -> true

(**

   [@@inline]
   let a = 3

   [@@inline]
   let a : 3 

   They are not considered externals, they are part of the language
*)

let rs_externals (attrs : t) pval_prim =
  match (attrs, pval_prim) with
  | _, [] -> false
  (* This is  val *)
  | [], _ ->
    (* Not any attribute found *)
    prims_to_be_encoded pval_prim
  | _, _ ->
    Ext_list.exists_fst attrs (fun ({txt} : string Asttypes.loc) ->
        Ext_array.exists external_attrs (fun (x : string) -> txt = x))
    || prims_to_be_encoded pval_prim

let is_inline : attr -> bool = fun ({txt}, _) -> txt = "inline"

let has_inline_payload (attrs : t) = Ext_list.find_first attrs is_inline

let has_await_payload (attrs : t) = Ext_list.exists attrs Ast_await.is_await

type derive_attr = {bs_deriving: Ast_payload.action list option} [@@unboxed]

let process_derive_type (attrs : t) : derive_attr * t =
  Ext_list.fold_left attrs
    ({bs_deriving = None}, [])
    (fun (st, acc) (({txt; loc}, payload) as attr) ->
      match txt with
      | "deriving" -> (
        match st.bs_deriving with
        | None ->
          ( {
              bs_deriving =
                Some (Ast_payload.ident_or_record_as_config loc payload);
            },
            acc )
        | Some _ -> Bs_syntaxerr.err loc Duplicated_bs_deriving)
      | _ -> (st, attr :: acc))

(* duplicated attributes not allowed *)
let iter_process_bs_string_int_unwrap_uncurry (attrs : t) =
  let st = ref `Nothing in
  let assign v (({loc; _}, _) as attr : attr) =
    if !st = `Nothing then (
      Used_attributes.mark_used_attribute attr;
      st := v)
    else Bs_syntaxerr.err loc Conflict_attributes
  in
  Ext_list.iter attrs (fun (({txt; loc = _}, _) as attr) ->
      match txt with
      | "string" -> assign `String attr
      | "int" -> assign `Int attr
      | "ignore" -> assign `Ignore attr
      | "unwrap" -> assign `Unwrap attr
      | _ -> ());
  !st

let iter_process_bs_string_as (attrs : t) : string option =
  let st = ref None in
  Ext_list.iter attrs (fun (({txt; loc}, payload) as attr) ->
      match txt with
      | "as" ->
        if !st = None then (
          match Ast_payload.is_single_string payload with
          | None -> Bs_syntaxerr.err loc Expect_string_literal
          | Some (v, _dec) ->
            Used_attributes.mark_used_attribute attr;
            st := Some v)
        else raise (Ast_untagged_variants.Error (loc, Duplicated_bs_as))
      | _ -> ());
  !st
let has_bs_optional (attrs : t) : bool =
  Ext_list.exists attrs (fun (({txt}, _) as attr) ->
      match txt with
      | "optional" ->
        Used_attributes.mark_used_attribute attr;
        true
      | _ -> false)

let iter_process_bs_int_as (attrs : t) =
  let st = ref None in
  Ext_list.iter attrs (fun (({txt; loc}, payload) as attr) ->
      match txt with
      | "as" ->
        if !st = None then (
          match Ast_payload.is_single_int payload with
          | None -> Bs_syntaxerr.err loc Expect_int_literal
          | Some _ as v ->
            Used_attributes.mark_used_attribute attr;
            st := v)
        else raise (Ast_untagged_variants.Error (loc, Duplicated_bs_as))
      | _ -> ());
  !st

type as_const_payload = Int of int | Str of string * External_arg_spec.delim

let iter_process_bs_string_or_int_as (attrs : Parsetree.attributes) =
  let st = ref None in
  Ext_list.iter attrs (fun (({txt; loc}, payload) as attr) ->
      match txt with
      | "as" ->
        if !st = None then (
          Used_attributes.mark_used_attribute attr;
          match Ast_payload.is_single_int payload with
          | None -> (
            match payload with
            | PStr
                [
                  {
                    pstr_desc =
                      Pstr_eval
                        ( {
                            pexp_desc = Pexp_constant (Pconst_string (s, delim_));
                            pexp_loc;
                            _;
                          },
                          _ );
                    _;
                  };
                ]
              when Ast_utf8_string_interp.parse_processed_delim delim_ <> None
              -> (
              let delim =
                match Ast_utf8_string_interp.parse_processed_delim delim_ with
                | None -> assert false
                | Some delim -> delim
              in
              st := Some (Str (s, delim));
              if delim = DNoQuotes then
                (* check that it is a valid object literal *)
                match
                  Classify_function.classify
                    ~check:(pexp_loc, Bs_flow_ast_utils.flow_deli_offset delim_)
                    s
                with
                | Js_literal _ -> ()
                | _ ->
                  Location.raise_errorf ~loc:pexp_loc
                    "an object literal expected")
            | _ -> Bs_syntaxerr.err loc Expect_int_or_string_or_json_literal)
          | Some v -> st := Some (Int v))
        else raise (Ast_untagged_variants.Error (loc, Duplicated_bs_as))
      | _ -> ());
  !st

let locg = Location.none

let get : attr = ({txt = "get"; loc = locg}, Ast_payload.empty)

let get_index : attr = ({txt = "get_index"; loc = locg}, Ast_payload.empty)

let set : attr = ({txt = "set"; loc = locg}, Ast_payload.empty)

let internal_expansive : attr =
  ({txt = "internal.expansive"; loc = locg}, Ast_payload.empty)

let bs_return_undefined : attr =
  ( {txt = "return"; loc = locg},
    PStr
      [
        {
          pstr_desc =
            Pstr_eval
              ( {
                  pexp_desc =
                    Pexp_ident {txt = Lident "undefined_to_opt"; loc = locg};
                  pexp_loc = locg;
                  pexp_attributes = [];
                },
                [] );
          pstr_loc = locg;
        };
      ] )

let is_gentype (attr : attr) =
  match attr with
  | {Location.txt = "genType" | "gentype"; _}, _ -> true
  | _ -> false

let gentype : attr = ({txt = "genType"; loc = locg}, Ast_payload.empty)
