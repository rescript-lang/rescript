(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Format
open Asttypes
open Primitive
open Lambda

let rec struct_const ppf = function
  | Const_base (Const_int n) -> fprintf ppf "%i" n
  | Const_base (Const_char i) ->
    fprintf ppf "%s" (Pprintast.string_of_int_as_char i)
  | Const_base (Const_string (s, _)) -> fprintf ppf "%S" s
  | Const_immstring s -> fprintf ppf "#%S" s
  | Const_base (Const_float f) -> fprintf ppf "%s" f
  | Const_base (Const_int32 n) -> fprintf ppf "%lil" n
  | Const_base (Const_int64 n) -> fprintf ppf "%LiL" n
  | Const_base (Const_bigint (sign, n)) ->
    fprintf ppf "%sn" (Bigint_utils.to_string sign n)
  | Const_pointer (n, _) -> fprintf ppf "%ia" n
  | Const_block (tag_info, []) ->
    let tag = Lambda.tag_of_tag_info tag_info in
    fprintf ppf "[%i]" tag
  | Const_block (tag_info, sc1 :: scl) ->
    let tag = Lambda.tag_of_tag_info tag_info in
    let sconsts ppf scl =
      List.iter (fun sc -> fprintf ppf "@ %a" struct_const sc) scl
    in
    fprintf ppf "@[<1>[%i:@ @[%a%a@]]@]" tag struct_const sc1 sconsts scl
  | Const_false -> fprintf ppf "false"
  | Const_true -> fprintf ppf "true"

let value_kind = function
  | Pgenval -> ""

(* let field_kind = function
   | Pgenval -> "*"
   | Pintval -> "int"
   | Pfloatval -> "float"
   | Pboxedintval bi -> boxed_integer_name bi *)

let string_of_loc_kind = function
  | Loc_FILE -> "loc_FILE"
  | Loc_LINE -> "loc_LINE"
  | Loc_MODULE -> "loc_MODULE"
  | Loc_POS -> "loc_POS"
  | Loc_LOC -> "loc_LOC"

(* let block_shape ppf shape = match shape with
   | None | Some [] -> ()
   | Some l when List.for_all ((=) Pgenval) l -> ()
   | Some [elt] ->
       Format.fprintf ppf " (%s)" (field_kind elt)
   | Some (h :: t) ->
       Format.fprintf ppf " (%s" (field_kind h);
       List.iter (fun elt ->
           Format.fprintf ppf ",%s" (field_kind elt))
         t;
       Format.fprintf ppf ")" *)

let str_of_field_info (fld_info : Lambda.field_dbg_info) =
  match fld_info with
  | Fld_module {name}
  | Fld_record {name}
  | Fld_record_inline {name}
  | Fld_record_extension {name} ->
    name
  | Fld_tuple -> "[]"
  | Fld_poly_var_tag -> "`"
  | Fld_poly_var_content -> "#"
  | Fld_extension -> "ext"
  | Fld_variant -> "var"
  | Fld_cons -> "cons"
let print_taginfo ppf = function
  | Blk_extension -> fprintf ppf "ext"
  | Blk_record_ext {fields = ss} ->
    fprintf ppf "[%s]" (String.concat ";" (Array.to_list ss))
  | Blk_tuple -> fprintf ppf "tuple"
  | Blk_constructor {name; num_nonconst} ->
    fprintf ppf "%s/%i" name num_nonconst
  | Blk_poly_var name -> fprintf ppf "`%s" name
  | Blk_record {fields = ss} ->
    fprintf ppf "[%s]" (String.concat ";" (List.map fst (Array.to_list ss)))
  | Blk_module ss -> fprintf ppf "[%s]" (String.concat ";" ss)
  | Blk_some -> fprintf ppf "some"
  | Blk_some_not_nested -> fprintf ppf "some_not_nested"
  | Blk_lazy_general -> fprintf ppf "lazy_general"
  | Blk_module_export _ -> fprintf ppf "module/exports"
  | Blk_record_inlined {fields = ss} ->
    fprintf ppf "[%s]" (String.concat ";" (List.map fst (Array.to_list ss)))

let primitive ppf = function
  | Pidentity -> fprintf ppf "id"
  | Pignore -> fprintf ppf "ignore"
  | Pdebugger -> fprintf ppf "debugger"
  | Ptypeof -> fprintf ppf "typeof"
  | Pnull -> fprintf ppf "null"
  | Pundefined -> fprintf ppf "undefined"
  | Pfn_arity -> fprintf ppf "fn.length"
  | Prevapply -> fprintf ppf "revapply"
  | Pdirapply -> fprintf ppf "dirapply"
  | Ploc kind -> fprintf ppf "%s" (string_of_loc_kind kind)
  | Pgetglobal id -> fprintf ppf "global %a" Ident.print id
  | Pmakeblock taginfo -> fprintf ppf "makeblock %a" print_taginfo taginfo
  | Pfield (n, fld) -> fprintf ppf "field:%s/%i" (str_of_field_info fld) n
  | Psetfield (n, _) -> fprintf ppf "setfield %i" n
  | Pduprecord -> fprintf ppf "duprecord"
  | Pccall p -> fprintf ppf "%s" p.prim_name
  | Praise k -> fprintf ppf "%s" (Lambda.raise_kind k)
  | Pobjcomp Ceq -> fprintf ppf "=="
  | Pobjcomp Cneq -> fprintf ppf "!="
  | Pobjcomp Clt -> fprintf ppf "<"
  | Pobjcomp Cle -> fprintf ppf "<="
  | Pobjcomp Cgt -> fprintf ppf ">"
  | Pobjcomp Cge -> fprintf ppf ">="
  | Pobjorder -> fprintf ppf "compare"
  | Pobjmin -> fprintf ppf "min"
  | Pobjmax -> fprintf ppf "max"
  | Pobjtag -> fprintf ppf "tag"
  | Pobjsize -> fprintf ppf "length"
  | Psequand -> fprintf ppf "&&"
  | Psequor -> fprintf ppf "||"
  | Pnot -> fprintf ppf "not"
  | Pboolcomp Ceq -> fprintf ppf "=="
  | Pboolcomp Cneq -> fprintf ppf "!="
  | Pboolcomp Clt -> fprintf ppf "<"
  | Pboolcomp Cle -> fprintf ppf "<="
  | Pboolcomp Cgt -> fprintf ppf ">"
  | Pboolcomp Cge -> fprintf ppf ">="
  | Pboolorder -> fprintf ppf "compare"
  | Pboolmin -> fprintf ppf "min"
  | Pboolmax -> fprintf ppf "max"
  | Pnegint -> fprintf ppf "~"
  | Paddint -> fprintf ppf "+"
  | Psubint -> fprintf ppf "-"
  | Pmulint -> fprintf ppf "*"
  | Pdivint -> fprintf ppf "/"
  | Pmodint -> fprintf ppf "mod"
  | Pandint -> fprintf ppf "and"
  | Porint -> fprintf ppf "or"
  | Pxorint -> fprintf ppf "xor"
  | Plslint -> fprintf ppf "lsl"
  | Plsrint -> fprintf ppf "lsr"
  | Pasrint -> fprintf ppf "asr"
  | Pintcomp Ceq -> fprintf ppf "=="
  | Pintcomp Cneq -> fprintf ppf "!="
  | Pintcomp Clt -> fprintf ppf "<"
  | Pintcomp Cle -> fprintf ppf "<="
  | Pintcomp Cgt -> fprintf ppf ">"
  | Pintcomp Cge -> fprintf ppf ">="
  | Pintorder -> fprintf ppf "compare"
  | Pintmin -> fprintf ppf "min"
  | Pintmax -> fprintf ppf "max"
  | Poffsetint n -> fprintf ppf "%i+" n
  | Poffsetref n -> fprintf ppf "+:=%i" n
  | Pintoffloat -> fprintf ppf "int_of_float"
  | Pfloatofint -> fprintf ppf "float_of_int"
  | Pnegfloat -> fprintf ppf "~."
  | Pabsfloat -> fprintf ppf "abs."
  | Paddfloat -> fprintf ppf "+."
  | Psubfloat -> fprintf ppf "-."
  | Pmulfloat -> fprintf ppf "*."
  | Pdivfloat -> fprintf ppf "/."
  | Pmodfloat -> fprintf ppf "mod"
  | Pfloatcomp Ceq -> fprintf ppf "==."
  | Pfloatcomp Cneq -> fprintf ppf "!=."
  | Pfloatcomp Clt -> fprintf ppf "<."
  | Pfloatcomp Cle -> fprintf ppf "<=."
  | Pfloatcomp Cgt -> fprintf ppf ">."
  | Pfloatcomp Cge -> fprintf ppf ">=."
  | Pfloatorder -> fprintf ppf "compare"
  | Pfloatmin -> fprintf ppf "min"
  | Pfloatmax -> fprintf ppf "max"
  | Pnegbigint -> fprintf ppf "~"
  | Paddbigint -> fprintf ppf "+"
  | Psubbigint -> fprintf ppf "-"
  | Pmulbigint -> fprintf ppf "*"
  | Ppowbigint -> fprintf ppf "**"
  | Pandbigint -> fprintf ppf "and"
  | Porbigint -> fprintf ppf "or"
  | Pxorbigint -> fprintf ppf "xor"
  | Plslbigint -> fprintf ppf "lsl"
  | Pasrbigint -> fprintf ppf "asr"
  | Pdivbigint -> fprintf ppf "/"
  | Pmodbigint -> fprintf ppf "mod"
  | Pbigintcomp Ceq -> fprintf ppf "==,"
  | Pbigintcomp Cneq -> fprintf ppf "!=,"
  | Pbigintcomp Clt -> fprintf ppf "<,"
  | Pbigintcomp Cle -> fprintf ppf "<=,"
  | Pbigintcomp Cgt -> fprintf ppf ">,"
  | Pbigintcomp Cge -> fprintf ppf ">=,"
  | Pbigintorder -> fprintf ppf "compare"
  | Pbigintmin -> fprintf ppf "min"
  | Pbigintmax -> fprintf ppf "max"
  | Pstringlength -> fprintf ppf "string.length"
  | Pstringrefu -> fprintf ppf "string.unsafe_get"
  | Pstringrefs -> fprintf ppf "string.get"
  | Pstringcomp Ceq -> fprintf ppf "=="
  | Pstringcomp Cneq -> fprintf ppf "!="
  | Pstringcomp Clt -> fprintf ppf "<"
  | Pstringcomp Cle -> fprintf ppf "<="
  | Pstringcomp Cgt -> fprintf ppf ">"
  | Pstringcomp Cge -> fprintf ppf ">="
  | Pstringorder -> fprintf ppf "compare"
  | Pstringmin -> fprintf ppf "min"
  | Pstringmax -> fprintf ppf "max"
  | Pstringadd -> fprintf ppf "string.concat"
  | Parraylength -> fprintf ppf "array.length"
  | Pmakearray Mutable -> fprintf ppf "makearray"
  | Pmakearray Immutable -> fprintf ppf "makearray_imm"
  | Parrayrefu -> fprintf ppf "array.unsafe_get"
  | Parraysetu -> fprintf ppf "array.unsafe_set"
  | Parrayrefs -> fprintf ppf "array.get"
  | Parraysets -> fprintf ppf "array.set"
  | Pmakelist Mutable -> fprintf ppf "makelist"
  | Pmakelist Immutable -> fprintf ppf "makelist_imm"
  | Pmakedict -> fprintf ppf "makedict"
  | Pisint -> fprintf ppf "isint"
  | Pisout -> fprintf ppf "isout"
  | Pisnullable -> fprintf ppf "isnullable"
  | Pcreate_extension s -> fprintf ppf "extension[%s]" s
  | Pextension_slot_eq -> fprintf ppf "#extension_slot_eq"
  | Pwrap_exn -> fprintf ppf "wrap_exn"
  | Pawait -> fprintf ppf "await"
  | Pimport -> fprintf ppf "import"
  | Pinit_mod -> fprintf ppf "#init_mod"
  | Pupdate_mod -> fprintf ppf "#update_mod"
  | Phash -> fprintf ppf "hash"
  | Phash_mixint -> fprintf ppf "hash_mix_int"
  | Phash_mixstring -> fprintf ppf "hash_mix_string"
  | Phash_finalmix -> fprintf ppf "hash_final_mix"
  | Pcurry_apply i -> fprintf ppf "apply[%d]" i
  | Pjscomp Ceq -> fprintf ppf "=="
  | Pjscomp Cneq -> fprintf ppf "!="
  | Pjscomp Clt -> fprintf ppf "<"
  | Pjscomp Cle -> fprintf ppf "<="
  | Pjscomp Cgt -> fprintf ppf ">"
  | Pjscomp Cge -> fprintf ppf ">="
  | Pundefined_to_opt -> fprintf ppf "undefined_to_opt"
  | Pnull_to_opt -> fprintf ppf "null_to_opt"
  | Pnullable_to_opt -> fprintf ppf "nullable_to_opt"
  | Pis_not_none -> fprintf ppf "#is_not_none"
  | Pval_from_option -> fprintf ppf "#val_from_option"
  | Pval_from_option_not_nest -> fprintf ppf "#val_from_option_not_nest"
  | Pis_poly_var_block -> fprintf ppf "#is_poly_var_block"
  | Pjs_raw_expr -> fprintf ppf "#raw_expr"
  | Pjs_raw_stmt -> fprintf ppf "#raw_stmt"
  | Pjs_fn_make arity -> fprintf ppf "#fn_mk(%d)" arity
  | Pjs_fn_make_unit -> fprintf ppf "#fn_mk_unit"
  | Pjs_fn_method -> fprintf ppf "#fn_method"

let function_attribute ppf {inline; is_a_functor; return_unit} =
  if is_a_functor then fprintf ppf "is_a_functor@ ";
  if return_unit then fprintf ppf "void@ ";
  match inline with
  | Default_inline -> ()
  | Always_inline -> fprintf ppf "always_inline@ "
  | Never_inline -> fprintf ppf "never_inline@ "

let apply_inlined_attribute ppf = function
  | Default_inline -> ()
  | Always_inline -> fprintf ppf " always_inline"
  | Never_inline -> fprintf ppf " never_inline"

let rec lam ppf = function
  | Lvar id -> Ident.print ppf id
  | Lconst cst -> struct_const ppf cst
  | Lapply ap ->
    let lams ppf largs = List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
    fprintf ppf "@[<2>(apply@ %a%a%a)@]" lam ap.ap_func lams ap.ap_args
      apply_inlined_attribute ap.ap_inlined
  | Lfunction {params; body; attr} ->
    let pr_params ppf params =
      List.iter (fun param -> fprintf ppf "@ %a" Ident.print param) params
    in
    fprintf ppf "@[<2>(function%a@ %a%a)@]" pr_params params function_attribute
      attr lam body
  | Llet (str, k, id, arg, body) ->
    let kind = function
      | Alias -> "a"
      | Strict -> ""
      | StrictOpt -> "o"
      | Variable -> "v"
    in
    let rec letbody = function
      | Llet (str, k, id, arg, body) ->
        fprintf ppf "@ @[<2>%a =%s%s@ %a@]" Ident.print id (kind str)
          (value_kind k) lam arg;
        letbody body
      | expr -> expr
    in
    fprintf ppf "@[<2>(let@ @[<hv 1>(@[<2>%a =%s%s@ %a@]" Ident.print id
      (kind str) (value_kind k) lam arg;
    let expr = letbody body in
    fprintf ppf ")@]@ %a)@]" lam expr
  | Lletrec (id_arg_list, body) ->
    let bindings ppf id_arg_list =
      let spc = ref false in
      List.iter
        (fun (id, l) ->
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<2>%a@ %a@]" Ident.print id lam l)
        id_arg_list
    in
    fprintf ppf "@[<2>(letrec@ (@[<hv 1>%a@])@ %a)@]" bindings id_arg_list lam
      body
  | Lprim (prim, largs, _) ->
    let lams ppf largs = List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
    fprintf ppf "@[<2>(%a%a)@]" primitive prim lams largs
  | Lswitch (larg, sw, _loc) ->
    let switch ppf sw =
      let spc = ref false in
      List.iter
        (fun (n, l) ->
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<hv 1>case int %i:@ %a@]" n lam l)
        sw.sw_consts;
      List.iter
        (fun (n, l) ->
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<hv 1>case tag %i:@ %a@]" n lam l)
        sw.sw_blocks;
      match sw.sw_failaction with
      | None -> ()
      | Some l ->
        if !spc then fprintf ppf "@ " else spc := true;
        fprintf ppf "@[<hv 1>default:@ %a@]" lam l
    in
    fprintf ppf "@[<1>(%s %a@ @[<v 0>%a@])@]"
      (match sw.sw_failaction with
      | None -> "switch*"
      | _ -> "switch")
      lam larg switch sw
  | Lstringswitch (arg, cases, default, _) ->
    let switch ppf cases =
      let spc = ref false in
      List.iter
        (fun (s, l) ->
          if !spc then fprintf ppf "@ " else spc := true;
          fprintf ppf "@[<hv 1>case \"%s\":@ %a@]" (String.escaped s) lam l)
        cases;
      match default with
      | Some default ->
        if !spc then fprintf ppf "@ " else spc := true;
        fprintf ppf "@[<hv 1>default:@ %a@]" lam default
      | None -> ()
    in
    fprintf ppf "@[<1>(stringswitch %a@ @[<v 0>%a@])@]" lam arg switch cases
  | Lstaticraise (i, ls) ->
    let lams ppf largs = List.iter (fun l -> fprintf ppf "@ %a" lam l) largs in
    fprintf ppf "@[<2>(exit@ %d%a)@]" i lams ls
  | Lstaticcatch (lbody, (i, vars), lhandler) ->
    fprintf ppf "@[<2>(catch@ %a@;<1 -1>with (%d%a)@ %a)@]" lam lbody i
      (fun ppf vars ->
        match vars with
        | [] -> ()
        | _ -> List.iter (fun x -> fprintf ppf " %a" Ident.print x) vars)
      vars lam lhandler
  | Ltrywith (lbody, param, lhandler) ->
    fprintf ppf "@[<2>(try@ %a@;<1 -1>with %a@ %a)@]" lam lbody Ident.print
      param lam lhandler
  | Lifthenelse (lcond, lif, lelse) ->
    fprintf ppf "@[<2>(if@ %a@ %a@ %a)@]" lam lcond lam lif lam lelse
  | Lsequence (l1, l2) -> fprintf ppf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
  | Lwhile (lcond, lbody) ->
    fprintf ppf "@[<2>(while@ %a@ %a)@]" lam lcond lam lbody
  | Lfor (param, lo, hi, dir, body) ->
    fprintf ppf "@[<2>(for %a@ %a@ %s@ %a@ %a)@]" Ident.print param lam lo
      (match dir with
      | Upto -> "to"
      | Downto -> "downto")
      lam hi lam body
  | Lassign (id, expr) ->
    fprintf ppf "@[<2>(assign@ %a@ %a)@]" Ident.print id lam expr
  | Lsend (name, obj, _) -> fprintf ppf "@[<2>(send%s@ %a@ )@]" name lam obj

and sequence ppf = function
  | Lsequence (l1, l2) -> fprintf ppf "%a@ %a" sequence l1 sequence l2
  | l -> lam ppf l

let structured_constant = struct_const

let lambda = lam
