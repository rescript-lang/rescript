(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

open Format
open Asttypes

let rec struct_const ppf (cst : Lam_constant.t) =
  match cst with
  | Const_js_true -> fprintf ppf "#true"
  | Const_js_false -> fprintf ppf "#false"
  | Const_js_null -> fprintf ppf "#null"
  | Const_module_alias -> fprintf ppf "#alias"
  | Const_js_undefined _ -> fprintf ppf "#undefined"
  | Const_int {i} -> fprintf ppf "%ld" i
  | Const_char i -> fprintf ppf "%s" (Ext_util.string_of_int_as_char i)
  | Const_string {s} -> fprintf ppf "%S" s
  | Const_float f -> fprintf ppf "%s" f
  | Const_bigint (sign, i) -> fprintf ppf "%sn" (Bigint_utils.to_string sign i)
  | Const_pointer name -> fprintf ppf "`%s" name
  | Const_some n -> fprintf ppf "[some-c]%a" struct_const n
  | Const_block (tag, _, []) -> fprintf ppf "[%i]" tag
  | Const_block (tag, _, sc1 :: scl) ->
    let sconsts ppf scl =
      List.iter (fun sc -> fprintf ppf "@ %a" struct_const sc) scl
    in
    fprintf ppf "@[<1>[%i:@ @[%a%a@]]@]" tag struct_const sc1 sconsts scl

(* let string_of_loc_kind (loc : Lambda.loc_kind) =
   match loc with
   | Loc_FILE -> "loc_FILE"
   | Loc_LINE -> "loc_LINE"
   | Loc_MODULE -> "loc_MODULE"
   | Loc_POS -> "loc_POS"
   | Loc_LOC -> "loc_LOC" *)

let primitive ppf (prim : Lam_primitive.t) =
  match prim with
  (* | Pcreate_exception s -> fprintf ppf "[exn-create]%S" s  *)
  | Pcreate_extension s -> fprintf ppf "[ext-create]%S" s
  | Pwrap_exn -> fprintf ppf "#exn"
  | Pinit_mod -> fprintf ppf "init_mod!"
  | Pupdate_mod -> fprintf ppf "update_mod!"
  | Pjs_apply -> fprintf ppf "#apply"
  | Pjs_runtime_apply -> fprintf ppf "#runtime_apply"
  | Pjs_unsafe_downgrade {name; setter} ->
    if setter then fprintf ppf "##%s#=" name else fprintf ppf "##%s" name
  | Pfn_arity -> fprintf ppf "fn.length"
  | Pjs_fn_make i -> fprintf ppf "js_fn_make_%i" i
  | Pjs_fn_make_unit -> fprintf ppf "js_fn_make_unit"
  | Pjs_fn_method -> fprintf ppf "js_fn_method"
  | Pdebugger -> fprintf ppf "debugger"
  | Praw_js_code _ -> fprintf ppf "[raw]"
  | Ptypeof -> fprintf ppf "typeof"
  | Pnull_to_opt -> fprintf ppf "[null->opt]"
  | Pundefined_to_opt -> fprintf ppf "[undefined->opt]"
  | Pnull_undefined_to_opt -> fprintf ppf "[null/undefined->opt]"
  | Pis_null -> fprintf ppf "[?null]"
  | Pis_not_none -> fprintf ppf "[?is-not-none]"
  | Psome -> fprintf ppf "[some]"
  | Psome_not_nest -> fprintf ppf "[some-not-nest]"
  | Pval_from_option -> fprintf ppf "[?unbox]"
  | Pval_from_option_not_nest -> fprintf ppf "[?unbox-not-nest]"
  | Pis_undefined -> fprintf ppf "[?undefined]"
  | Pis_null_undefined -> fprintf ppf "[?null?undefined]"
  | Pimport -> fprintf ppf "[import]"
  | Pmakeblock (tag, _, Immutable) -> fprintf ppf "makeblock %i" tag
  | Pmakeblock (tag, _, Mutable) -> fprintf ppf "makemutable %i" tag
  | Pfield (n, field_info) -> (
    match Lam_compat.str_of_field_info field_info with
    | None -> fprintf ppf "field %i" n
    | Some s -> fprintf ppf "field %s/%i" s n)
  | Psetfield (n, _) ->
    let instr = "setfield " in
    fprintf ppf "%s%i" instr n
  | Pduprecord -> fprintf ppf "duprecord"
  | Pjs_call {prim_name} -> fprintf ppf "%s[js]" prim_name
  | Pjs_object_create _ -> fprintf ppf "[js.obj]"
  | Praise -> fprintf ppf "raise"
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
  | Pstringadd -> fprintf ppf "+*"
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
  | Pintcomp Ceq -> fprintf ppf "==[int]"
  | Pintcomp Cneq -> fprintf ppf "!=[int]"
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
  | Pdivbigint -> fprintf ppf "/"
  | Pmodbigint -> fprintf ppf "mod"
  | Ppowbigint -> fprintf ppf "**"
  | Pandbigint -> fprintf ppf "and"
  | Porbigint -> fprintf ppf "or"
  | Pxorbigint -> fprintf ppf "xor"
  | Plslbigint -> fprintf ppf "lsl"
  | Pasrbigint -> fprintf ppf "asr"
  | Pbigintcomp Ceq -> fprintf ppf "=="
  | Pbigintcomp Cneq -> fprintf ppf "!="
  | Pbigintcomp Clt -> fprintf ppf "<"
  | Pbigintcomp Cle -> fprintf ppf "<="
  | Pbigintcomp Cgt -> fprintf ppf ">"
  | Pbigintcomp Cge -> fprintf ppf ">="
  | Pbigintorder -> fprintf ppf "compare"
  | Pbigintmin -> fprintf ppf "min"
  | Pbigintmax -> fprintf ppf "max"
  | Pjscomp Ceq -> fprintf ppf "#=="
  | Pjscomp Cneq -> fprintf ppf "#!="
  | Pjscomp Clt -> fprintf ppf "#<"
  | Pjscomp Cle -> fprintf ppf "#<="
  | Pjscomp Cgt -> fprintf ppf "#>"
  | Pjscomp Cge -> fprintf ppf "#>="
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
  | Parraylength -> fprintf ppf "array.length"
  | Pmakearray -> fprintf ppf "makearray"
  | Pmakelist -> fprintf ppf "makelist"
  | Pmakedict -> fprintf ppf "makedict"
  | Parrayrefu -> fprintf ppf "array.unsafe_get"
  | Parraysetu -> fprintf ppf "array.unsafe_set"
  | Parrayrefs -> fprintf ppf "array.get"
  | Parraysets -> fprintf ppf "array.set"
  | Pisint -> fprintf ppf "isint"
  | Pis_poly_var_block -> fprintf ppf "#is_poly_var_block"
  | Pisout i -> fprintf ppf "isout %d" i
  | Pawait -> fprintf ppf "await"
  | Phash -> fprintf ppf "hash"
  | Phash_mixint -> fprintf ppf "hash_mix_int"
  | Phash_mixstring -> fprintf ppf "hash_mix_string"
  | Phash_finalmix -> fprintf ppf "hash_final_mix"

type print_kind = Alias | Strict | StrictOpt | Variable | Recursive

let kind = function
  | Alias -> "a"
  | Strict -> ""
  | StrictOpt -> "o"
  | Variable -> "v"
  | Recursive -> "r"

let to_print_kind (k : Lam_compat.let_kind) : print_kind =
  match k with
  | Alias -> Alias
  | Strict -> Strict
  | StrictOpt -> StrictOpt
  | Variable -> Variable

let rec aux (acc : (print_kind * Ident.t * Lam.t) list) (lam : Lam.t) =
  match lam with
  | Llet (str3, id3, arg3, body3) ->
    aux ((to_print_kind str3, id3, arg3) :: acc) body3
  | Lletrec (bind_args, body) ->
    aux
      (Ext_list.map_append bind_args acc (fun (id, l) -> (Recursive, id, l)))
      body
  | e -> (acc, e)

(* type left_var =
   {
    kind : print_kind ;
    id : Ident.t
   } *)

(* type left =
   | Id of left_var *)
(* | Nop *)

let flatten (lam : Lam.t) : (print_kind * Ident.t * Lam.t) list * Lam.t =
  match lam with
  | Llet (str, id, arg, body) -> aux [(to_print_kind str, id, arg)] body
  | Lletrec (bind_args, body) ->
    aux (Ext_list.map bind_args (fun (id, l) -> (Recursive, id, l))) body
  | _ -> assert false

(* let get_string ((id : Ident.t), (pos : int)) (env : Env.t) : string =
   match  Env.find_module (Pident id) env with
   | {md_type = Mty_signature signature  ; _ } ->
    (* Env.prefix_idents, could be cached  *)
    let serializable_sigs =
      Ext_list.filter (fun x ->
          match x with
          | Sig_typext _
          | Sig_module _
          | Sig_class _ -> true
          | Sig_value(_, {val_kind = Val_prim _}) -> false
                           | Sig_value _ -> true
                           | _ -> false
                           ) signature  in
                           (begin match Ext_list.nth_opt  serializable_sigs  pos  with
                           | Some (Sig_value (i,_)
                           | Sig_module (i,_,_)
                           | Sig_typext (i,_,_)
                           | Sig_modtype(i,_)
                           | Sig_class (i,_,_)
                           | Sig_class_type(i,_,_)
                           | Sig_type(i,_,_)) -> i
                           | None -> assert false
                           end).name
                           | _ -> assert false
*)

let lambda ppf v =
  let rec lam ppf (l : Lam.t) =
    match l with
    | Lvar id -> Ident.print ppf id
    | Lglobal_module (id, dynamic_import) ->
      fprintf ppf
        (if dynamic_import then "dynamic global %a" else "global %a")
        Ident.print id
    | Lconst cst -> struct_const ppf cst
    | Lapply {ap_func; ap_args; ap_info = {ap_inlined}} ->
      let lams ppf args = List.iter (fun l -> fprintf ppf "@ %a" lam l) args in
      fprintf ppf "@[<2>(apply%s@ %a%a)@]"
        (match ap_inlined with
        | Always_inline -> "%inlned"
        | _ -> "")
        lam ap_func lams ap_args
    | Lfunction {params; body; _} ->
      let pr_params ppf params =
        List.iter (fun param -> fprintf ppf "@ %a" Ident.print param) params
        (* | Tupled -> *)
        (*     fprintf ppf " ("; *)
        (*     let first = ref true in *)
        (*     List.iter *)
        (*       (fun param -> *)
        (*         if !first then first := false else fprintf ppf ",@ "; *)
        (*         Ident.print ppf param) *)
        (*       params; *)
        (*     fprintf ppf ")"  *)
      in
      fprintf ppf "@[<2>(function%a@ %a)@]" pr_params params lam body
    | (Llet _ | Lletrec _) as x ->
      let args, body = flatten x in
      let bindings ppf id_arg_list =
        let spc = ref false in
        List.iter
          (fun (k, id, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<2>%a =%s@ %a@]" Ident.print id (kind k) lam l)
          id_arg_list
      in
      fprintf ppf "@[<2>(let@ (@[<hv 1>%a@]" bindings (List.rev args);
      fprintf ppf ")@ %a)@]" lam body
    | Lprim
        {
          primitive = Pfield (n, Fld_module {name = s});
          args = [Lglobal_module (id, dynamic_import)];
          _;
        } ->
      fprintf ppf
        (if dynamic_import then "dynamic %s.%s/%d" else "%s.%s/%d")
        id.name s n
    | Lprim {primitive = prim; args = largs; _} ->
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs
      in
      fprintf ppf "@[<2>(%a%a)@]" primitive prim lams largs
    | Lswitch (larg, sw) ->
      let switch ppf (sw : Lam.lambda_switch) =
        let spc = ref false in
        List.iter
          (fun (n, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>case int %i %S:@ %a@]" n
              (match sw.sw_names with
              | None -> ""
              | Some x -> x.consts.(n).name)
              lam l)
          sw.sw_consts;
        List.iter
          (fun (n, l) ->
            if !spc then fprintf ppf "@ " else spc := true;
            fprintf ppf "@[<hv 1>case tag %i %S:@ %a@]" n
              (match sw.sw_names with
              | None -> ""
              | Some x -> x.blocks.(n).tag.name)
              lam l)
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
    | Lstringswitch (arg, cases, default) ->
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
      let lams ppf largs =
        List.iter (fun l -> fprintf ppf "@ %a" lam l) largs
      in
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
    | Lsequence (l1, l2) ->
      fprintf ppf "@[<2>(seq@ %a@ %a)@]" lam l1 sequence l2
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
  and sequence ppf = function
    | Lsequence (l1, l2) -> fprintf ppf "%a@ %a" sequence l1 sequence l2
    | l -> lam ppf l
  in
  lam ppf v

(* let structured_constant = struct_const *)

(* let rec flatten_seq acc (lam : Lam.t) =
   match lam with
   | Lsequence(l1,l2) ->
    flatten_seq (flatten_seq acc l1) l2
   | x -> x :: acc *)

(* exception Not_a_module *)

(* let rec flat (acc : (left * Lam.t) list ) (lam : Lam.t) =
   match lam with
   | Llet (str,id,arg,body) ->
    flat ( (Id {kind = to_print_kind str;  id}, arg) :: acc) body
   | Lletrec (bind_args, body) ->
    flat
      (Ext_list.map_append bind_args acc
        (fun (id, arg ) -> (Id {kind = Recursive;  id}, arg)) )
      body
   | Lsequence (l,r) ->
    flat (flat acc l) r
   | x -> (Nop, x) :: acc *)

(* let lambda_as_module env  ppf (lam : Lam.t) =
   try
    (* match lam with *)
    (* | Lprim {primitive = Psetglobal id ; args =  [biglambda]; _} *)
    (* might be wrong in toplevel *)
    (* -> *)

    begin match flat [] lam  with
      | (Nop, Lprim {primitive = Pmakeblock (_, _, _); args =  toplevels; _})
        :: rest ->
        (* let spc = ref false in *)
        List.iter
          (fun (left, l) ->
             match left with
             | Id { kind = k; id } ->
               fprintf ppf "@[<2>%a =%s@ %a@]@." Ident.print id (kind k) lambda l
             | Nop ->

               fprintf ppf "@[<2>%a@]@."  lambda l
          )

        @@ List.rev rest


      | _ -> raise Not_a_module
    end
   (* | _ -> raise Not_a_module *)
   with _ ->
    lambda ppf lam;
    fprintf ppf "; lambda-failure" *)

let serialize (filename : string) (lam : Lam.t) : unit =
  let ou = open_out filename in
  let old = Format.get_margin () in
  let () = Format.set_margin 10000 in
  let fmt = Format.formatter_of_out_channel ou in
  (* lambda_as_module env fmt lambda; *)
  lambda fmt lam;
  Format.pp_print_flush fmt ();
  close_out ou;
  Format.set_margin old

let lambda_to_string = Format.asprintf "%a" lambda

let primitive_to_string = Format.asprintf "%a" primitive
