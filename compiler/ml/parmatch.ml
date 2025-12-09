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

(* Detection of partial matches and unused match cases. *)

open Misc
open Asttypes
open Types
open Typedtree

(*************************************)
(* Utilities for building patterns   *)
(*************************************)

let make_pat desc ty tenv =
  {
    pat_desc = desc;
    pat_loc = Location.none;
    pat_extra = [];
    pat_type = ty;
    pat_env = tenv;
    pat_attributes = [];
  }

let omega = make_pat Tpat_any Ctype.none Env.empty

let extra_pat =
  make_pat (Tpat_var (Ident.create "+", mknoloc "+")) Ctype.none Env.empty

let rec omegas i = if i <= 0 then [] else omega :: omegas (i - 1)

let omega_list l = List.map (fun _ -> omega) l

let zero = make_pat (Tpat_constant (Const_int 0)) Ctype.none Env.empty

(*******************)
(* Coherence check *)
(*******************)

(* For some of the operations we do in this module, we would like (because it
   simplifies matters) to assume that patterns appearing on a given column in a
   pattern matrix are /coherent/ (think "of the same type").
   Unfortunately that is not always true.

   Consider the following (well-typed) example:
   {[
     type _ t = S : string t | U : unit t

     let f (type a) (t1 : a t) (t2 : a t) (a : a) =
       match t1, t2, a with
       | U, _, () -> ()
       | _, S, "" -> ()
   ]}

   Clearly the 3rd column contains incoherent patterns.

   On the example above, most of the algorithms will explore the pattern matrix
   as illustrated by the following tree:

   {v
                                                   S
                                                -------> | "" |
                             U     | S, "" | __/         | () |
                         --------> | _, () |   \  ¬ S
        | U, _, () | __/                        -------> | () |
        | _, S, "" |   \
                        ---------> | S, "" | ----------> | "" |
                           ¬ U                    S
   v}

   where following an edge labelled by a pattern P means "assuming the value I
   am matching on is filtered by [P] on the column I am currently looking at,
   then the following submatrix is still reachable".

   Notice that at any point of that tree, if the first column of a matrix is
   incoherent, then the branch leading to it can only be taken if the scrutinee
   is ill-typed.
   In the example above the only case where we have a matrix with an incoherent
   first column is when we consider [t1, t2, a] to be [U, S, ...]. However such
   a value would be ill-typed, so we can never actually get there.

   Checking the first column at each step of the recursion and making the
   concious decision of "aborting" the algorithm whenever the first column
   becomes incoherent, allows us to retain the initial assumption in later
   stages of the algorithms.

   ---

   N.B. two patterns can be considered coherent even though they might not be of
   the same type.

   That's in part because we only care about the "head" of patterns and leave
   checking coherence of subpatterns for the next steps of the algorithm:
   ('a', 'b') and (1, ()) will be deemed coherent because they are both a tuples
   of arity 2 (we'll notice at a later stage the incoherence of 'a' and 1).

   But also because it can be hard/costly to determine exactly whether two
   patterns are of the same type or not (eg. in the example above with _ and S,
   but see also the module [Coherence_illustration] in
   testsuite/tests/basic-more/robustmatch.ml).

   For the moment our weak, loosely-syntactic, coherence check seems to be
   enough and we leave it to each user to consider (and document!) what happens
   when an "incoherence" is not detected by this check.
*)

let simplify_head_pat p k =
  let rec simplify_head_pat p k =
    match p.pat_desc with
    | Tpat_alias (p, _, _) -> simplify_head_pat p k
    | Tpat_var (_, _) -> omega :: k
    | Tpat_or (p1, p2, _) -> simplify_head_pat p1 (simplify_head_pat p2 k)
    | _ -> p :: k
  in
  simplify_head_pat p k

let rec simplified_first_col = function
  | [] -> []
  | [] :: _ -> assert false (* the rows are non-empty! *)
  | (p :: _) :: rows -> simplify_head_pat p (simplified_first_col rows)

(* Given the simplified first column of a matrix, this function first looks for
   a "discriminating" pattern on that column (i.e. a non-omega one) and then
   check that every other head pattern in the column is coherent with that one.
*)
let all_coherent column =
  let coherent_heads hp1 hp2 =
    match (hp1.pat_desc, hp2.pat_desc) with
    | (Tpat_var _ | Tpat_alias _ | Tpat_or _), _
    | _, (Tpat_var _ | Tpat_alias _ | Tpat_or _) ->
      assert false
    | Tpat_construct (_, c, _), Tpat_construct (_, c', _) ->
      c.cstr_consts = c'.cstr_consts && c.cstr_nonconsts = c'.cstr_nonconsts
    | Tpat_constant c1, Tpat_constant c2 -> (
      match (c1, c2) with
      | Const_char _, Const_char _
      | Const_int _, Const_int _
      | Const_int32 _, Const_int32 _
      | Const_int64 _, Const_int64 _
      | Const_bigint _, Const_bigint _
      | Const_float _, Const_float _
      | Const_string _, Const_string _ ->
        true
      | ( ( Const_char _ | Const_int _ | Const_int32 _ | Const_int64 _
          | Const_bigint _ | Const_float _ | Const_string _ ),
          _ ) ->
        false)
    | Tpat_tuple l1, Tpat_tuple l2 -> List.length l1 = List.length l2
    | ( Tpat_record ((_, lbl1, _, _) :: _, _),
        Tpat_record ((_, lbl2, _, _) :: _, _) ) ->
      Array.length lbl1.lbl_all = Array.length lbl2.lbl_all
    | Tpat_any, _
    | _, Tpat_any
    | Tpat_record ([], _), Tpat_record (_, _)
    | Tpat_record (_, _), Tpat_record ([], _)
    | Tpat_variant _, Tpat_variant _
    | Tpat_array _, Tpat_array _ ->
      true
    | _, _ -> false
  in
  match
    List.find
      (fun head_pat ->
        match head_pat.pat_desc with
        | Tpat_var _ | Tpat_alias _ | Tpat_or _ -> assert false
        | Tpat_any -> false
        | _ -> true)
      column
  with
  | exception Not_found ->
    (* only omegas on the column: the column is coherent. *)
    true
  | discr_pat -> List.for_all (coherent_heads discr_pat) column

let first_column simplified_matrix = List.map fst simplified_matrix

(***********************)
(* Compatibility check *)
(***********************)

(* Patterns p and q compatible means:
    there exists value V that matches both, However....

   The case of extension types is dubious, as constructor rebind permits
   that different constructors are the same (and are thus compatible).

   Compilation must take this into account, consider:

   type t = ..
   type t += A|B
   type t += C=A

   let f x y = match x,y with
   | true,A  -> '1'
   | _,C     -> '2'
   | false,A -> '3'
   | _,_     -> '_'

   As C is bound to A the value of f false A is '2' (and not '3' as it would
   be in the absence of rebinding).

   Not considering rebinding, patterns "false,A" and "_,C" are incompatible
   and the compiler can swap the second and third clause, resulting in the
   (more efficiently compiled) matching

   match x,y with
   | true,A  -> '1'
   | false,A -> '3'
   | _,C     -> '2'
   | _,_     -> '_'

   This is not correct: when C is bound to A, "f false A" returns '2' (not '3')


   However, diagnostics do not take constructor rebinding into account.
   Notice, that due to module abstraction constructor rebinding is hidden.

   module X : sig type t = .. type t += A|B end = struct
     type t = ..
     type t += A
     type t += B=A
   end

   open X

   let f x = match x with
   | A -> '1'
   | B -> '2'
   | _ -> '_'

   The second clause above will NOT (and cannot) be flagged as useless.

   Finally, there are two compatibility fonction
    compat p q      ---> 'syntactic compatibility, used for diagnostics.
    may_compat p q --->   a safe approximation of possible compat,
                          for compilation
*)

let is_absent tag row = Btype.row_field tag !row = Rabsent

let is_absent_pat p =
  match p.pat_desc with
  | Tpat_variant (tag, _, row) -> is_absent tag row
  | _ -> false

let const_compare x y =
  match (x, y) with
  | Const_float f1, Const_float f2 ->
    compare (float_of_string f1) (float_of_string f2)
  | Const_bigint (s1, b1), Const_bigint (s2, b2) ->
    Bigint_utils.compare (s1, b1) (s2, b2)
  | Const_string (s1, _), Const_string (s2, _) -> String.compare s1 s2
  | _, _ -> compare x y

let records_args l1 l2 =
  (* Invariant: fields are already sorted by Typecore.type_label_a_list *)
  let rec combine r1 r2 l1 l2 =
    match (l1, l2) with
    | [], [] -> (List.rev r1, List.rev r2)
    | [], (_, _, p2, _) :: rem2 -> combine (omega :: r1) (p2 :: r2) [] rem2
    | (_, _, p1, _) :: rem1, [] -> combine (p1 :: r1) (omega :: r2) rem1 []
    | (_, lbl1, p1, _) :: rem1, (_, lbl2, p2, _) :: rem2 ->
      if lbl1.lbl_pos < lbl2.lbl_pos then
        combine (p1 :: r1) (omega :: r2) rem1 l2
      else if lbl1.lbl_pos > lbl2.lbl_pos then
        combine (omega :: r1) (p2 :: r2) l1 rem2
      else (* same label on both sides *)
        combine (p1 :: r1) (p2 :: r2) rem1 rem2
  in
  combine [] [] l1 l2

module Compat = struct
  type eq_cd = constructor_description -> constructor_description -> bool

  let rec compat ~(equal_cd : eq_cd) p q =
    match (p.pat_desc, q.pat_desc) with
    (* Variables match any value *)
    | (Tpat_any | Tpat_var _), _ | _, (Tpat_any | Tpat_var _) -> true
    (* Structural induction *)
    | Tpat_alias (p, _, _), _ -> compat ~equal_cd p q
    | _, Tpat_alias (q, _, _) -> compat ~equal_cd p q
    | Tpat_or (p1, p2, _), _ -> compat ~equal_cd p1 q || compat ~equal_cd p2 q
    | _, Tpat_or (q1, q2, _) -> compat ~equal_cd p q1 || compat ~equal_cd p q2
    (* Constructors, with special case for extension *)
    | Tpat_construct (_, c1, ps1), Tpat_construct (_, c2, ps2) ->
      equal_cd c1 c2 && compats ~equal_cd ps1 ps2
    (* More standard stuff *)
    | Tpat_variant (l1, op1, _), Tpat_variant (l2, op2, _) ->
      l1 = l2 && ocompat ~equal_cd op1 op2
    | Tpat_constant c1, Tpat_constant c2 -> const_compare c1 c2 = 0
    | Tpat_tuple ps, Tpat_tuple qs -> compats ~equal_cd ps qs
    | Tpat_record (l1, _), Tpat_record (l2, _) ->
      let ps, qs = records_args l1 l2 in
      compats ~equal_cd ps qs
    | Tpat_array ps, Tpat_array qs ->
      List.length ps = List.length qs && compats ~equal_cd ps qs
    | _, _ -> false

  and ocompat ~equal_cd op oq =
    match (op, oq) with
    | None, None -> true
    | Some p, Some q -> compat ~equal_cd p q
    | None, Some _ | Some _, None -> false

  and compats ~equal_cd ps qs =
    match (ps, qs) with
    | [], [] -> true
    | p :: ps, q :: qs -> compat ~equal_cd p q && compats ~equal_cd ps qs
    | _, _ -> false
end

let equal_tag c1 c2 = Types.equal_tag c1.cstr_tag c2.cstr_tag

let compat = Compat.compat ~equal_cd:equal_tag

and compats = Compat.compats ~equal_cd:equal_tag

(* Due to (potential) rebinding, two extension constructors
   of the same arity type may equal *)

exception Empty (* Empty pattern *)

(****************************************)
(* Utilities for retrieving type paths  *)
(****************************************)

(* May need a clean copy, cf. PR#4745 *)
let clean_copy ty =
  if ty.level = Btype.generic_level then ty
  else Subst.type_expr Subst.identity ty

let get_type_path ty tenv =
  let ty = Ctype.repr (Ctype.expand_head tenv (clean_copy ty)) in
  match ty.desc with
  | Tconstr (path, _, _) -> path
  | _ -> fatal_error "Parmatch.get_type_path"

(*************************************)
(* Values as patterns pretty printer *)
(*************************************)

let print_res_pat : (Typedtree.pattern -> string) ref =
  ref (fun _ -> assert false)

open Format

let is_cons = function
  | {cstr_name = "::"} -> true
  | _ -> false

let pretty_const c =
  match c with
  | Const_int i -> Printf.sprintf "%d" i
  | Const_char i -> Printf.sprintf "%s" (Pprintast.string_of_int_as_char i)
  | Const_string (s, _) -> Printf.sprintf "%S" s
  | Const_float f -> Printf.sprintf "%s" f
  | Const_int32 i -> Printf.sprintf "%ldl" i
  | Const_int64 i -> Printf.sprintf "%LdL" i
  | Const_bigint (sign, i) ->
    Printf.sprintf "%s" (Bigint_utils.to_string sign i)

let rec pretty_val ppf v =
  match v.pat_extra with
  | (cstr, _loc, _attrs) :: rem -> (
    match cstr with
    | Tpat_unpack ->
      fprintf ppf "@[(module %a)@]" pretty_val {v with pat_extra = rem}
    | Tpat_constraint _ ->
      fprintf ppf "@[(%a : _)@]" pretty_val {v with pat_extra = rem}
    | Tpat_type _ ->
      fprintf ppf "@[(# %a)@]" pretty_val {v with pat_extra = rem}
    | Tpat_open _ ->
      fprintf ppf "@[(# %a)@]" pretty_val {v with pat_extra = rem})
  | [] -> (
    match v.pat_desc with
    | Tpat_any -> fprintf ppf "_"
    | Tpat_var (x, _) -> fprintf ppf "%s" (Ident.name x)
    | Tpat_constant c -> fprintf ppf "%s" (pretty_const c)
    | Tpat_tuple vs -> fprintf ppf "@[(%a)@]" (pretty_vals ",") vs
    | Tpat_construct (_, cstr, []) -> fprintf ppf "%s" cstr.cstr_name
    | Tpat_construct (_, cstr, [w]) ->
      fprintf ppf "@[<2>%s(%a)@]" cstr.cstr_name pretty_arg w
    | Tpat_construct (_, cstr, vs) -> (
      let name = cstr.cstr_name in
      match (name, vs) with
      | "::", [v1; v2] -> fprintf ppf "@[%a::@,%a@]" pretty_car v1 pretty_cdr v2
      | _ -> fprintf ppf "@[<2>%s@ @[(%a)@]@]" name (pretty_vals ",") vs)
    | Tpat_variant (l, None, _) -> fprintf ppf "#%s" l
    | Tpat_variant (l, Some w, _) -> fprintf ppf "@[<2>#%s(%a)@]" l pretty_arg w
    | Tpat_record (lvs, _) -> (
      let filtered_lvs =
        Ext_list.filter lvs (function
          | _, _, {pat_desc = Tpat_any}, _ -> false (* do not show lbl=_ *)
          | _ -> true)
      in
      match filtered_lvs with
      | [] -> fprintf ppf "_"
      | (_, _lbl, _, _) :: _q ->
        let elision_mark _ = () in
        fprintf ppf "@[{%a%t}@]" pretty_lvals filtered_lvs elision_mark)
    | Tpat_array vs -> fprintf ppf "@[[%a]@]" (pretty_vals ",") vs
    | Tpat_alias (v, x, _) ->
      fprintf ppf "@[(%a@ as %a)@]" pretty_val v Ident.print x
    | Tpat_or (v, w, _) -> fprintf ppf "@[%a | @,%a@]" pretty_or v pretty_or w)

and pretty_car ppf v =
  match v.pat_desc with
  | Tpat_construct (_, cstr, [_; _]) when is_cons cstr ->
    fprintf ppf "(%a)" pretty_val v
  | _ -> pretty_val ppf v

and pretty_cdr ppf v =
  match v.pat_desc with
  | Tpat_construct (_, cstr, [v1; v2]) when is_cons cstr ->
    fprintf ppf "%a::@,%a" pretty_car v1 pretty_cdr v2
  | _ -> pretty_val ppf v

and pretty_arg ppf v =
  match v.pat_desc with
  | Tpat_construct (_, _, _ :: _) | Tpat_variant (_, Some _, _) ->
    fprintf ppf "(%a)" pretty_val v
  | _ -> pretty_val ppf v

and pretty_or ppf v =
  match v.pat_desc with
  | Tpat_or (v, w, _) -> fprintf ppf "%a | @,%a" pretty_or v pretty_or w
  | _ -> pretty_val ppf v

and pretty_vals sep ppf = function
  | [] -> ()
  | [v] -> pretty_val ppf v
  | v :: vs -> fprintf ppf "%a%s@ %a" pretty_val v sep (pretty_vals sep) vs

and pretty_lvals ppf = function
  | [] -> ()
  | [(_, lbl, v, _)] -> fprintf ppf "%s: %a" lbl.lbl_name pretty_val v
  | (_, lbl, v, _) :: rest ->
    fprintf ppf "%s: %a,@ %a" lbl.lbl_name pretty_val v pretty_lvals rest

let top_pretty ppf v = fprintf ppf "@[%a@]@?" pretty_val v

let pretty_pat p =
  top_pretty Format.str_formatter p;
  prerr_string (Format.flush_str_formatter ())

type matrix = pattern list list

let pretty_line ps =
  List.iter
    (fun p ->
      top_pretty Format.str_formatter p;
      prerr_string " <";
      prerr_string (Format.flush_str_formatter ());
      prerr_string ">")
    ps

let pretty_matrix (pss : matrix) =
  prerr_endline "begin matrix";
  List.iter
    (fun ps ->
      pretty_line ps;
      prerr_endline "")
    pss;
  prerr_endline "end matrix"

(****************************)
(* Utilities for matching   *)
(****************************)

(* Check top matching *)
let simple_match p1 p2 =
  match (p1.pat_desc, p2.pat_desc) with
  | Tpat_construct (_, c1, _), Tpat_construct (_, c2, _) ->
    Types.equal_tag c1.cstr_tag c2.cstr_tag
  | Tpat_variant (l1, _, _), Tpat_variant (l2, _, _) -> l1 = l2
  | Tpat_constant c1, Tpat_constant c2 -> const_compare c1 c2 = 0
  | Tpat_record _, Tpat_record _ -> true
  | Tpat_tuple p1s, Tpat_tuple p2s | Tpat_array p1s, Tpat_array p2s ->
    List.length p1s = List.length p2s
  | _, (Tpat_any | Tpat_var _) -> true
  | _, _ -> false

(* extract record fields as a whole *)
let record_arg p =
  match p.pat_desc with
  | Tpat_any -> []
  | Tpat_record (args, _) -> args
  | _ -> fatal_error "Parmatch.as_record"

(* Raise Not_found when pos is not present in arg *)
let get_field pos arg =
  let _, _, p, _ = List.find (fun (_, lbl, _, _) -> pos = lbl.lbl_pos) arg in
  p

let extract_fields omegas arg =
  List.map
    (fun (_, lbl, _, _) ->
      try get_field lbl.lbl_pos arg with Not_found -> omega)
    omegas

let all_record_args lbls =
  match lbls with
  | (_, {lbl_all}, _, opt) :: _ ->
    let t =
      Array.map
        (fun lbl -> (mknoloc (Longident.Lident "?temp?"), lbl, omega, opt))
        lbl_all
    in
    List.iter
      (fun ((id, lbl, pat, o) as x) ->
        let lbl_is_optional () =
          match lbl.lbl_repres with
          | Record_inlined _ -> false
          | _ -> lbl.lbl_optional
        in
        let x =
          match pat.pat_desc with
          | Tpat_construct
              ( {txt = Longident.Ldot (Longident.Lident "*predef*", "Some")},
                _,
                [({pat_desc = Tpat_constant _} as c)] )
            when lbl_is_optional () ->
            (id, lbl, c, o)
          | Tpat_construct
              ( {txt = Longident.Ldot (Longident.Lident "*predef*", "Some")},
                _,
                [({pat_desc = Tpat_construct (_, cd, _)} as pat_construct)] )
            when lbl_is_optional () -> (
            let cdecl =
              Ast_untagged_variants
              .constructor_declaration_from_constructor_description
                ~env:pat.pat_env cd
            in
            match cdecl with
            | None -> x
            | Some cstr -> (
              match
                Ast_untagged_variants.get_block_type ~env:pat.pat_env cstr
              with
              | Some block_type
                when not
                       (Ast_untagged_variants.block_type_can_be_undefined
                          block_type) ->
                (id, lbl, pat_construct, o)
              | _ -> x))
          | _ -> x
        in
        t.(lbl.lbl_pos) <- x)
      lbls;
    Array.to_list t
  | _ -> fatal_error "Parmatch.all_record_args"

(* Build argument list when p2 >= p1, where p1 is a simple pattern *)
let rec simple_match_args p1 p2 =
  match p2.pat_desc with
  | Tpat_alias (p2, _, _) -> simple_match_args p1 p2
  | Tpat_construct (_, _, args) -> args
  | Tpat_variant (_, Some arg, _) -> [arg]
  | Tpat_tuple args -> args
  | Tpat_record (args, _) -> extract_fields (record_arg p1) args
  | Tpat_array args -> args
  | Tpat_any | Tpat_var _ -> (
    match p1.pat_desc with
    | Tpat_construct (_, _, args) -> omega_list args
    | Tpat_variant (_, Some _, _) -> [omega]
    | Tpat_tuple args -> omega_list args
    | Tpat_record (args, _) -> omega_list args
    | Tpat_array args -> omega_list args
    | _ -> [])
  | _ -> []

(*
  Normalize a pattern ->
   all arguments are omega (simple pattern) and no more variables
*)

let rec normalize_pat q =
  match q.pat_desc with
  | Tpat_any | Tpat_constant _ -> q
  | Tpat_var _ -> make_pat Tpat_any q.pat_type q.pat_env
  | Tpat_alias (p, _, _) -> normalize_pat p
  | Tpat_tuple args ->
    make_pat (Tpat_tuple (omega_list args)) q.pat_type q.pat_env
  | Tpat_construct (lid, c, args) ->
    make_pat (Tpat_construct (lid, c, omega_list args)) q.pat_type q.pat_env
  | Tpat_variant (l, arg, row) ->
    make_pat
      (Tpat_variant (l, may_map (fun _ -> omega) arg, row))
      q.pat_type q.pat_env
  | Tpat_array args ->
    make_pat (Tpat_array (omega_list args)) q.pat_type q.pat_env
  | Tpat_record (largs, closed) ->
    make_pat
      (Tpat_record
         ( List.map (fun (lid, lbl, _, opt) -> (lid, lbl, omega, opt)) largs,
           closed ))
      q.pat_type q.pat_env
  | Tpat_or _ -> fatal_error "Parmatch.normalize_pat"

(*
  Build normalized (cf. supra) discriminating pattern,
  in the non-data type case
*)

let discr_pat q pss =
  let rec acc_pat acc pss =
    match pss with
    | ({pat_desc = Tpat_alias (p, _, _)} :: ps) :: pss ->
      acc_pat acc ((p :: ps) :: pss)
    | ({pat_desc = Tpat_or (p1, p2, _)} :: ps) :: pss ->
      acc_pat acc ((p1 :: ps) :: (p2 :: ps) :: pss)
    | ({pat_desc = Tpat_any | Tpat_var _} :: _) :: pss -> acc_pat acc pss
    | (({pat_desc = Tpat_tuple _} as p) :: _) :: _ -> normalize_pat p
    | (({pat_desc = Tpat_record (largs, closed)} as p) :: _) :: pss ->
      let new_omegas =
        List.fold_right
          (fun (lid, lbl, _, opt) r ->
            try
              let _ = get_field lbl.lbl_pos r in
              r
            with Not_found -> (lid, lbl, omega, opt) :: r)
          largs (record_arg acc)
      in
      acc_pat
        (make_pat (Tpat_record (new_omegas, closed)) p.pat_type p.pat_env)
        pss
    | _ -> acc
  in

  match normalize_pat q with
  | {pat_desc = Tpat_any | Tpat_record _} as q -> acc_pat q pss
  | q -> q

(*
   In case a matching value is found, set actual arguments
   of the matching pattern.
*)

let rec read_args xs r =
  match (xs, r) with
  | [], _ -> ([], r)
  | _ :: xs, arg :: rest ->
    let args, rest = read_args xs rest in
    (arg :: args, rest)
  | _, _ -> fatal_error "Parmatch.read_args"

let do_set_args erase_mutable q r =
  match q with
  | {pat_desc = Tpat_tuple omegas} ->
    let args, rest = read_args omegas r in
    make_pat (Tpat_tuple args) q.pat_type q.pat_env :: rest
  | {pat_desc = Tpat_record (omegas, closed)} ->
    let args, rest = read_args omegas r in
    make_pat
      (Tpat_record
         ( List.map2
             (fun (lid, lbl, _, opt) arg ->
               if
                 erase_mutable
                 &&
                 match lbl.lbl_mut with
                 | Mutable -> true
                 | Immutable -> false
               then (lid, lbl, omega, opt)
               else (lid, lbl, arg, opt))
             omegas args,
           closed ))
      q.pat_type q.pat_env
    :: rest
  | {pat_desc = Tpat_construct (lid, c, omegas)} ->
    let args, rest = read_args omegas r in
    make_pat (Tpat_construct (lid, c, args)) q.pat_type q.pat_env :: rest
  | {pat_desc = Tpat_variant (l, omega, row)} ->
    let arg, rest =
      match (omega, r) with
      | Some _, a :: r -> (Some a, r)
      | None, r -> (None, r)
      | _ -> assert false
    in
    make_pat (Tpat_variant (l, arg, row)) q.pat_type q.pat_env :: rest
  | {pat_desc = Tpat_array omegas} ->
    let args, rest = read_args omegas r in
    make_pat (Tpat_array args) q.pat_type q.pat_env :: rest
  | {pat_desc = Tpat_constant _ | Tpat_any} ->
    q :: r (* case any is used in matching.ml *)
  | _ -> fatal_error "Parmatch.set_args"

let set_args q r = do_set_args false q r

and set_args_erase_mutable q r = do_set_args true q r

(* filter pss according to pattern q *)
let filter_one q pss =
  let rec filter_rec = function
    | ({pat_desc = Tpat_alias (p, _, _)} :: ps) :: pss ->
      filter_rec ((p :: ps) :: pss)
    | ({pat_desc = Tpat_or (p1, p2, _)} :: ps) :: pss ->
      filter_rec ((p1 :: ps) :: (p2 :: ps) :: pss)
    | (p :: ps) :: pss ->
      if simple_match q p then (simple_match_args q p @ ps) :: filter_rec pss
      else filter_rec pss
    | _ -> []
  in
  filter_rec pss

(*
  Filter pss in the ``extra case''. This applies :
  - According to an extra constructor (datatype case, non-complete signature).
  - According to anything (all-variables case).
*)
let filter_extra pss =
  let rec filter_rec = function
    | ({pat_desc = Tpat_alias (p, _, _)} :: ps) :: pss ->
      filter_rec ((p :: ps) :: pss)
    | ({pat_desc = Tpat_or (p1, p2, _)} :: ps) :: pss ->
      filter_rec ((p1 :: ps) :: (p2 :: ps) :: pss)
    | ({pat_desc = Tpat_any | Tpat_var _} :: qs) :: pss -> qs :: filter_rec pss
    | _ :: pss -> filter_rec pss
    | [] -> []
  in
  filter_rec pss

(*
  Pattern p0 is the discriminating pattern,
  returns [(q0,pss0) ; ... ; (qn,pssn)]
  where the qi's are simple patterns and the pssi's are
  matched matrices.

  NOTES
   * (qi,[]) is impossible.
   * In the case when matching is useless (all-variable case),
     returns []
*)

let filter_all pat0 pss =
  let rec insert q qs env =
    match env with
    | [] ->
      let q0 = normalize_pat q in
      [(q0, [simple_match_args q0 q @ qs])]
    | ((q0, pss) as c) :: env ->
      if simple_match q0 q then
        (q0, (simple_match_args q0 q @ qs) :: pss) :: env
      else c :: insert q qs env
  in

  let rec filter_rec env = function
    | ({pat_desc = Tpat_alias (p, _, _)} :: ps) :: pss ->
      filter_rec env ((p :: ps) :: pss)
    | ({pat_desc = Tpat_or (p1, p2, _)} :: ps) :: pss ->
      filter_rec env ((p1 :: ps) :: (p2 :: ps) :: pss)
    | ({pat_desc = Tpat_any | Tpat_var _} :: _) :: pss -> filter_rec env pss
    | (p :: ps) :: pss -> filter_rec (insert p ps env) pss
    | _ -> env
  and filter_omega env = function
    | ({pat_desc = Tpat_alias (p, _, _)} :: ps) :: pss ->
      filter_omega env ((p :: ps) :: pss)
    | ({pat_desc = Tpat_or (p1, p2, _)} :: ps) :: pss ->
      filter_omega env ((p1 :: ps) :: (p2 :: ps) :: pss)
    | ({pat_desc = Tpat_any | Tpat_var _} :: ps) :: pss ->
      filter_omega
        (List.map
           (fun (q, qss) -> (q, (simple_match_args q omega @ ps) :: qss))
           env)
        pss
    | _ :: pss -> filter_omega env pss
    | [] -> env
  in

  filter_omega
    (filter_rec
       (match pat0.pat_desc with
       | Tpat_record _ | Tpat_tuple _ -> [(pat0, [])]
       | _ -> [])
       pss)
    pss

(* Variant related functions *)

let rec set_last a = function
  | [] -> []
  | [_] -> [a]
  | x :: l -> x :: set_last a l

(* mark constructor lines for failure when they are incomplete *)
let rec mark_partial = function
  | ({pat_desc = Tpat_alias (p, _, _)} :: ps) :: pss ->
    mark_partial ((p :: ps) :: pss)
  | ({pat_desc = Tpat_or (p1, p2, _)} :: ps) :: pss ->
    mark_partial ((p1 :: ps) :: (p2 :: ps) :: pss)
  | ({pat_desc = Tpat_any | Tpat_var _} :: _ as ps) :: pss ->
    ps :: mark_partial pss
  | ps :: pss -> set_last zero ps :: mark_partial pss
  | [] -> []

let close_variant env row =
  let row = Btype.row_repr row in
  let nm =
    List.fold_left
      (fun nm (_tag, f) ->
        match Btype.row_field_repr f with
        | Reither (_, _, false, e) ->
          (* m=false means that this tag is not explicitly matched *)
          Btype.set_row_field e Rabsent;
          None
        | Rabsent | Reither (_, _, true, _) | Rpresent _ -> nm)
      row.row_name row.row_fields
  in
  if (not row.row_closed) || nm != row.row_name then
    (* this unification cannot fail *)
    Ctype.unify env row.row_more
      (Btype.newgenty
         (Tvariant
            {
              row with
              row_fields = [];
              row_more = Btype.newgenvar ();
              row_closed = true;
              row_name = nm;
            }))

let row_of_pat pat =
  match Ctype.expand_head pat.pat_env pat.pat_type with
  | {desc = Tvariant row} -> Btype.row_repr row
  | _ -> assert false

(*
  Check whether the first column of env makes up a complete signature or
  not.
*)

let full_match closing env =
  match env with
  | ({pat_desc = Tpat_construct (_, c, _)}, _) :: _ ->
    if c.cstr_consts < 0 then false (* extensions *)
    else List.length env = c.cstr_consts + c.cstr_nonconsts
  | (({pat_desc = Tpat_variant _} as p), _) :: _ ->
    let fields =
      List.map
        (function
          | {pat_desc = Tpat_variant (tag, _, _)}, _ -> tag
          | _ -> assert false)
        env
    in
    let row = row_of_pat p in
    if closing && not (Btype.row_fixed row) then
      (* closing=true, we are considering the variant as closed *)
      List.for_all
        (fun (tag, f) ->
          match Btype.row_field_repr f with
          | Rabsent | Reither (_, _, false, _) -> true
          | Reither (_, _, true, _)
          (* m=true, do not discard matched tags, rather warn *)
          | Rpresent _ ->
            List.mem tag fields)
        row.row_fields
    else
      row.row_closed
      && List.for_all
           (fun (tag, f) ->
             Btype.row_field_repr f = Rabsent || List.mem tag fields)
           row.row_fields
  | ({pat_desc = Tpat_constant _}, _) :: _ -> false
  | ({pat_desc = Tpat_tuple _}, _) :: _ -> true
  | ({pat_desc = Tpat_record _}, _) :: _ -> true
  | ({pat_desc = Tpat_array _}, _) :: _ -> false
  | ({pat_desc = Tpat_any | Tpat_var _ | Tpat_alias _ | Tpat_or _}, _) :: _ | []
    ->
    assert false

(* Written as a non-fragile matching, PR#7451 originated from a fragile matching below. *)
let should_extend ext env =
  match ext with
  | None -> false
  | Some ext -> (
    match env with
    | [] -> assert false
    | (p, _) :: _ -> (
      match p.pat_desc with
      | Tpat_construct
          (_, {cstr_tag = Cstr_constant _ | Cstr_block _ | Cstr_unboxed}, _) ->
        let path = get_type_path p.pat_type p.pat_env in
        Path.same path ext
      | Tpat_construct (_, {cstr_tag = Cstr_extension _}, _) -> false
      | Tpat_constant _ | Tpat_tuple _ | Tpat_variant _ | Tpat_record _
      | Tpat_array _ ->
        false
      | Tpat_any | Tpat_var _ | Tpat_alias _ | Tpat_or _ -> assert false))

module ConstructorTagHashtbl = Hashtbl.Make (struct
  type t = Types.constructor_tag
  let hash = Hashtbl.hash
  let equal = Types.equal_tag
end)

(* complement constructor tags *)
let complete_tags nconsts nconstrs tags =
  let seen_const = Array.make nconsts false
  and seen_constr = Array.make nconstrs false in
  List.iter
    (function
      | Cstr_constant i -> seen_const.(i) <- true
      | Cstr_block i -> seen_constr.(i) <- true
      | _ -> assert false)
    tags;
  let r = ConstructorTagHashtbl.create (nconsts + nconstrs) in
  for i = 0 to nconsts - 1 do
    if not seen_const.(i) then ConstructorTagHashtbl.add r (Cstr_constant i) ()
  done;
  for i = 0 to nconstrs - 1 do
    if not seen_constr.(i) then ConstructorTagHashtbl.add r (Cstr_block i) ()
  done;
  r

(* build a pattern from a constructor list *)
let pat_of_constr ex_pat cstr =
  {
    ex_pat with
    pat_desc =
      Tpat_construct
        ( mknoloc (Longident.Lident "?pat_of_constr?"),
          cstr,
          omegas cstr.cstr_arity );
  }

let orify x y = make_pat (Tpat_or (x, y, None)) x.pat_type x.pat_env

let rec orify_many = function
  | [] -> assert false
  | [x] -> x
  | x :: xs -> orify x (orify_many xs)

let pat_of_constrs ex_pat cstrs =
  if cstrs = [] then raise Empty
  else orify_many (List.map (pat_of_constr ex_pat) cstrs)

let pats_of_type ?(always = false) env ty =
  let ty' = Ctype.expand_head env ty in
  match ty'.desc with
  | Tconstr (path, _, _) -> (
    try
      match (Env.find_type path env).type_kind with
      | Type_variant cl
        when always
             || List.length cl = 1
             || List.for_all (fun cd -> cd.Types.cd_res <> None) cl ->
        let cstrs = fst (Env.find_type_descrs path env) in
        List.map (pat_of_constr (make_pat Tpat_any ty env)) cstrs
      | Type_record _ ->
        let labels = snd (Env.find_type_descrs path env) in
        let fields =
          List.map
            (fun ld ->
              (mknoloc (Longident.Lident "?pat_of_label?"), ld, omega, false))
            labels
        in
        [make_pat (Tpat_record (fields, Closed)) ty env]
      | _ -> [omega]
    with Not_found -> [omega])
  | Ttuple tl -> [make_pat (Tpat_tuple (omegas (List.length tl))) ty env]
  | _ -> [omega]

let rec get_variant_constructors env ty =
  match (Ctype.repr ty).desc with
  | Tconstr (path, _, _) -> (
    try
      match Env.find_type path env with
      | {type_kind = Type_variant _} -> fst (Env.find_type_descrs path env)
      | {type_manifest = Some _} ->
        get_variant_constructors env
          (Ctype.expand_head_once env (clean_copy ty))
      | _ -> fatal_error "Parmatch.get_variant_constructors"
    with Not_found -> fatal_error "Parmatch.get_variant_constructors")
  | _ -> fatal_error "Parmatch.get_variant_constructors"

(* Sends back a pattern that complements constructor tags all_tag *)
let complete_constrs p all_tags =
  let c =
    match p.pat_desc with
    | Tpat_construct (_, c, _) -> c
    | _ -> assert false
  in
  let not_tags = complete_tags c.cstr_consts c.cstr_nonconsts all_tags in
  let constrs = get_variant_constructors p.pat_env c.cstr_res in
  let others =
    Ext_list.filter constrs (fun cnstr ->
        ConstructorTagHashtbl.mem not_tags cnstr.cstr_tag)
  in
  let const, nonconst =
    List.partition (fun cnstr -> cnstr.cstr_arity = 0) others
  in
  const @ nonconst

let build_other_constrs env p =
  match p.pat_desc with
  | Tpat_construct (_, {cstr_tag = Cstr_constant _ | Cstr_block _}, _) ->
    let get_tag = function
      | {pat_desc = Tpat_construct (_, c, _)} -> c.cstr_tag
      | _ -> fatal_error "Parmatch.get_tag"
    in
    let all_tags = List.map (fun (p, _) -> get_tag p) env in
    pat_of_constrs p (complete_constrs p all_tags)
  | _ -> extra_pat

(* Auxiliary for build_other *)

let build_other_constant proj make first next p env =
  let all = List.map (fun (p, _) -> proj p.pat_desc) env in
  let rec try_const i =
    if List.mem i all then try_const (next i)
    else make_pat (make i) p.pat_type p.pat_env
  in
  try_const first

(*
  Builds a pattern that is incompatible with all patterns in
  in the first column of env
*)

let some_other_tag = "<some other tag>"

let build_other ext env : Typedtree.pattern =
  match env with
  | ({pat_desc = Tpat_construct (lid, {cstr_tag = Cstr_extension _}, _)}, _)
    :: _ ->
    (* let c = {c with cstr_name = "*extension*"} in *)
    (* PR#7330 *)
    make_pat
      (Tpat_var (Ident.create "*extension*", {lid with txt = "*extension*"}))
      Ctype.none Env.empty
  | (({pat_desc = Tpat_construct _} as p), _) :: _ -> (
    match ext with
    | Some ext when Path.same ext (get_type_path p.pat_type p.pat_env) ->
      extra_pat
    | _ -> build_other_constrs env p)
  | (({pat_desc = Tpat_variant (_, _, r)} as p), _) :: _ -> (
    let tags =
      List.map
        (function
          | {pat_desc = Tpat_variant (tag, _, _)}, _ -> tag
          | _ -> assert false)
        env
    in
    let row = row_of_pat p in
    let make_other_pat tag const =
      let arg = if const then None else Some omega in
      make_pat (Tpat_variant (tag, arg, r)) p.pat_type p.pat_env
    in
    match
      List.fold_left
        (fun others (tag, f) ->
          if List.mem tag tags then others
          else
            match Btype.row_field_repr f with
            | Rabsent (* | Reither _ *) -> others
            (* This one is called after erasing pattern info *)
            | Reither (c, _, _, _) -> make_other_pat tag c :: others
            | Rpresent arg -> make_other_pat tag (arg = None) :: others)
        [] row.row_fields
    with
    | [] -> make_other_pat some_other_tag true
    | pat :: other_pats ->
      List.fold_left
        (fun p_res pat ->
          make_pat (Tpat_or (pat, p_res, None)) p.pat_type p.pat_env)
        pat other_pats)
  | (({pat_desc = Tpat_constant (Const_int _)} as p), _) :: _ ->
    build_other_constant
      (function
        | Tpat_constant (Const_int i) -> i
        | _ -> assert false)
      (function
        | i -> Tpat_constant (Const_int i))
      0 succ p env
  | (({pat_desc = Tpat_constant (Const_char _)} as p), _) :: _ ->
    build_other_constant
      (function
        | Tpat_constant (Const_char i) -> i
        | _ -> assert false)
      (function
        | i -> Tpat_constant (Const_char i))
      0 succ p env
  | (({pat_desc = Tpat_constant (Const_int32 _)} as p), _) :: _ ->
    build_other_constant
      (function
        | Tpat_constant (Const_int32 i) -> i
        | _ -> assert false)
      (function
        | i -> Tpat_constant (Const_int32 i))
      0l Int32.succ p env
  | (({pat_desc = Tpat_constant (Const_int64 _)} as p), _) :: _ ->
    build_other_constant
      (function
        | Tpat_constant (Const_int64 i) -> i
        | _ -> assert false)
      (function
        | i -> Tpat_constant (Const_int64 i))
      0L Int64.succ p env
  | (({pat_desc = Tpat_constant (Const_bigint _)} as p), _) :: _ ->
    build_other_constant
      (function
        | Tpat_constant (Const_bigint (sign, i)) ->
          String.length (Bigint_utils.to_string sign i)
        | _ -> assert false)
      (function
        | i -> Tpat_constant (Const_bigint (true, string_of_int i)))
      0 succ p env
  | (({pat_desc = Tpat_constant (Const_string _)} as p), _) :: _ ->
    build_other_constant
      (function
        | Tpat_constant (Const_string (s, _)) -> String.length s
        | _ -> assert false)
      (function
        | i -> Tpat_constant (Const_string (String.make i '*', None)))
      0 succ p env
  | (({pat_desc = Tpat_constant (Const_float _)} as p), _) :: _ ->
    build_other_constant
      (function
        | Tpat_constant (Const_float f) -> float_of_string f
        | _ -> assert false)
      (function
        | f -> Tpat_constant (Const_float (string_of_float f)))
      0.0
      (fun f -> f +. 1.0)
      p env
  | (({pat_desc = Tpat_array _} as p), _) :: _ ->
    let all_lengths =
      List.map
        (fun (p, _) ->
          match p.pat_desc with
          | Tpat_array args -> List.length args
          | _ -> assert false)
        env
    in
    let rec try_arrays l =
      if List.mem l all_lengths then try_arrays (l + 1)
      else make_pat (Tpat_array (omegas l)) p.pat_type p.pat_env
    in
    try_arrays 0
  | [] -> omega
  | _ -> omega

(*
  Core function :
  Is the last row of pattern matrix pss + qs satisfiable ?
  That is :
    Does there exists at least one value vector, es such that :
     1- for all ps in pss ps # es (ps and es are not compatible)
     2- qs <= es                  (es matches qs)
*)

let rec has_instance p =
  match p.pat_desc with
  | Tpat_variant (l, _, r) when is_absent l r -> false
  | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_, None, _) -> true
  | Tpat_alias (p, _, _) | Tpat_variant (_, Some p, _) -> has_instance p
  | Tpat_or (p1, p2, _) -> has_instance p1 || has_instance p2
  | Tpat_construct (_, _, ps) | Tpat_tuple ps | Tpat_array ps ->
    has_instances ps
  | Tpat_record (lps, _) -> has_instances (List.map (fun (_, _, x, _) -> x) lps)

and has_instances = function
  | [] -> true
  | q :: rem -> has_instance q && has_instances rem

(*
   In two places in the following function, we check the coherence of the first
   column of (pss + qs).
   If it is incoherent, then we exit early saying that (pss + qs) is not
   satisfiable (which is equivalent to saying "oh, we shouldn't have considered
   that branch, no good result came come from here").

   But what happens if we have a coherent but ill-typed column?
   - we might end up returning [false], which is equivalent to noticing the
   incompatibility: clearly this is fine.
   - if we end up returning [true] then we're saying that [qs] is useful while
   it is not. This is sad but not the end of the world, we're just allowing dead
   code to survive.
*)
let rec satisfiable pss qs =
  match pss with
  | [] -> has_instances qs
  | _ -> (
    match qs with
    | [] -> false
    | {pat_desc = Tpat_or (q1, q2, _)} :: qs ->
      satisfiable pss (q1 :: qs) || satisfiable pss (q2 :: qs)
    | {pat_desc = Tpat_alias (q, _, _)} :: qs -> satisfiable pss (q :: qs)
    | {pat_desc = Tpat_any | Tpat_var _} :: qs -> (
      if not (all_coherent (simplified_first_col pss)) then false
      else
        let q0 = discr_pat omega pss in
        match filter_all q0 pss with
        (* first column of pss is made of variables only *)
        | [] -> satisfiable (filter_extra pss) qs
        | constrs ->
          if full_match false constrs then
            List.exists
              (fun (p, pss) ->
                (not (is_absent_pat p))
                && satisfiable pss (simple_match_args p omega @ qs))
              constrs
          else satisfiable (filter_extra pss) qs)
    | {pat_desc = Tpat_variant (l, _, r)} :: _ when is_absent l r -> false
    | q :: qs ->
      if not (all_coherent (q :: simplified_first_col pss)) then false
      else
        let q0 = discr_pat q pss in
        satisfiable (filter_one q0 pss) (simple_match_args q0 q @ qs))

(* Also return the remaining cases, to enable GADT handling

   For considerations regarding the coherence check, see the comment on
   [satisfiable] above. *)
let rec satisfiables pss qs =
  match pss with
  | [] -> if has_instances qs then [qs] else []
  | _ -> (
    match qs with
    | [] -> []
    | {pat_desc = Tpat_or (q1, q2, _)} :: qs ->
      satisfiables pss (q1 :: qs) @ satisfiables pss (q2 :: qs)
    | {pat_desc = Tpat_alias (q, _, _)} :: qs -> satisfiables pss (q :: qs)
    | {pat_desc = Tpat_any | Tpat_var _} :: qs -> (
      if not (all_coherent (simplified_first_col pss)) then []
      else
        let q0 = discr_pat omega pss in
        let wild p =
          List.map (fun qs -> p :: qs) (satisfiables (filter_extra pss) qs)
        in
        match filter_all q0 pss with
        (* first column of pss is made of variables only *)
        | [] -> wild omega
        | (p, _) :: _ as constrs -> (
          let for_constrs () =
            List.flatten
              (List.map
                 (fun (p, pss) ->
                   if is_absent_pat p then []
                   else
                     List.map (set_args p)
                       (satisfiables pss (simple_match_args p omega @ qs)))
                 constrs)
          in
          if full_match false constrs then for_constrs ()
          else
            match p.pat_desc with
            | Tpat_construct _ ->
              (* activate this code for checking non-gadt constructors *)
              wild (build_other_constrs constrs p) @ for_constrs ()
            | _ -> wild omega))
    | {pat_desc = Tpat_variant (l, _, r)} :: _ when is_absent l r -> []
    | q :: qs ->
      if not (all_coherent (q :: simplified_first_col pss)) then []
      else
        let q0 = discr_pat q pss in
        List.map (set_args q0)
          (satisfiables (filter_one q0 pss) (simple_match_args q0 q @ qs)))

(*
  Now another satisfiable function that additionally
  supplies an example of a matching value.

  This function should be called for exhaustiveness check only.
*)

type 'a result =
  | Rnone (* No matching value *)
  | Rsome of 'a (* This matching value *)

(*
let rec try_many  f = function
  | [] -> Rnone
  | (p,pss)::rest ->
      match f (p,pss) with
      | Rnone -> try_many  f rest
      | r -> r
*)

let rappend r1 r2 =
  match (r1, r2) with
  | Rnone, _ -> r2
  | _, Rnone -> r1
  | Rsome l1, Rsome l2 -> Rsome (l1 @ l2)

let rec try_many_gadt f = function
  | [] -> Rnone
  | (p, pss) :: rest -> rappend (f (p, pss)) (try_many_gadt f rest)

(*
let rec exhaust ext pss n = match pss with
| []    ->  Rsome (omegas n)
| []::_ ->  Rnone
| pss   ->
    let q0 = discr_pat omega pss in
    begin match filter_all q0 pss with
          (* first column of pss is made of variables only *)
    | [] ->
        begin match exhaust ext (filter_extra pss) (n-1) with
        | Rsome r -> Rsome (q0::r)
        | r -> r
      end
    | constrs ->
        let try_non_omega (p,pss) =
          if is_absent_pat p then
            Rnone
          else
            match
              exhaust
                ext pss (List.length (simple_match_args p omega) + n - 1)
            with
            | Rsome r -> Rsome (set_args p r)
            | r       -> r in
        if
          full_match true false constrs && not (should_extend ext constrs)
        then
          try_many try_non_omega constrs
        else
          (*
             D = filter_extra pss is the default matrix
             as it is included in pss, one can avoid
             recursive calls on specialized matrices,
             Essentially :
             * D exhaustive => pss exhaustive
             * D non-exhaustive => we have a non-filtered value
          *)
          let r =  exhaust ext (filter_extra pss) (n-1) in
          match r with
          | Rnone -> Rnone
          | Rsome r ->
              try
                Rsome (build_other ext constrs::r)
              with
      (* cannot occur, since constructors don't make a full signature *)
              | Empty -> fatal_error "Parmatch.exhaust"
    end

let combinations f lst lst' =
  let rec iter2 x =
    function
        [] -> []
      | y :: ys ->
          f x y :: iter2 x ys
  in
  let rec iter =
    function
        [] -> []
      | x :: xs -> iter2 x lst' @ iter xs
  in
  iter lst
*)
(*
let print_pat pat =
  let rec string_of_pat pat =
    match pat.pat_desc with
        Tpat_var _ -> "v"
      | Tpat_any -> "_"
      | Tpat_alias (p, x) -> Printf.sprintf "(%s) as ?"  (string_of_pat p)
      | Tpat_constant n -> "0"
      | Tpat_construct (_, lid, _) ->
        Printf.sprintf "%s" (String.concat "." (Longident.flatten lid.txt))
      | Tpat_lazy p ->
        Printf.sprintf "(lazy %s)" (string_of_pat p)
      | Tpat_or (p1,p2,_) ->
        Printf.sprintf "(%s | %s)" (string_of_pat p1) (string_of_pat p2)
      | Tpat_tuple list ->
        Printf.sprintf "(%s)" (String.concat "," (List.map string_of_pat list))
      | Tpat_variant (_, _, _) -> "variant"
      | Tpat_record (_, _) -> "record"
      | Tpat_array _ -> "array"
  in
  Printf.fprintf stderr "PAT[%s]\n%!" (string_of_pat pat)
*)

(* strictly more powerful than exhaust; however, exhaust
   was kept for backwards compatibility *)

(* Limit to prevent exponential blowup in exhaustiveness checking.
   When the limit is exceeded, we conservatively report the pattern as
   non-exhaustive (Rnone) which may cause false warnings but prevents hangs. *)
let exhaust_gadt_limit = 1000

let rec exhaust_gadt_aux count (ext : Path.t option) pss n =
  (* Bail out if we've done too many recursive calls to prevent exponential blowup *)
  if !count > exhaust_gadt_limit then Rnone
  else (
    incr count;
    match pss with
    | [] -> Rsome [omegas n]
    | [] :: _ -> Rnone
    | pss -> (
      if not (all_coherent (simplified_first_col pss)) then
        (* We're considering an ill-typed branch, we won't actually be able to
         produce a well typed value taking that branch. *)
        Rnone
      else
        (* Assuming the first column is ill-typed but considered coherent, we
         might end up producing an ill-typed witness of non-exhaustivity
         corresponding to the current branch.

         If [exhaust] has been called by [do_check_partial], then the witnesses
         produced get typechecked and the ill-typed ones are discarded.

         If [exhaust] has been called by [do_check_fragile], then it is possible
         we might fail to warn the user that the matching is fragile. See for
         example testsuite/tests/warnings/w04_failure.ml. *)
        let q0 = discr_pat omega pss in
        match filter_all q0 pss with
        (* first column of pss is made of variables only *)
        | [] -> (
          match exhaust_gadt_aux count ext (filter_extra pss) (n - 1) with
          | Rsome r -> Rsome (List.map (fun row -> q0 :: row) r)
          | r -> r)
        | constrs -> (
          let try_non_omega (p, pss) =
            if is_absent_pat p then Rnone
            else
              match
                exhaust_gadt_aux count ext pss
                  (List.length (simple_match_args p omega) + n - 1)
              with
              | Rsome r -> Rsome (List.map (fun row -> set_args p row) r)
              | r -> r
          in
          let before = try_many_gadt try_non_omega constrs in
          if full_match false constrs && not (should_extend ext constrs) then
            before
          else
            (*
              D = filter_extra pss is the default matrix
              as it is included in pss, one can avoid
              recursive calls on specialized matrices,
              Essentially :
            * D exhaustive => pss exhaustive
            * D non-exhaustive => we have a non-filtered value
            *)
            let r = exhaust_gadt_aux count ext (filter_extra pss) (n - 1) in
            match r with
            | Rnone -> before
            | Rsome r -> (
              try
                let p = build_other ext constrs in
                let dug = List.map (fun tail -> p :: tail) r in
                match before with
                | Rnone -> Rsome dug
                | Rsome x -> Rsome (x @ dug)
              with
              (* cannot occur, since constructors don't make a full signature *)
              | Empty ->
                fatal_error "Parmatch.exhaust"))))

let exhaust_gadt ext pss n =
  let count = ref 0 in
  let ret = exhaust_gadt_aux count ext pss n in
  match ret with
  | Rnone -> Rnone
  | Rsome lst ->
    (* The following line is needed to compile stdlib/printf.ml *)
    if lst = [] then Rsome (omegas n)
    else
      let singletons =
        List.map
          (function
            | [x] -> x
            | _ -> assert false)
          lst
      in
      Rsome [orify_many singletons]

(*
   Another exhaustiveness check, enforcing variant typing.
   Note that it does not check exact exhaustiveness, but whether a
   matching could be made exhaustive by closing all variant types.
   When this is true of all other columns, the current column is left
   open (even if it means that the whole matching is not exhaustive as
   a result).
   When this is false for the matrix minus the current column, and the
   current column is composed of variant tags, we close the variant
   (even if it doesn't help in making the matching exhaustive).
*)

let rec pressure_variants tdefs = function
  | [] -> false
  | [] :: _ -> true
  | pss -> (
    if not (all_coherent (simplified_first_col pss)) then true
    else
      let q0 = discr_pat omega pss in
      match filter_all q0 pss with
      | [] -> pressure_variants tdefs (filter_extra pss)
      | constrs ->
        let rec try_non_omega = function
          | (_p, pss) :: rem ->
            let ok = pressure_variants tdefs pss in
            try_non_omega rem && ok
          | [] -> true
        in
        if full_match (tdefs = None) constrs then try_non_omega constrs
        else if tdefs = None then pressure_variants None (filter_extra pss)
        else
          let full = full_match true constrs in
          let ok =
            if full then try_non_omega constrs
            else try_non_omega (filter_all q0 (mark_partial pss))
          in
          (match (constrs, tdefs) with
          | (({pat_desc = Tpat_variant _} as p), _) :: _, Some env ->
            let row = row_of_pat p in
            if Btype.row_fixed row || pressure_variants None (filter_extra pss)
            then ()
            else close_variant env row
          | _ -> ());
          ok)

(* Yet another satisfiable function *)

(*
   This time every_satisfiable pss qs checks the
   utility of every expansion of qs.
   Expansion means expansion of or-patterns inside qs
*)

type answer =
  | Used (* Useful pattern *)
  | Unused (* Useless pattern *)
  | Upartial of Typedtree.pattern list (* Mixed, with list of useless ones *)

(* this row type enable column processing inside the matrix
    - left  ->  elements not to be processed,
    - right ->  elements to be processed
*)
type 'a row = {no_ors: 'a list; ors: 'a list; active: 'a list}

(*
let pretty_row {ors=ors ; no_ors=no_ors; active=active} =
  pretty_line ors ; prerr_string " *" ;
  pretty_line no_ors ; prerr_string " *" ;
  pretty_line active

let pretty_rows rs =
  prerr_endline "begin matrix" ;
  List.iter
    (fun r ->
      pretty_row r ;
      prerr_endline "")
    rs ;
  prerr_endline "end matrix"
*)

(* Initial build *)
let make_row ps = {ors = []; no_ors = []; active = ps}

let make_rows pss = List.map make_row pss

(* Useful to detect and expand  or pats inside as pats *)
let rec unalias p =
  match p.pat_desc with
  | Tpat_alias (p, _, _) -> unalias p
  | _ -> p

let is_var p =
  match (unalias p).pat_desc with
  | Tpat_any | Tpat_var _ -> true
  | _ -> false

let is_var_column rs =
  List.for_all
    (fun r ->
      match r.active with
      | p :: _ -> is_var p
      | [] -> assert false)
    rs

(* Standard or-args for left-to-right matching *)
let rec or_args p =
  match p.pat_desc with
  | Tpat_or (p1, p2, _) -> (p1, p2)
  | Tpat_alias (p, _, _) -> or_args p
  | _ -> assert false

(* Just remove current column *)
let remove r =
  match r.active with
  | _ :: rem -> {r with active = rem}
  | [] -> assert false

let remove_column rs = List.map remove rs

(* Current column has been processed *)
let push_no_or r =
  match r.active with
  | p :: rem -> {r with no_ors = p :: r.no_ors; active = rem}
  | [] -> assert false

let push_or r =
  match r.active with
  | p :: rem -> {r with ors = p :: r.ors; active = rem}
  | [] -> assert false

let push_or_column rs = List.map push_or rs

and push_no_or_column rs = List.map push_no_or rs

(* Those are adaptations of the previous homonymous functions that
   work on the current column, instead of the first column
*)

let discr_pat q rs = discr_pat q (List.map (fun r -> r.active) rs)

let filter_one q rs =
  let rec filter_rec rs =
    match rs with
    | [] -> []
    | r :: rem -> (
      match r.active with
      | [] -> assert false
      | {pat_desc = Tpat_alias (p, _, _)} :: ps ->
        filter_rec ({r with active = p :: ps} :: rem)
      | {pat_desc = Tpat_or (p1, p2, _)} :: ps ->
        filter_rec
          ({r with active = p1 :: ps} :: {r with active = p2 :: ps} :: rem)
      | p :: ps ->
        if simple_match q p then
          {r with active = simple_match_args q p @ ps} :: filter_rec rem
        else filter_rec rem)
  in
  filter_rec rs

(* Back to normal matrices *)
let make_vector r = List.rev r.no_ors

let make_matrix rs = List.map make_vector rs

(* Standard union on answers *)
let union_res r1 r2 =
  match (r1, r2) with
  | Unused, _ | _, Unused -> Unused
  | Used, _ -> r2
  | _, Used -> r1
  | Upartial u1, Upartial u2 -> Upartial (u1 @ u2)

(* propose or pats for expansion *)
let extract_elements qs =
  let rec do_rec seen = function
    | [] -> []
    | q :: rem ->
      {no_ors = List.rev_append seen rem @ qs.no_ors; ors = []; active = [q]}
      :: do_rec (q :: seen) rem
  in
  do_rec [] qs.ors

(* idem for matrices *)
let transpose rs =
  match rs with
  | [] -> assert false
  | r :: rem ->
    let i = List.map (fun x -> [x]) r in
    List.fold_left (List.map2 (fun r x -> x :: r)) i rem

let extract_columns pss qs =
  match pss with
  | [] -> List.map (fun _ -> []) qs.ors
  | _ ->
    let rows = List.map extract_elements pss in
    transpose rows

(* Core function
   The idea is to first look for or patterns (recursive case), then
   check or-patterns argument usefulness (terminal case)
*)
let rec simplified_first_usefulness_col = function
  | [] -> []
  | row :: rows -> (
    match row.active with
    | [] -> assert false (* the rows are non-empty! *)
    | p :: _ -> simplify_head_pat p (simplified_first_usefulness_col rows))

let rec every_satisfiables pss qs =
  match qs.active with
  | [] -> (
    (* qs is now partitionned,  check usefulness *)
    match qs.ors with
    | [] ->
      (* no or-patterns *)
      if satisfiable (make_matrix pss) (make_vector qs) then Used else Unused
    | _ ->
      (* n or-patterns -> 2n expansions *)
      List.fold_right2
        (fun pss qs r ->
          match r with
          | Unused -> Unused
          | _ -> (
            match qs.active with
            | [q] ->
              let q1, q2 = or_args q in
              let r_loc = every_both pss qs q1 q2 in
              union_res r r_loc
            | _ -> assert false))
        (extract_columns pss qs) (extract_elements qs) Used)
  | q :: rem -> (
    let uq = unalias q in
    match uq.pat_desc with
    | Tpat_any | Tpat_var _ ->
      if is_var_column pss then
        (* forget about ``all-variable''  columns now *)
        every_satisfiables (remove_column pss) (remove qs)
      else
        (* otherwise this is direct food for satisfiable *)
        every_satisfiables (push_no_or_column pss) (push_no_or qs)
    | Tpat_or (q1, q2, _) ->
      if q1.pat_loc.Location.loc_ghost && q2.pat_loc.Location.loc_ghost then
        (* syntactically generated or-pats should not be expanded *)
        every_satisfiables (push_no_or_column pss) (push_no_or qs)
      else
        (* this is a real or-pattern *)
        every_satisfiables (push_or_column pss) (push_or qs)
    | Tpat_variant (l, _, r) when is_absent l r ->
      (* Ah Jacques... *)
      Unused
    | _ ->
      (* standard case, filter matrix *)
      (* The handling of incoherent matrices is kept in line with
                [satisfiable] *)
      if not (all_coherent (uq :: simplified_first_usefulness_col pss)) then
        Unused
      else
        let q0 = discr_pat q pss in
        every_satisfiables (filter_one q0 pss)
          {qs with active = simple_match_args q0 q @ rem})

(*
  This function ``every_both'' performs the usefulness check
  of or-pat q1|q2.
  The trick is to call every_satisfied twice with
  current active columns restricted to q1 and q2,
  That way,
  - others orpats in qs.ors will not get expanded.
  - all matching work performed on qs.no_ors is not performed again.
  *)
and every_both pss qs q1 q2 =
  let qs1 = {qs with active = [q1]} and qs2 = {qs with active = [q2]} in
  let r1 = every_satisfiables pss qs1
  and r2 = every_satisfiables (if compat q1 q2 then qs1 :: pss else pss) qs2 in
  match r1 with
  | Unused -> (
    match r2 with
    | Unused -> Unused
    | Used -> Upartial [q1]
    | Upartial u2 -> Upartial (q1 :: u2))
  | Used -> (
    match r2 with
    | Unused -> Upartial [q2]
    | _ -> r2)
  | Upartial u1 -> (
    match r2 with
    | Unused -> Upartial (u1 @ [q2])
    | Used -> r1
    | Upartial u2 -> Upartial (u1 @ u2))

(* le_pat p q  means, forall V,  V matches q implies V matches p *)
let rec le_pat p q =
  match (p.pat_desc, q.pat_desc) with
  | (Tpat_var _ | Tpat_any), _ -> true
  | Tpat_alias (p, _, _), _ -> le_pat p q
  | _, Tpat_alias (q, _, _) -> le_pat p q
  | Tpat_constant c1, Tpat_constant c2 -> const_compare c1 c2 = 0
  | Tpat_construct (_, c1, ps), Tpat_construct (_, c2, qs) ->
    Types.equal_tag c1.cstr_tag c2.cstr_tag && le_pats ps qs
  | Tpat_variant (l1, Some p1, _), Tpat_variant (l2, Some p2, _) ->
    l1 = l2 && le_pat p1 p2
  | Tpat_variant (l1, None, _r1), Tpat_variant (l2, None, _) -> l1 = l2
  | Tpat_variant (_, _, _), Tpat_variant (_, _, _) -> false
  | Tpat_tuple ps, Tpat_tuple qs -> le_pats ps qs
  | Tpat_record (l1, _), Tpat_record (l2, _) ->
    let ps, qs = records_args l1 l2 in
    le_pats ps qs
  | Tpat_array ps, Tpat_array qs -> Ext_list.same_length ps qs && le_pats ps qs
  (* In all other cases, enumeration is performed *)
  | _, _ -> not (satisfiable [[p]] [q])

and le_pats ps qs =
  match (ps, qs) with
  | p :: ps, q :: qs -> le_pat p q && le_pats ps qs
  | _, _ -> true

let get_mins le ps =
  let rec select_rec r = function
    | [] -> r
    | p :: ps ->
      if List.exists (fun p0 -> le p0 p) ps then select_rec r ps
      else select_rec (p :: r) ps
  in
  select_rec [] (select_rec [] ps)

(*
  lub p q is a pattern that matches all values matched by p and q
  may raise Empty, when p and q are not compatible
*)

let rec lub p q =
  match (p.pat_desc, q.pat_desc) with
  | Tpat_alias (p, _, _), _ -> lub p q
  | _, Tpat_alias (q, _, _) -> lub p q
  | (Tpat_any | Tpat_var _), _ -> q
  | _, (Tpat_any | Tpat_var _) -> p
  | Tpat_or (p1, p2, _), _ -> orlub p1 p2 q
  | _, Tpat_or (q1, q2, _) -> orlub q1 q2 p (* Thanks god, lub is commutative *)
  | Tpat_constant c1, Tpat_constant c2 when const_compare c1 c2 = 0 -> p
  | Tpat_tuple ps, Tpat_tuple qs ->
    let rs = lubs ps qs in
    make_pat (Tpat_tuple rs) p.pat_type p.pat_env
  | Tpat_construct (lid, c1, ps1), Tpat_construct (_, c2, ps2)
    when Types.equal_tag c1.cstr_tag c2.cstr_tag ->
    let rs = lubs ps1 ps2 in
    make_pat (Tpat_construct (lid, c1, rs)) p.pat_type p.pat_env
  | Tpat_variant (l1, Some p1, row), Tpat_variant (l2, Some p2, _) when l1 = l2
    ->
    let r = lub p1 p2 in
    make_pat (Tpat_variant (l1, Some r, row)) p.pat_type p.pat_env
  | Tpat_variant (l1, None, _row), Tpat_variant (l2, None, _) when l1 = l2 -> p
  | Tpat_record (l1, closed), Tpat_record (l2, _) ->
    let rs = record_lubs l1 l2 in
    make_pat (Tpat_record (rs, closed)) p.pat_type p.pat_env
  | Tpat_array ps, Tpat_array qs when List.length ps = List.length qs ->
    let rs = lubs ps qs in
    make_pat (Tpat_array rs) p.pat_type p.pat_env
  | _, _ -> raise Empty

and orlub p1 p2 q =
  try
    let r1 = lub p1 q in
    try {q with pat_desc = Tpat_or (r1, lub p2 q, None)} with Empty -> r1
  with Empty -> lub p2 q

and record_lubs l1 l2 =
  let rec lub_rec l1 l2 =
    match (l1, l2) with
    | [], _ -> l2
    | _, [] -> l1
    | (lid1, lbl1, p1, o1) :: rem1, (lid2, lbl2, p2, o2) :: rem2 ->
      if lbl1.lbl_pos < lbl2.lbl_pos then
        (lid1, lbl1, p1, o1) :: lub_rec rem1 l2
      else if lbl2.lbl_pos < lbl1.lbl_pos then
        (lid2, lbl2, p2, o2) :: lub_rec l1 rem2
      else
        let o = if o1 = o2 then o1 else raise Empty in
        (lid1, lbl1, lub p1 p2, o) :: lub_rec rem1 rem2
  in
  lub_rec l1 l2

and lubs ps qs =
  match (ps, qs) with
  | p :: ps, q :: qs -> lub p q :: lubs ps qs
  | _, _ -> []

(******************************)
(* Exported variant closing   *)
(******************************)

(* Apply pressure to variants *)

let pressure_variants tdefs patl =
  let pss = List.map (fun p -> [p; omega]) patl in
  ignore (pressure_variants (Some tdefs) pss)

(*****************************)
(* Utilities for diagnostics *)
(*****************************)

(*
  Build up a working pattern matrix by forgetting
  about guarded patterns
*)

let rec initial_matrix = function
  | [] -> []
  | {c_guard = Some _} :: rem -> initial_matrix rem
  | {c_guard = None; c_lhs = p} :: rem -> [p] :: initial_matrix rem

(******************************************)
(* Look for a row that matches some value *)
(******************************************)

(*
  Useful for seeing if the example of
  non-matched value can indeed be matched
  (by a guarded clause)
*)

exception NoGuard

let rec initial_all no_guard = function
  | [] -> if no_guard then raise NoGuard else []
  | {c_lhs = pat; c_guard; _} :: rem ->
    ([pat], pat.pat_loc) :: initial_all (no_guard && c_guard = None) rem

let rec do_filter_var = function
  | (_ :: ps, loc) :: rem -> (ps, loc) :: do_filter_var rem
  | _ -> []

let do_filter_one q pss =
  let rec filter_rec = function
    | ({pat_desc = Tpat_alias (p, _, _)} :: ps, loc) :: pss ->
      filter_rec ((p :: ps, loc) :: pss)
    | ({pat_desc = Tpat_or (p1, p2, _)} :: ps, loc) :: pss ->
      filter_rec ((p1 :: ps, loc) :: (p2 :: ps, loc) :: pss)
    | (p :: ps, loc) :: pss ->
      if simple_match q p then
        (simple_match_args q p @ ps, loc) :: filter_rec pss
      else filter_rec pss
    | _ -> []
  in
  filter_rec pss

let rec do_match pss qs =
  match qs with
  | [] -> (
    match pss with
    | ([], loc) :: _ -> Some loc
    | _ -> None)
  | q :: qs -> (
    match q with
    | {pat_desc = Tpat_or (q1, q2, _)} -> (
      match do_match pss (q1 :: qs) with
      | None -> do_match pss (q2 :: qs)
      | r -> r)
    | {pat_desc = Tpat_any} -> do_match (do_filter_var pss) qs
    | _ ->
      let q0 = normalize_pat q in
      (* [pss] will (or won't) match [q0 :: qs] regardless of the coherence of
         its first column. *)
      do_match (do_filter_one q0 pss) (simple_match_args q0 q @ qs))

let check_partial_all v casel =
  try
    let pss = initial_all true casel in
    do_match pss [v]
  with NoGuard -> None

(************************)
(* Exhaustiveness check *)
(************************)

(* conversion from Typedtree.pattern to Parsetree.pattern list *)
module Conv = struct
  open Parsetree
  let mkpat desc = Ast_helper.Pat.mk desc

  let name_counter = ref 0
  let fresh name =
    let current = !name_counter in
    name_counter := !name_counter + 1;
    "#$" ^ name ^ string_of_int current

  let conv typed =
    let constrs = Hashtbl.create 7 in
    let labels = Hashtbl.create 7 in
    let rec loop pat =
      match pat.pat_desc with
      | Tpat_or (pa, pb, _) -> mkpat (Ppat_or (loop pa, loop pb))
      | Tpat_var (_, ({txt = "*extension*"} as nm)) ->
        (* PR#7330 *)
        mkpat (Ppat_var nm)
      | Tpat_any | Tpat_var _ -> mkpat Ppat_any
      | Tpat_constant c -> mkpat (Ppat_constant (Untypeast.constant c))
      | Tpat_alias (p, _, _) -> loop p
      | Tpat_tuple lst -> mkpat (Ppat_tuple (List.map loop lst))
      | Tpat_construct (cstr_lid, cstr, lst) ->
        let id = fresh cstr.cstr_name in
        let lid = {cstr_lid with txt = Longident.Lident id} in
        Hashtbl.add constrs id cstr;
        let arg =
          match List.map loop lst with
          | [] -> None
          | [p] -> Some p
          | lst -> Some (mkpat (Ppat_tuple lst))
        in
        mkpat (Ppat_construct (lid, arg))
      | Tpat_variant (label, p_opt, _row_desc) ->
        let arg = Misc.may_map loop p_opt in
        mkpat (Ppat_variant (label, arg))
      | Tpat_record (subpatterns, _closed_flag) ->
        let fields =
          List.map
            (fun (_, lbl, p, optional) ->
              let id = fresh lbl.lbl_name in
              Hashtbl.add labels id lbl;
              {lid = mknoloc (Longident.Lident id); x = loop p; opt = optional})
            subpatterns
        in
        mkpat (Ppat_record (fields, Open))
      | Tpat_array lst -> mkpat (Ppat_array (List.map loop lst))
    in
    let ps = loop typed in
    (ps, constrs, labels)
end

(* Whether the counter-example contains an extension pattern *)
let contains_extension pat =
  let r = ref false in
  let rec loop = function
    | {pat_desc = Tpat_var (_, {txt = "*extension*"})} -> r := true
    | p -> Typedtree.iter_pattern_desc loop p.pat_desc
  in
  loop pat;
  !r

(* Build an untyped or-pattern from its expected type *)
let ppat_of_type env ty =
  match pats_of_type env ty with
  | [{pat_desc = Tpat_any}] ->
    (Conv.mkpat Parsetree.Ppat_any, Hashtbl.create 0, Hashtbl.create 0)
  | pats -> Conv.conv (orify_many pats)

let do_check_partial ?partial_match_warning_hint ?pred exhaust loc casel pss =
  match pss with
  | [] ->
    (*
          This can occur
          - For empty matches generated by ocamlp4 (no warning)
          - when all patterns have guards (then, casel <> [])
          (specific warning)
          Then match MUST be considered non-exhaustive,
          otherwise compilation of PM is broken.
          *)
    (match casel with
    | [] -> ()
    | _ ->
      if Warnings.is_active Warnings.All_clauses_guarded then
        Location.prerr_warning loc Warnings.All_clauses_guarded);
    Partial
  | ps :: _ -> (
    match exhaust None pss (List.length ps) with
    | Rnone -> Total
    | Rsome [u] -> (
      let v =
        match pred with
        | Some pred ->
          let pattern, constrs, labels = Conv.conv u in
          let u' = pred constrs labels pattern in
          (* pretty_pat u;
             begin match u' with
               None -> prerr_endline ": impossible"
             | Some _ -> prerr_endline ": possible"
             end; *)
          u'
        | None -> Some u
      in
      match v with
      | None -> Total
      | Some v ->
        (if Warnings.is_active (Warnings.Partial_match "") then
           let errmsg =
             try
               let buf = Buffer.create 16 in
               Buffer.add_string buf "| ";
               Buffer.add_string buf (!print_res_pat v);
               (match check_partial_all v casel with
               | None -> ()
               | Some _ ->
                 (* This is 'Some loc', where loc is the location of
                    a possibly matching clause.
                    Forget about loc, because printing two locations
                    is a pain in the top-level *)
                 Buffer.add_string buf
                   "\n(However, some guarded clause may match this value.)");
               if contains_extension v then
                 Buffer.add_string buf
                   "\n\
                    Matching over values of extensible variant types (the \
                    *extension* above)\n\
                    must include a wild card pattern in order to be exhaustive.";
               (match partial_match_warning_hint with
               | None -> ()
               | Some h when String.length h > 0 ->
                 Buffer.add_string buf ("\n\n  " ^ h)
               | Some _ -> ());
               Buffer.contents buf
             with _ -> ""
           in
           Location.prerr_warning loc (Warnings.Partial_match errmsg));
        Partial)
    | _ -> fatal_error "Parmatch.check_partial")

(*
let do_check_partial_normal loc casel pss =
  do_check_partial exhaust loc casel pss
 *)

let do_check_partial_gadt ?partial_match_warning_hint pred loc casel pss =
  do_check_partial ?partial_match_warning_hint ~pred exhaust_gadt loc casel pss

(*****************)
(* Fragile check *)
(*****************)

(* Collect all data types in a pattern *)

let rec add_path path = function
  | [] -> [path]
  | x :: rem as paths ->
    if Path.same path x then paths else x :: add_path path rem

let extendable_path path =
  not
    (Path.same path Predef.path_bool
    || Path.same path Predef.path_list
    || Path.same path Predef.path_unit
    || Path.same path Predef.path_option)

let rec collect_paths_from_pat r p =
  match p.pat_desc with
  | Tpat_construct
      (_, {cstr_tag = Cstr_constant _ | Cstr_block _ | Cstr_unboxed}, ps) ->
    let path = get_type_path p.pat_type p.pat_env in
    List.fold_left collect_paths_from_pat
      (if extendable_path path then add_path path r else r)
      ps
  | Tpat_any | Tpat_var _ | Tpat_constant _ | Tpat_variant (_, None, _) -> r
  | Tpat_tuple ps
  | Tpat_array ps
  | Tpat_construct (_, {cstr_tag = Cstr_extension _}, ps) ->
    List.fold_left collect_paths_from_pat r ps
  | Tpat_record (lps, _) ->
    List.fold_left (fun r (_, _, p, _) -> collect_paths_from_pat r p) r lps
  | Tpat_variant (_, Some p, _) | Tpat_alias (p, _, _) ->
    collect_paths_from_pat r p
  | Tpat_or (p1, p2, _) ->
    collect_paths_from_pat (collect_paths_from_pat r p1) p2

(*
  Actual fragile check
   1. Collect data types in the patterns of the match.
   2. One exhaustivity check per datatype, considering that
      the type is extended.
*)

let do_check_fragile_param exhaust loc casel pss =
  let exts =
    List.fold_left (fun r c -> collect_paths_from_pat r c.c_lhs) [] casel
  in
  match exts with
  | [] -> ()
  | _ -> (
    match pss with
    | [] -> ()
    | ps :: _ ->
      List.iter
        (fun ext ->
          match exhaust (Some ext) pss (List.length ps) with
          | Rnone ->
            Location.prerr_warning loc (Warnings.Fragile_match (Path.name ext))
          | Rsome _ -> ())
        exts)

(*let do_check_fragile_normal = do_check_fragile_param exhaust*)
let do_check_fragile_gadt = do_check_fragile_param exhaust_gadt

(********************************)
(* Exported unused clause check *)
(********************************)

let check_unused pred casel =
  if Warnings.is_active Warnings.Unused_match then
    let rec do_rec pref = function
      | [] -> ()
      | {c_lhs = q; c_guard} :: rem ->
        let qs = [q] in
        (try
           let pss = get_mins le_pats (Ext_list.filter pref (compats qs)) in
           (* First look for redundant or partially redundant patterns *)
           let r = every_satisfiables (make_rows pss) (make_row qs) in
           (* Do not warn for unused [pat -> .] *)
           let r =
             (* Do not refine if there are no other lines *)
             let skip =
               r = Unused || pref = []
               || not (Warnings.is_active Warnings.Unreachable_case)
             in
             if skip then r
             else
               (* Then look for empty patterns *)
               let sfs = satisfiables pss qs in
               if sfs = [] then Unused
               else
                 let sfs =
                   List.map
                     (function
                       | [u] -> u
                       | _ -> assert false)
                     sfs
                 in
                 let u = orify_many sfs in
                 (*Format.eprintf "%a@." pretty_val u;*)
                 let pattern, constrs, labels = Conv.conv u in
                 let pattern = {pattern with Parsetree.ppat_loc = q.pat_loc} in
                 match pred constrs labels pattern with
                 | None ->
                   Location.prerr_warning q.pat_loc Warnings.Unreachable_case;
                   Used
                 | _ -> r
           in
           match r with
           | Unused -> Location.prerr_warning q.pat_loc Warnings.Unused_match
           | Upartial ps ->
             ps
             |> List.filter (fun p ->
                    not (Variant_type_spread.is_pat_from_variant_spread_attr p))
             |> List.iter (fun p ->
                    Location.prerr_warning p.pat_loc Warnings.Unused_pat)
           | Used -> ()
         with Empty | Not_found | NoGuard -> assert false);

        if c_guard <> None then do_rec pref rem else do_rec ([q] :: pref) rem
    in

    do_rec [] casel

(*********************************)
(* Exported irrefutability tests *)
(*********************************)

let irrefutable pat = le_pat pat omega

let inactive ~partial pat =
  match partial with
  | Partial -> false
  | Total ->
    let rec loop pat =
      match pat.pat_desc with
      | Tpat_array _ -> false
      | Tpat_any | Tpat_var _ | Tpat_variant (_, None, _) -> true
      | Tpat_constant c -> (
        match c with
        | Const_string _ -> true (*Config.safe_string*)
        | Const_int _ | Const_char _ | Const_float _ | Const_int32 _
        | Const_int64 _ | Const_bigint _ ->
          true)
      | Tpat_tuple ps | Tpat_construct (_, _, ps) ->
        List.for_all (fun p -> loop p) ps
      | Tpat_alias (p, _, _) | Tpat_variant (_, Some p, _) -> loop p
      | Tpat_record (ldps, _) ->
        List.for_all
          (fun (_, lbl, p, _) -> lbl.lbl_mut = Immutable && loop p)
          ldps
      | Tpat_or (p, q, _) -> loop p && loop q
    in
    loop pat

(*********************************)
(* Exported exhaustiveness check *)
(*********************************)

(*
   Fragile check is performed when required and
   on exhaustive matches only.
*)

let check_partial_param do_check_partial do_check_fragile loc casel =
  let pss = initial_matrix casel in
  let pss = get_mins le_pats pss in
  let total = do_check_partial loc casel pss in
  if total = Total && Warnings.is_active (Warnings.Fragile_match "") then
    do_check_fragile loc casel pss;
  total

(*let check_partial =
    check_partial_param
      do_check_partial_normal
      do_check_fragile_normal*)

let check_partial_gadt ?partial_match_warning_hint pred loc casel =
  check_partial_param
    (do_check_partial_gadt ?partial_match_warning_hint pred)
    do_check_fragile_gadt loc casel

(*************************************)
(* Ambiguous variable in or-patterns *)
(*************************************)

(* Specification: ambiguous variables in or-patterns.

   The semantics of or-patterns in OCaml is specified with
   a left-to-right bias: a value [v] matches the pattern [p | q] if it
   matches [p] or [q], but if it matches both, the environment
   captured by the match is the environment captured by [p], never the
   one captured by [q].

   While this property is generally well-understood, one specific case
   where users expect a different semantics is when a pattern is
   followed by a when-guard: [| p when g -> e]. Consider for example:

     | ((Const x, _) | (_, Const x)) when is_neutral x -> branch

   The semantics is clear: match the scrutinee against the pattern, if
   it matches, test the guard, and if the guard passes, take the
   branch.

   However, consider the input [(Const a, Const b)], where [a] fails
   the test [is_neutral f], while [b] passes the test [is_neutral
   b]. With the left-to-right semantics, the clause above is *not*
   taken by its input: matching [(Const a, Const b)] against the
   or-pattern succeeds in the left branch, it returns the environment
   [x -> a], and then the guard [is_neutral a] is tested and fails,
   the branch is not taken. Most users, however, intuitively expect
   that any pair that has one side passing the test will take the
   branch. They assume it is equivalent to the following:

     | (Const x, _) when is_neutral x -> branch
     | (_, Const x) when is_neutral x -> branch

   while it is not.

   The code below is dedicated to finding these confusing cases: the
   cases where a guard uses "ambiguous" variables, that are bound to
   different parts of the scrutinees by different sides of
   a or-pattern. In other words, it finds the cases where the
   specified left-to-right semantics is not equivalent to
   a non-deterministic semantics (any branch can be taken) relatively
   to a specific guard.
*)

module IdSet = Set.Make (Ident)

let pattern_vars p = IdSet.of_list (Typedtree.pat_bound_idents p)

(* Row for ambiguous variable search,
   unseen is the traditional pattern row,
   seen   is a list of position bindings *)

type amb_row = {unseen: pattern list; seen: IdSet.t list}

(* Push binding variables now *)

let rec do_push r p ps seen k =
  match p.pat_desc with
  | Tpat_alias (p, x, _) -> do_push (IdSet.add x r) p ps seen k
  | Tpat_var (x, _) -> (omega, {unseen = ps; seen = IdSet.add x r :: seen}) :: k
  | Tpat_or (p1, p2, _) -> do_push r p1 ps seen (do_push r p2 ps seen k)
  | _ -> (p, {unseen = ps; seen = r :: seen}) :: k

let rec push_vars = function
  | [] -> []
  | {unseen = []} :: _ -> assert false
  | {unseen = p :: ps; seen} :: rem ->
    do_push IdSet.empty p ps seen (push_vars rem)

let collect_stable = function
  | [] -> assert false
  | {seen = xss; _} :: rem ->
    let rec c_rec xss = function
      | [] -> xss
      | {seen = yss; _} :: rem ->
        let xss = List.map2 IdSet.inter xss yss in
        c_rec xss rem
    in
    let inters = c_rec xss rem in
    List.fold_left IdSet.union IdSet.empty inters

(*********************************************)
(* Filtering utilities for our specific rows *)
(*********************************************)

(* Take a pattern matrix as a list (rows) of lists (columns) of patterns
     | p1, p2, .., pn
     | q1, q2, .., qn
     | r1, r2, .., rn
     | ...

   We split this matrix into a list of sub-matrices, one for each head
   constructor appearing in the leftmost column. For each row whose
   left column starts with a head constructor, remove this head
   column, prepend one column for each argument of the constructor,
   and add the resulting row in the sub-matrix corresponding to this
   head constructor.

   Rows whose left column is omega (the Any pattern _) may match any
   head constructor, so they are added to all groups.

   The list of sub-matrices is represented as a list of pair
     (head constructor, submatrix)
*)

let filter_all =
  (* the head constructor (as a pattern with omega arguments) of
     a pattern *)
  let discr_head pat =
    match pat.pat_desc with
    | Tpat_record (lbls, closed) ->
      (* a partial record pattern { f1 = p1; f2 = p2; _ }
         needs to be expanded, otherwise matching against this head
         would drop the pattern arguments for non-mentioned fields *)
      let lbls = all_record_args lbls in
      normalize_pat {pat with pat_desc = Tpat_record (lbls, closed)}
    | _ -> normalize_pat pat
  in

  (* insert a row of head [p] and rest [r] into the right group *)
  let rec insert p r env =
    match env with
    | [] ->
      (* if no group matched this row, it has a head constructor that
         was never seen before; add a new sub-matrix for this head *)
      let p0 = discr_head p in
      [(p0, [{r with unseen = simple_match_args p0 p @ r.unseen}])]
    | ((q0, rs) as bd) :: env ->
      if simple_match q0 p then
        let r = {r with unseen = simple_match_args q0 p @ r.unseen} in
        (q0, r :: rs) :: env
      else bd :: insert p r env
  in

  (* insert a row of head omega into all groups *)
  let insert_omega r env =
    List.map
      (fun (q0, rs) ->
        let r = {r with unseen = simple_match_args q0 omega @ r.unseen} in
        (q0, r :: rs))
      env
  in

  let rec filter_rec env = function
    | [] -> env
    | ({pat_desc = Tpat_var _ | Tpat_alias _ | Tpat_or _}, _) :: _ ->
      assert false
    | ({pat_desc = Tpat_any}, _) :: rs -> filter_rec env rs
    | (p, r) :: rs -> filter_rec (insert p r env) rs
  in

  let rec filter_omega env = function
    | [] -> env
    | ({pat_desc = Tpat_var _ | Tpat_alias _ | Tpat_or _}, _) :: _ ->
      assert false
    | ({pat_desc = Tpat_any}, r) :: rs -> filter_omega (insert_omega r env) rs
    | _ :: rs -> filter_omega env rs
  in

  fun rs ->
    (* first insert the rows with head constructors,
       to get the definitive list of groups *)
    let env = filter_rec [] rs in
    (* then add the omega rows to all groups *)
    filter_omega env rs

(* Compute stable bindings *)

let rec do_stable rs =
  match rs with
  | [] -> assert false (* No empty matrix *)
  | {unseen = []; _} :: _ -> collect_stable rs
  | _ -> (
    let rs = push_vars rs in
    if not (all_coherent (first_column rs)) then
      (* If the first column is incoherent, then all the variables of this
         matrix are stable. *)
      List.fold_left
        (fun acc (_, {seen; _}) -> List.fold_left IdSet.union acc seen)
        IdSet.empty rs
    else
      (* If the column is ill-typed but deemed coherent, we might spuriously
         warn about some variables being unstable.
         As sad as that might be, the warning can be silenced by splitting the
         or-pattern... *)
      match filter_all rs with
      | [] -> do_stable (List.map snd rs)
      | (_, rs) :: env ->
        List.fold_left
          (fun xs (_, rs) -> IdSet.inter xs (do_stable rs))
          (do_stable rs) env)

let stable p = do_stable [{unseen = [p]; seen = []}]

(* All identifier paths that appear in an expression that occurs
    as a clause right hand side or guard.

   The function is rather complex due to the compilation of
   unpack patterns by introducing code in rhs expressions
   and **guards**.

   For pattern (module M:S)  -> e the code is
   let module M_mod = unpack M .. in e

   Hence M is "free" in e iff M_mod is free in e.

   Not doing so will yield excessive  warning in
   (module (M:S) } ...) when true -> ....
   as M is always present in
   let module M_mod = unpack M .. in true
*)

let all_rhs_idents exp =
  let ids = ref IdSet.empty in
  let module Iterator = TypedtreeIter.MakeIterator (struct
    include TypedtreeIter.DefaultIteratorArgument
    let enter_expression exp =
      match exp.exp_desc with
      | Texp_ident (path, _lid, _descr) ->
        List.iter (fun id -> ids := IdSet.add id !ids) (Path.heads path)
      | _ -> ()

    (* Very hackish, detect unpack pattern  compilation
       and perform "indirect check for them" *)
    let is_unpack exp =
      List.exists (fun (attr, _) -> attr.txt = "#modulepat") exp.exp_attributes

    let leave_expression exp =
      if is_unpack exp then
        match exp.exp_desc with
        | Texp_letmodule
            ( id_mod,
              _,
              {
                mod_desc =
                  Tmod_unpack
                    ({exp_desc = Texp_ident (Path.Pident id_exp, _, _)}, _);
              },
              _ ) ->
          assert (IdSet.mem id_exp !ids);
          if not (IdSet.mem id_mod !ids) then ids := IdSet.remove id_exp !ids
        | _ -> assert false
  end) in
  Iterator.iter_expression exp;
  !ids

let check_ambiguous_bindings =
  let open Warnings in
  let warn0 = Ambiguous_pattern [] in
  fun cases ->
    if is_active warn0 then
      List.iter
        (fun case ->
          match case with
          | {c_guard = None; _} -> ()
          | {c_lhs = p; c_guard = Some g; _} ->
            let all = IdSet.inter (pattern_vars p) (all_rhs_idents g) in
            if not (IdSet.is_empty all) then
              let st = stable p in
              let ambiguous = IdSet.diff all st in
              if not (IdSet.is_empty ambiguous) then
                let pps = IdSet.elements ambiguous |> List.map Ident.name in
                let warn = Ambiguous_pattern pps in
                Location.prerr_warning p.pat_loc warn)
        cases
