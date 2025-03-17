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

(* Compilation of pattern matching *)

open Misc
open Asttypes
open Types
open Typedtree
open Lambda
open Parmatch
open Printf

let dbg = false

(* See Peyton-Jones, ``The Implementation of functional programming
   languages'', chapter 5. *)
(*
  Well, it was true at the beginning of the world.
  Now, see Lefessant-Maranget ``Optimizing Pattern-Matching'' ICFP'2001
*)

(*
   Compatibility predicate that considers potential rebindings of constructors
   of an extension type.

   "may_compat p q" returns false when p and q never admit a common instance;
   returns true when they may have a common instance.
*)

module MayCompat = Parmatch.Compat (struct
  let equal = Types.may_equal_constr
end)
let may_compat = MayCompat.compat

and may_compats = MayCompat.compats

(*
   Many functions on the various data structures of the algorithm :
     - Pattern matrices.
     - Default environments: mapping from matrices to exit numbers.
     - Contexts:  matrices whose column are partitioned into
       left and right.
     - Jump summaries: mapping from exit numbers to contexts
*)

let string_of_lam lam =
  Printlambda.lambda Format.str_formatter lam;
  Format.flush_str_formatter ()

type matrix = pattern list list

let add_omega_column pss = List.map (fun ps -> omega :: ps) pss

type ctx = {left: pattern list; right: pattern list}

let pretty_ctx ctx =
  List.iter
    (fun {left; right} ->
      prerr_string "LEFT:";
      pretty_line left;
      prerr_string " RIGHT:";
      pretty_line right;
      prerr_endline "")
    ctx

let le_ctx c1 c2 = le_pats c1.left c2.left && le_pats c1.right c2.right

let lshift {left; right} =
  match right with
  | x :: xs -> {left = x :: left; right = xs}
  | _ -> assert false

let lforget {left; right} =
  match right with
  | _ :: xs -> {left = omega :: left; right = xs}
  | _ -> assert false

let rec small_enough n = function
  | [] -> true
  | _ :: rem -> if n <= 0 then false else small_enough (n - 1) rem

let ctx_lshift ctx =
  if small_enough 31 ctx then List.map lshift ctx
  else (* Context pruning *) get_mins le_ctx (List.map lforget ctx)

let rshift {left; right} =
  match left with
  | p :: ps -> {left = ps; right = p :: right}
  | _ -> assert false

let ctx_rshift ctx = List.map rshift ctx

let rec nchars n ps =
  if n <= 0 then ([], ps)
  else
    match ps with
    | p :: rem ->
      let chars, cdrs = nchars (n - 1) rem in
      (p :: chars, cdrs)
    | _ -> assert false

let rshift_num n {left; right} =
  let shifted, left = nchars n left in
  {left; right = shifted @ right}

let ctx_rshift_num n ctx = List.map (rshift_num n) ctx

(* Recombination of contexts (eg: (_,_)::p1::p2::rem ->  (p1,p2)::rem)
   All mutable fields are replaced by '_', since side-effects in
   guards can alter these fields *)

let combine {left; right} =
  match left with
  | p :: ps -> {left = ps; right = set_args_erase_mutable p right}
  | _ -> assert false

let ctx_combine ctx = List.map combine ctx

let ncols = function
  | [] -> 0
  | ps :: _ -> List.length ps

exception NoMatch
exception OrPat

let filter_matrix matcher pss =
  let rec filter_rec = function
    | (p :: ps) :: rem -> (
      match p.pat_desc with
      | Tpat_alias (p, _, _) -> filter_rec ((p :: ps) :: rem)
      | Tpat_var _ -> filter_rec ((omega :: ps) :: rem)
      | _ -> (
        let rem = filter_rec rem in
        try matcher p ps :: rem with
        | NoMatch -> rem
        | OrPat -> (
          match p.pat_desc with
          | Tpat_or (p1, p2, _) -> filter_rec [p1 :: ps; p2 :: ps] @ rem
          | _ -> assert false)))
    | [] -> []
    | _ ->
      pretty_matrix pss;
      fatal_error "Matching.filter_matrix"
  in
  filter_rec pss

let make_default matcher env =
  let rec make_rec = function
    | [] -> []
    | ([[]], i) :: _ -> [([[]], i)]
    | (pss, i) :: rem -> (
      let rem = make_rec rem in
      match filter_matrix matcher pss with
      | [] -> rem
      | [] :: _ -> ([[]], i) :: rem
      | pss -> (pss, i) :: rem)
  in
  make_rec env

let ctx_matcher p =
  let p = normalize_pat p in
  match p.pat_desc with
  | Tpat_construct (_, cstr, omegas) -> (
    fun q rem ->
      match q.pat_desc with
      | Tpat_construct (_, cstr', args)
      (* NB:  may_constr_equal considers (potential) constructor rebinding *)
        when Types.may_equal_constr cstr cstr' ->
        (p, args @ rem)
      | Tpat_any -> (p, omegas @ rem)
      | _ -> raise NoMatch)
  | Tpat_constant cst -> (
    fun q rem ->
      match q.pat_desc with
      | Tpat_constant cst' when const_compare cst cst' = 0 -> (p, rem)
      | Tpat_any -> (p, rem)
      | _ -> raise NoMatch)
  | Tpat_variant (lab, Some omega, _) -> (
    fun q rem ->
      match q.pat_desc with
      | Tpat_variant (lab', Some arg, _) when lab = lab' -> (p, arg :: rem)
      | Tpat_any -> (p, omega :: rem)
      | _ -> raise NoMatch)
  | Tpat_variant (lab, None, _) -> (
    fun q rem ->
      match q.pat_desc with
      | Tpat_variant (lab', None, _) when lab = lab' -> (p, rem)
      | Tpat_any -> (p, rem)
      | _ -> raise NoMatch)
  | Tpat_array omegas -> (
    let len = List.length omegas in
    fun q rem ->
      match q.pat_desc with
      | Tpat_array args when List.length args = len -> (p, args @ rem)
      | Tpat_any -> (p, omegas @ rem)
      | _ -> raise NoMatch)
  | Tpat_tuple omegas -> (
    let len = List.length omegas in
    fun q rem ->
      match q.pat_desc with
      | Tpat_tuple args when List.length args = len -> (p, args @ rem)
      | Tpat_any -> (p, omegas @ rem)
      | _ -> raise NoMatch)
  | Tpat_record (((_, lbl, _, _) :: _ as l), _) -> (
    (* Records are normalized *)
    let len = Array.length lbl.lbl_all in
    fun q rem ->
      match q.pat_desc with
      | Tpat_record (((_, lbl', _, _) :: _ as l'), _)
        when Array.length lbl'.lbl_all = len ->
        let l' = all_record_args l' in
        (p, List.fold_right (fun (_, _, p, _) r -> p :: r) l' rem)
      | Tpat_any -> (p, List.fold_right (fun (_, _, p, _) r -> p :: r) l rem)
      | _ -> raise NoMatch)
  | Tpat_lazy omega -> (
    fun q rem ->
      match q.pat_desc with
      | Tpat_lazy arg -> (p, arg :: rem)
      | Tpat_any -> (p, omega :: rem)
      | _ -> raise NoMatch)
  | _ -> fatal_error "Matching.ctx_matcher"

let filter_ctx q ctx =
  let matcher = ctx_matcher q in

  let rec filter_rec = function
    | ({right = p :: ps} as l) :: rem -> (
      match p.pat_desc with
      | Tpat_or (p1, p2, _) ->
        filter_rec
          ({l with right = p1 :: ps} :: {l with right = p2 :: ps} :: rem)
      | Tpat_alias (p, _, _) -> filter_rec ({l with right = p :: ps} :: rem)
      | Tpat_var _ -> filter_rec ({l with right = omega :: ps} :: rem)
      | _ -> (
        let rem = filter_rec rem in
        try
          let to_left, right = matcher p ps in
          {left = to_left :: l.left; right} :: rem
        with NoMatch -> rem))
    | [] -> []
    | _ -> fatal_error "Matching.filter_ctx"
  in

  filter_rec ctx

let select_columns pss ctx =
  let n = ncols pss in
  List.fold_right
    (fun ps r ->
      List.fold_right
        (fun {left; right} r ->
          let transfert, right = nchars n right in
          try {left = lubs transfert ps @ left; right} :: r with Empty -> r)
        ctx r)
    pss []

let ctx_lub p ctx =
  List.fold_right
    (fun {left; right} r ->
      match right with
      | q :: rem -> ( try {left; right = lub p q :: rem} :: r with Empty -> r)
      | _ -> fatal_error "Matching.ctx_lub")
    ctx []

let ctx_match ctx pss =
  List.exists
    (fun {right = qs} -> List.exists (fun ps -> may_compats qs ps) pss)
    ctx

type jumps = (int * ctx list) list

let pretty_jumps (env : jumps) =
  match env with
  | [] -> ()
  | _ ->
    List.iter
      (fun (i, ctx) ->
        Printf.fprintf stderr "jump for %d\n" i;
        pretty_ctx ctx)
      env

let rec jumps_extract (i : int) = function
  | [] -> ([], [])
  | ((j, pss) as x) :: rem as all ->
    if i = j then (pss, rem)
    else if j < i then ([], all)
    else
      let r, rem = jumps_extract i rem in
      (r, x :: rem)

let rec jumps_remove (i : int) = function
  | [] -> []
  | (j, _) :: rem when i = j -> rem
  | x :: rem -> x :: jumps_remove i rem

let jumps_empty = []

and jumps_is_empty = function
  | [] -> true
  | _ -> false

let jumps_singleton i = function
  | [] -> []
  | ctx -> [(i, ctx)]

let jumps_add i pss jumps =
  match pss with
  | [] -> jumps
  | _ ->
    let rec add = function
      | [] -> [(i, pss)]
      | ((j, qss) as x) :: rem as all ->
        if (j : int) > i then x :: add rem
        else if j < i then (i, pss) :: all
        else (i, get_mins le_ctx (pss @ qss)) :: rem
    in
    add jumps

let rec jumps_union (env1 : (int * ctx list) list) env2 =
  match (env1, env2) with
  | [], _ -> env2
  | _, [] -> env1
  | ((i1, pss1) as x1) :: rem1, ((i2, pss2) as x2) :: rem2 ->
    if i1 = i2 then (i1, get_mins le_ctx (pss1 @ pss2)) :: jumps_union rem1 rem2
    else if i1 > i2 then x1 :: jumps_union rem1 env2
    else x2 :: jumps_union env1 rem2

let rec merge = function
  | env1 :: env2 :: rem -> jumps_union env1 env2 :: merge rem
  | envs -> envs

let rec jumps_unions envs =
  match envs with
  | [] -> []
  | [env] -> env
  | _ -> jumps_unions (merge envs)

let jumps_map f env = List.map (fun (i, pss) -> (i, f pss)) env

(* Pattern matching before any compilation *)

type pattern_matching = {
  mutable cases: (pattern list * lambda) list;
  args: (lambda * let_kind) list;
  default: (matrix * int) list;
}

(* Pattern matching after application of both the or-pat rule and the
   mixture rule *)

type pm_or_compiled = {
  body: pattern_matching;
  handlers: (matrix * int * Ident.t list * pattern_matching) list;
  or_matrix: matrix;
}

type pm_half_compiled =
  | PmOr of pm_or_compiled
  | PmVar of pm_var_compiled
  | Pm of pattern_matching

and pm_var_compiled = {inside: pm_half_compiled; var_arg: lambda}

type pm_half_compiled_info = {
  me: pm_half_compiled;
  matrix: matrix;
  top_default: (matrix * int) list;
}

let pretty_cases cases =
  List.iter
    (fun (ps, _l) ->
      List.iter
        (fun p ->
          Parmatch.top_pretty Format.str_formatter p;
          prerr_string " ";
          prerr_string (Format.flush_str_formatter ()))
        ps;
      (*
      prerr_string " -> " ;
      Printlambda.lambda Format.str_formatter l ;
      prerr_string (Format.flush_str_formatter ()) ;
*)
      prerr_endline "")
    cases

let pretty_def def =
  prerr_endline "+++++ Defaults +++++";
  List.iter
    (fun (pss, i) ->
      Printf.fprintf stderr "Matrix for %d\n" i;
      pretty_matrix pss)
    def;
  prerr_endline "+++++++++++++++++++++"

let pretty_pm pm =
  pretty_cases pm.cases;
  if pm.default <> [] then pretty_def pm.default

let rec pretty_precompiled = function
  | Pm pm ->
    prerr_endline "++++ PM ++++";
    pretty_pm pm
  | PmVar x ->
    prerr_endline "++++ VAR ++++";
    pretty_precompiled x.inside
  | PmOr x ->
    prerr_endline "++++ OR ++++";
    pretty_pm x.body;
    pretty_matrix x.or_matrix;
    List.iter
      (fun (_, i, _, pm) ->
        eprintf "++ Handler %d ++\n" i;
        pretty_pm pm)
      x.handlers

let pretty_precompiled_res first nexts =
  pretty_precompiled first;
  List.iter
    (fun (e, pmh) ->
      eprintf "** DEFAULT %d **\n" e;
      pretty_precompiled pmh)
    nexts

(* Identifying some semantically equivalent lambda-expressions,
   Our goal here is also to
   find alpha-equivalent (simple) terms *)

(* However, as shown by PR#6359 such sharing may hinders the
   lambda-code invariant that all bound idents are unique,
   when switches are compiled to test sequences.
   The definitive fix is the systematic introduction of exit/catch
   in case action sharing is present.
*)

module StoreExp = Switch.Store (struct
  type t = lambda
  type key = lambda
  let compare_key = compare
  let make_key = Lambda.make_key
end)

let make_exit i = Lstaticraise (i, [])

(* Introduce a catch, if worth it *)
let make_catch d k =
  match d with
  | Lstaticraise (_, []) -> k d
  | _ ->
    let e = next_raise_count () in
    Lstaticcatch (k (make_exit e), (e, []), d)

(* Introduce a catch, if worth it, delayed version *)
let rec as_simple_exit = function
  | Lstaticraise (i, []) -> Some i
  | Llet (Alias, _k, _, _, e) -> as_simple_exit e
  | _ -> None

let make_catch_delayed handler =
  match as_simple_exit handler with
  | Some i -> (i, fun act -> act)
  | None -> (
    let i = next_raise_count () in
    (*
    Printf.eprintf "SHARE LAMBDA: %i\n%s\n" i (string_of_lam handler);
*)
    ( i,
      fun body ->
        match body with
        | Lstaticraise (j, _) -> if i = j then handler else body
        | _ -> Lstaticcatch (body, (i, []), handler) ))

let raw_action l =
  match make_key l with
  | Some l -> l
  | None -> l

let tr_raw act =
  match make_key act with
  | Some act -> act
  | None -> raise Exit

let same_actions = function
  | [] -> None
  | [(_, act)] -> Some act
  | (_, act0) :: rem -> (
    try
      let raw_act0 = tr_raw act0 in
      let rec s_rec = function
        | [] -> Some act0
        | (_, act) :: rem -> if raw_act0 = tr_raw act then s_rec rem else None
      in
      s_rec rem
    with Exit -> None)

(* Test for swapping two clauses *)

let up_ok_action act1 act2 =
  try
    let raw1 = tr_raw act1 and raw2 = tr_raw act2 in
    raw1 = raw2
  with Exit -> false

let up_ok (ps, act_p) l =
  List.for_all
    (fun (qs, act_q) -> up_ok_action act_p act_q || not (may_compats ps qs))
    l

(*
   The simplify function normalizes the first column of the match
     - records are expanded so that they possess all fields
     - aliases are removed and replaced by bindings in actions.
   However or-patterns are simplified differently,
     - aliases are not removed
     - or-patterns (_|p) are changed into _
*)

exception Var of pattern

let simplify_or p =
  let rec simpl_rec p =
    match p with
    | {pat_desc = Tpat_any | Tpat_var _} -> raise (Var p)
    | {pat_desc = Tpat_alias (q, id, s)} -> (
      try {p with pat_desc = Tpat_alias (simpl_rec q, id, s)}
      with Var q -> raise (Var {p with pat_desc = Tpat_alias (q, id, s)}))
    | {pat_desc = Tpat_or (p1, p2, o)} -> (
      let q1 = simpl_rec p1 in
      try
        let q2 = simpl_rec p2 in
        {p with pat_desc = Tpat_or (q1, q2, o)}
      with Var q2 -> raise (Var {p with pat_desc = Tpat_or (q1, q2, o)}))
    | {pat_desc = Tpat_record (lbls, closed)} ->
      let all_lbls = all_record_args lbls in
      {p with pat_desc = Tpat_record (all_lbls, closed)}
    | _ -> p
  in
  try simpl_rec p with Var p -> p

let simplify_cases args cls =
  match args with
  | [] -> assert false
  | (arg, _) :: _ ->
    let rec simplify = function
      | [] -> []
      | ((pat :: patl, action) as cl) :: rem -> (
        match pat.pat_desc with
        | Tpat_var (id, _) ->
          (omega :: patl, bind Alias id arg action) :: simplify rem
        | Tpat_any -> cl :: simplify rem
        | Tpat_alias (p, id, _) ->
          simplify ((p :: patl, bind Alias id arg action) :: rem)
        | Tpat_record ([], _) -> (omega :: patl, action) :: simplify rem
        | Tpat_record (lbls, closed) ->
          let all_lbls = all_record_args lbls in
          let full_pat = {pat with pat_desc = Tpat_record (all_lbls, closed)} in
          (full_pat :: patl, action) :: simplify rem
        | Tpat_or _ -> (
          let pat_simple = simplify_or pat in
          match pat_simple.pat_desc with
          | Tpat_or _ -> (pat_simple :: patl, action) :: simplify rem
          | _ -> simplify ((pat_simple :: patl, action) :: rem))
        | _ -> cl :: simplify rem)
      | _ -> assert false
    in

    simplify cls

(* Once matchings are simplified one can easily find
   their nature *)

let rec what_is_cases cases =
  match cases with
  | ({pat_desc = Tpat_any} :: _, _) :: rem -> what_is_cases rem
  | ({pat_desc = Tpat_var _ | Tpat_or (_, _, _) | Tpat_alias (_, _, _)} :: _, _)
    :: _ ->
    assert false (* applies to simplified matchings only *)
  | (p :: _, _) :: _ -> p
  | [] -> omega
  | _ -> assert false

(* A few operations on default environments *)
let as_matrix cases = get_mins le_pats (List.map (fun (ps, _) -> ps) cases)

let cons_default matrix raise_num default =
  match matrix with
  | [] -> default
  | _ -> (matrix, raise_num) :: default

let default_compat p def =
  List.fold_right
    (fun (pss, i) r ->
      let qss =
        List.fold_right
          (fun qs r ->
            match qs with
            | q :: rem when may_compat p q -> rem :: r
            | _ -> r)
          pss []
      in
      match qss with
      | [] -> r
      | _ -> (qss, i) :: r)
    def []

(* Or-pattern expansion, variables are a complication w.r.t. the article *)
let rec extract_vars r p =
  match p.pat_desc with
  | Tpat_var (id, _) -> IdentSet.add id r
  | Tpat_alias (p, id, _) -> extract_vars (IdentSet.add id r) p
  | Tpat_tuple pats -> List.fold_left extract_vars r pats
  | Tpat_record (lpats, _) ->
    List.fold_left (fun r (_, _, p, _) -> extract_vars r p) r lpats
  | Tpat_construct (_, _, pats) -> List.fold_left extract_vars r pats
  | Tpat_array pats -> List.fold_left extract_vars r pats
  | Tpat_variant (_, Some p, _) -> extract_vars r p
  | Tpat_lazy p -> extract_vars r p
  | Tpat_or (p, _, _) -> extract_vars r p
  | Tpat_constant _ | Tpat_any | Tpat_variant (_, None, _) -> r

exception Cannot_flatten

let mk_alpha_env arg aliases ids =
  List.map
    (fun id ->
      ( id,
        if List.mem id aliases then
          match arg with
          | Some v -> v
          | _ -> raise Cannot_flatten
        else Ident.create (Ident.name id) ))
    ids

let rec explode_or_pat arg patl mk_action rem vars aliases = function
  | {pat_desc = Tpat_or (p1, p2, _)} ->
    explode_or_pat arg patl mk_action
      (explode_or_pat arg patl mk_action rem vars aliases p2)
      vars aliases p1
  | {pat_desc = Tpat_alias (p, id, _)} ->
    explode_or_pat arg patl mk_action rem vars (id :: aliases) p
  | {pat_desc = Tpat_var (x, _)} ->
    let env = mk_alpha_env arg (x :: aliases) vars in
    (omega :: patl, mk_action (List.map snd env)) :: rem
  | p ->
    let env = mk_alpha_env arg aliases vars in
    (alpha_pat env p :: patl, mk_action (List.map snd env)) :: rem

let pm_free_variables {cases} =
  List.fold_right
    (fun (_, act) r -> IdentSet.union (free_variables act) r)
    cases IdentSet.empty

(* Basic grouping predicates *)
let pat_as_constr = function
  | {pat_desc = Tpat_construct (_, cstr, _)} -> cstr
  | _ -> fatal_error "Matching.pat_as_constr"

let group_constant = function
  | {pat_desc = Tpat_constant _} -> true
  | _ -> false

and group_constructor = function
  | {pat_desc = Tpat_construct (_, _, _)} -> true
  | _ -> false

and group_variant = function
  | {pat_desc = Tpat_variant (_, _, _)} -> true
  | _ -> false

and group_var = function
  | {pat_desc = Tpat_any} -> true
  | _ -> false

and group_tuple = function
  | {pat_desc = Tpat_tuple _ | Tpat_any} -> true
  | _ -> false

and group_record = function
  | {pat_desc = Tpat_record _ | Tpat_any} -> true
  | _ -> false

and group_array = function
  | {pat_desc = Tpat_array _} -> true
  | _ -> false

and group_lazy = function
  | {pat_desc = Tpat_lazy _} -> true
  | _ -> false

let get_group p =
  match p.pat_desc with
  | Tpat_any -> group_var
  | Tpat_constant _ -> group_constant
  | Tpat_construct _ -> group_constructor
  | Tpat_tuple _ -> group_tuple
  | Tpat_record _ -> group_record
  | Tpat_array _ -> group_array
  | Tpat_variant (_, _, _) -> group_variant
  | Tpat_lazy _ -> group_lazy
  | _ -> fatal_error "Matching.get_group"

let is_or p =
  match p.pat_desc with
  | Tpat_or _ -> true
  | _ -> false

(* Conditions for appending to the Or matrix *)
let conda p q = not (may_compat p q)

and condb act ps qs = (not (is_guarded act)) && Parmatch.le_pats qs ps

let or_ok p ps l =
  List.for_all
    (function
      | ({pat_desc = Tpat_or _} as q) :: qs, act -> conda p q || condb act ps qs
      | _ -> true)
    l

(* Insert or append a pattern in the Or matrix *)

let equiv_pat p q = le_pat p q && le_pat q p

let rec get_equiv p l =
  match l with
  | ((q :: _, _) as cl) :: rem ->
    if equiv_pat p q then
      let others, rem = get_equiv p rem in
      (cl :: others, rem)
    else ([], l)
  | _ -> ([], l)

let insert_or_append p ps act ors no =
  let rec attempt seen = function
    | ((q :: qs, act_q) as cl) :: rem ->
      if is_or q then
        if may_compat p q then
          if
            IdentSet.is_empty (extract_vars IdentSet.empty p)
            && IdentSet.is_empty (extract_vars IdentSet.empty q)
            && equiv_pat p q
          then
            (* attempt insert, for equivalent orpats with no variables *)
            let _, not_e = get_equiv q rem in
            if
              or_ok p ps not_e
              &&
              (* check append condition for head of O *)
              List.for_all (* check insert condition for tail of O *)
                (fun cl ->
                  match cl with
                  | q :: _, _ -> not (may_compat p q)
                  | _ -> assert false)
                seen
            then
              (* insert *)
              (List.rev_append seen ((p :: ps, act) :: cl :: rem), no)
            else (* fail to insert or append *)
              (ors, (p :: ps, act) :: no)
          else if condb act_q ps qs then
            (* check condition (b) for append *)
            attempt (cl :: seen) rem
          else (ors, (p :: ps, act) :: no)
        else (* p # q, go on with append/insert *)
          attempt (cl :: seen) rem
      else
        (* q is not an or-pat, go on with append/insert *)
        attempt (cl :: seen) rem
    | _ ->
      (* [] in fact *)
      ((p :: ps, act) :: ors, no)
  in
  (* success in appending *)
  attempt [] ors

(* Reconstruct default information from half_compiled  pm list *)

let rec rebuild_matrix pmh =
  match pmh with
  | Pm pm -> as_matrix pm.cases
  | PmOr {or_matrix = m} -> m
  | PmVar x -> add_omega_column (rebuild_matrix x.inside)

let rec rebuild_default nexts def =
  match nexts with
  | [] -> def
  | (e, pmh) :: rem ->
    (add_omega_column (rebuild_matrix pmh), e) :: rebuild_default rem def

let rebuild_nexts arg nexts k =
  List.fold_right
    (fun (e, pm) k -> (e, PmVar {inside = pm; var_arg = arg}) :: k)
    nexts k

(*
  Split a matching.
    Splitting is first directed by or-patterns, then by
    tests (e.g. constructors)/variable transitions.

    The approach is greedy, every split function attempts to
    raise rows as much as possible in the top matrix,
    then splitting applies again to the remaining rows.

    Some precompilation of or-patterns and
    variable pattern occurs. Mostly this means that bindings
    are performed now,  being replaced by let-bindings
    in actions (cf. simplify_cases).

    Additionally, if the match argument is a variable, matchings whose
    first column is made of variables only are splitted further
    (cf. precompile_var).

*)

let rec split_or argo cls args def =
  let cls = simplify_cases args cls in

  let rec do_split before ors no = function
    | [] -> cons_next (List.rev before) (List.rev ors) (List.rev no)
    | ((p :: ps, act) as cl) :: rem ->
      if up_ok cl no then
        if is_or p then
          let ors, no = insert_or_append p ps act ors no in
          do_split before ors no rem
        else if up_ok cl ors then do_split (cl :: before) ors no rem
        else if or_ok p ps ors then do_split before (cl :: ors) no rem
        else do_split before ors (cl :: no) rem
      else do_split before ors (cl :: no) rem
    | _ -> assert false
  and cons_next yes yesor = function
    | [] -> precompile_or argo yes yesor args def []
    | rem ->
      let {me = next; matrix; top_default = def}, nexts =
        do_split [] [] [] rem
      in
      let idef = next_raise_count () in
      precompile_or argo yes yesor args
        (cons_default matrix idef def)
        ((idef, next) :: nexts)
  in

  do_split [] [] [] cls

(* Ultra-naive splitting, close to semantics, used for extension,
   as potential rebind prevents any kind of optimisation *)

and split_naive cls args def k =
  let rec split_exc cstr0 yes = function
    | [] ->
      let yes = List.rev yes in
      ( {
          me = Pm {cases = yes; args; default = def};
          matrix = as_matrix yes;
          top_default = def;
        },
        k )
    | ((p :: _, _) as cl) :: rem ->
      if group_constructor p then
        let cstr = pat_as_constr p in
        if cstr = cstr0 then split_exc cstr0 (cl :: yes) rem
        else
          let yes = List.rev yes in
          let {me = next; matrix; top_default = def}, nexts =
            split_exc cstr [cl] rem
          in
          let idef = next_raise_count () in
          let def = cons_default matrix idef def in
          ( {
              me = Pm {cases = yes; args; default = def};
              matrix = as_matrix yes;
              top_default = def;
            },
            (idef, next) :: nexts )
      else
        let yes = List.rev yes in
        let {me = next; matrix; top_default = def}, nexts =
          split_noexc [cl] rem
        in
        let idef = next_raise_count () in
        let def = cons_default matrix idef def in
        ( {
            me = Pm {cases = yes; args; default = def};
            matrix = as_matrix yes;
            top_default = def;
          },
          (idef, next) :: nexts )
    | _ -> assert false
  and split_noexc yes = function
    | [] -> precompile_var args (List.rev yes) def k
    | ((p :: _, _) as cl) :: rem ->
      if group_constructor p then
        let yes = List.rev yes in
        let {me = next; matrix; top_default = def}, nexts =
          split_exc (pat_as_constr p) [cl] rem
        in
        let idef = next_raise_count () in
        precompile_var args yes
          (cons_default matrix idef def)
          ((idef, next) :: nexts)
      else split_noexc (cl :: yes) rem
    | _ -> assert false
  in

  match cls with
  | [] -> assert false
  | ((p :: _, _) as cl) :: rem ->
    if group_constructor p then split_exc (pat_as_constr p) [cl] rem
    else split_noexc [cl] rem
  | _ -> assert false

and split_constr cls args def k =
  let ex_pat = what_is_cases cls in
  match ex_pat.pat_desc with
  | Tpat_any -> precompile_var args cls def k
  | Tpat_construct (_, {cstr_tag = Cstr_extension _}, _) ->
    split_naive cls args def k
  | _ -> (
    let group = get_group ex_pat in

    let rec split_ex yes no = function
      | [] -> (
        let yes = List.rev yes and no = List.rev no in
        match no with
        | [] ->
          ( {
              me = Pm {cases = yes; args; default = def};
              matrix = as_matrix yes;
              top_default = def;
            },
            k )
        | cl :: rem -> (
          match yes with
          | [] ->
            (* Could not success in raising up a constr matching up *)
            split_noex [cl] [] rem
          | _ ->
            let {me = next; matrix; top_default = def}, nexts =
              split_noex [cl] [] rem
            in
            let idef = next_raise_count () in
            let def = cons_default matrix idef def in
            ( {
                me = Pm {cases = yes; args; default = def};
                matrix = as_matrix yes;
                top_default = def;
              },
              (idef, next) :: nexts )))
      | ((p :: _, _) as cl) :: rem ->
        if group p && up_ok cl no then split_ex (cl :: yes) no rem
        else split_ex yes (cl :: no) rem
      | _ -> assert false
    and split_noex yes no = function
      | [] -> (
        let yes = List.rev yes and no = List.rev no in
        match no with
        | [] -> precompile_var args yes def k
        | cl :: rem ->
          let {me = next; matrix; top_default = def}, nexts =
            split_ex [cl] [] rem
          in
          let idef = next_raise_count () in
          precompile_var args yes
            (cons_default matrix idef def)
            ((idef, next) :: nexts))
      | [((ps, _) as cl)] when List.for_all group_var ps && yes <> [] ->
        (* This enables an extra division in some frequent cases :
           last row is made of variables only *)
        split_noex yes (cl :: no) []
      | ((p :: _, _) as cl) :: rem ->
        if (not (group p)) && up_ok cl no then split_noex (cl :: yes) no rem
        else split_noex yes (cl :: no) rem
      | _ -> assert false
    in

    match cls with
    | ((p :: _, _) as cl) :: rem ->
      if group p then split_ex [cl] [] rem else split_noex [cl] [] rem
    | _ -> assert false)

and precompile_var args cls def k =
  match args with
  | [] -> assert false
  | _ :: (((Lvar v as av), _) as arg) :: rargs -> (
    match cls with
    | [_] ->
      (* as splitted as it can *)
      dont_precompile_var args cls def k
    | _ -> (
      (* Precompile *)
      let var_cls =
        List.map
          (fun (ps, act) ->
            match ps with
            | _ :: ps -> (ps, act)
            | _ -> assert false)
          cls
      and var_def = make_default (fun _ rem -> rem) def in
      let {me = first; matrix}, nexts =
        split_or (Some v) var_cls (arg :: rargs) var_def
      in

      (* Compute top information *)
      match nexts with
      | [] ->
        (* If you need *)
        dont_precompile_var args cls def k
      | _ ->
        let rfirst =
          {
            me = PmVar {inside = first; var_arg = av};
            matrix = add_omega_column matrix;
            top_default = rebuild_default nexts def;
          }
        and rnexts = rebuild_nexts av nexts k in
        (rfirst, rnexts)))
  | _ -> dont_precompile_var args cls def k

and dont_precompile_var args cls def k =
  ( {
      me = Pm {cases = cls; args; default = def};
      matrix = as_matrix cls;
      top_default = def;
    },
    k )

and precompile_or argo cls ors args def k =
  match ors with
  | [] -> split_constr cls args def k
  | _ ->
    let rec do_cases = function
      | (({pat_desc = Tpat_or _} as orp) :: patl, action) :: rem ->
        let others, rem = get_equiv orp rem in
        let orpm =
          {
            cases =
              (patl, action)
              :: List.map
                   (function
                     | _ :: ps, action -> (ps, action)
                     | _ -> assert false)
                   others;
            args =
              (match args with
              | _ :: r -> r
              | _ -> assert false);
            default = default_compat orp def;
          }
        in
        let vars =
          IdentSet.elements
            (IdentSet.inter
               (extract_vars IdentSet.empty orp)
               (pm_free_variables orpm))
        in
        let or_num = next_raise_count () in
        let new_patl = Parmatch.omega_list patl in

        let mk_new_action vs =
          Lstaticraise (or_num, List.map (fun v -> Lvar v) vs)
        in

        let body, handlers = do_cases rem in
        ( explode_or_pat argo new_patl mk_new_action body vars [] orp,
          let mat = [[orp]] in
          (mat, or_num, vars, orpm) :: handlers )
      | cl :: rem ->
        let new_ord, new_to_catch = do_cases rem in
        (cl :: new_ord, new_to_catch)
      | [] -> ([], [])
    in

    let end_body, handlers = do_cases ors in
    let matrix = as_matrix (cls @ ors)
    and body = {cases = cls @ end_body; args; default = def} in
    ( {me = PmOr {body; handlers; or_matrix = matrix}; matrix; top_default = def},
      k )

let split_precompile argo pm =
  let {me = next}, nexts = split_or argo pm.cases pm.args pm.default in
  if
    dbg
    && (nexts <> []
       ||
       match next with
       | PmOr _ -> true
       | _ -> false)
  then (
    prerr_endline "** SPLIT **";
    pretty_pm pm;
    pretty_precompiled_res next nexts);
  (next, nexts)

(* General divide functions *)

let add_line patl_action pm =
  pm.cases <- patl_action :: pm.cases;
  pm

type cell = {pm: pattern_matching; ctx: ctx list; pat: pattern}

let add make_matching_fun division eq_key key patl_action args =
  try
    let _, cell = List.find (fun (k, _) -> eq_key key k) division in
    cell.pm.cases <- patl_action :: cell.pm.cases;
    division
  with Not_found ->
    let cell = make_matching_fun args in
    cell.pm.cases <- [patl_action];
    (key, cell) :: division

let divide make eq_key get_key get_args ctx pm =
  let rec divide_rec = function
    | (p :: patl, action) :: rem ->
      let this_match = divide_rec rem in
      add (make p pm.default ctx) this_match eq_key (get_key p)
        (get_args p patl, action)
        pm.args
    | _ -> []
  in

  divide_rec pm.cases

let divide_line make_ctx make get_args pat ctx pm =
  let rec divide_rec = function
    | (p :: patl, action) :: rem ->
      let this_match = divide_rec rem in
      add_line (get_args p patl, action) this_match
    | _ -> make pm.default pm.args
  in

  {pm = divide_rec pm.cases; ctx = make_ctx ctx; pat}

(* Then come various functions,
   There is one set of functions per matching style
   (constants, constructors etc.)

   - matcher functions are arguments to make_default (for default handlers)
   They may raise NoMatch or OrPat and perform the full
   matching (selection + arguments).


   - get_args and get_key are for the compiled matrices, note that
   selection and getting arguments are separated.

   - make_ _matching combines the previous functions for producing
   new  ``pattern_matching'' records.
*)

let rec matcher_const cst p rem =
  match p.pat_desc with
  | Tpat_or (p1, p2, _) -> (
    try matcher_const cst p1 rem with NoMatch -> matcher_const cst p2 rem)
  | Tpat_constant c1 when const_compare c1 cst = 0 -> rem
  | Tpat_any -> rem
  | _ -> raise NoMatch

let get_key_constant caller = function
  | {pat_desc = Tpat_constant cst} -> cst
  | p ->
    prerr_endline ("BAD: " ^ caller);
    pretty_pat p;
    assert false

let get_args_constant _ rem = rem

let make_constant_matching p def ctx = function
  | [] -> fatal_error "Matching.make_constant_matching"
  | _ :: argl ->
    let def = make_default (matcher_const (get_key_constant "make" p)) def
    and ctx = filter_ctx p ctx in
    {pm = {cases = []; args = argl; default = def}; ctx; pat = normalize_pat p}

let divide_constant ctx m =
  divide make_constant_matching
    (fun c d -> const_compare c d = 0)
    (get_key_constant "divide")
    get_args_constant ctx m

(* Matching against a constructor *)

let make_field_args ~fld_info loc binding_kind arg first_pos last_pos argl =
  let rec make_args pos =
    if pos > last_pos then argl
    else
      (Lprim (Pfield (pos, fld_info), [arg], loc), binding_kind)
      :: make_args (pos + 1)
  in
  make_args first_pos

let get_key_constr = function
  | {pat_desc = Tpat_construct (_, cstr, _)} -> cstr.cstr_tag
  | _ -> assert false

let get_args_constr p rem =
  match p with
  | {pat_desc = Tpat_construct (_, _, args)} -> args @ rem
  | _ -> assert false

(* NB: matcher_constr applies to default matrices.

       In that context, matching by constructors of extensible
       types degrades to arity checking, due to potential rebinding.
       This comparison is performed by Types.may_equal_constr.
*)

let matcher_constr cstr =
  match cstr.cstr_arity with
  | 0 ->
    let rec matcher_rec q rem =
      match q.pat_desc with
      | Tpat_or (p1, p2, _) -> (
        try matcher_rec p1 rem with NoMatch -> matcher_rec p2 rem)
      | Tpat_construct (_, cstr', []) when Types.may_equal_constr cstr cstr' ->
        rem
      | Tpat_any -> rem
      | _ -> raise NoMatch
    in
    matcher_rec
  | 1 ->
    let rec matcher_rec q rem =
      match q.pat_desc with
      | Tpat_or (p1, p2, _) -> (
        let r1 = try Some (matcher_rec p1 rem) with NoMatch -> None
        and r2 = try Some (matcher_rec p2 rem) with NoMatch -> None in
        match (r1, r2) with
        | None, None -> raise NoMatch
        | Some r1, None -> r1
        | None, Some r2 -> r2
        | Some (a1 :: _), Some (a2 :: _) ->
          {a1 with pat_loc = Location.none; pat_desc = Tpat_or (a1, a2, None)}
          :: rem
        | _, _ -> assert false)
      | Tpat_construct (_, cstr', [arg]) when Types.may_equal_constr cstr cstr'
        ->
        arg :: rem
      | Tpat_any -> omega :: rem
      | _ -> raise NoMatch
    in
    matcher_rec
  | _ -> (
    fun q rem ->
      match q.pat_desc with
      | Tpat_or (_, _, _) -> raise OrPat
      | Tpat_construct (_, cstr', args) when Types.may_equal_constr cstr cstr'
        ->
        args @ rem
      | Tpat_any -> Parmatch.omegas cstr.cstr_arity @ rem
      | _ -> raise NoMatch)

let make_constr_matching p def ctx = function
  | [] -> fatal_error "Matching.make_constr_matching"
  | (arg, _mut) :: argl ->
    let cstr = pat_as_constr p in
    let untagged = Ast_untagged_variants.has_untagged cstr.cstr_attributes in
    let newargs =
      if cstr.cstr_inlined <> None || (untagged && cstr.cstr_args <> []) then
        (arg, Alias) :: argl
      else
        match cstr.cstr_tag with
        | Cstr_block _ when Datarepr.constructor_has_optional_shape cstr ->
          let from_option =
            match p.pat_desc with
            | Tpat_construct (_, _, [{pat_type; pat_env}])
              when Typeopt.type_cannot_contain_undefined pat_type pat_env ->
              Pval_from_option_not_nest
            | _ -> Pval_from_option
          in
          (Lprim (from_option, [arg], p.pat_loc), Alias) :: argl
        | Cstr_constant _ | Cstr_block _ ->
          make_field_args p.pat_loc Alias arg 0 (cstr.cstr_arity - 1) argl
            ~fld_info:(if cstr.cstr_name = "::" then Fld_cons else Fld_variant)
        | Cstr_unboxed -> (arg, Alias) :: argl
        | Cstr_extension _ ->
          make_field_args p.pat_loc Alias arg 1 cstr.cstr_arity argl
            ~fld_info:Fld_extension
    in
    {
      pm =
        {
          cases = [];
          args = newargs;
          default = make_default (matcher_constr cstr) def;
        };
      ctx = filter_ctx p ctx;
      pat = normalize_pat p;
    }

let divide_constructor ctx pm =
  divide make_constr_matching Types.equal_tag get_key_constr get_args_constr ctx
    pm

(* Matching against a variant *)

let rec matcher_variant_const lab p rem =
  match p.pat_desc with
  | Tpat_or (p1, p2, _) -> (
    try matcher_variant_const lab p1 rem
    with NoMatch -> matcher_variant_const lab p2 rem)
  | Tpat_variant (lab1, _, _) when lab1 = lab -> rem
  | Tpat_any -> rem
  | _ -> raise NoMatch

let make_variant_matching_constant p lab def ctx = function
  | [] -> fatal_error "Matching.make_variant_matching_constant"
  | _ :: argl ->
    let def = make_default (matcher_variant_const lab) def
    and ctx = filter_ctx p ctx in
    {pm = {cases = []; args = argl; default = def}; ctx; pat = normalize_pat p}

let matcher_variant_nonconst lab p rem =
  match p.pat_desc with
  | Tpat_or (_, _, _) -> raise OrPat
  | Tpat_variant (lab1, Some arg, _) when lab1 = lab -> arg :: rem
  | Tpat_any -> omega :: rem
  | _ -> raise NoMatch

let make_variant_matching_nonconst p lab def ctx = function
  | [] -> fatal_error "Matching.make_variant_matching_nonconst"
  | (arg, _mut) :: argl ->
    let def = make_default (matcher_variant_nonconst lab) def
    and ctx = filter_ctx p ctx in
    {
      pm =
        {
          cases = [];
          args =
            (Lprim (Pfield (1, Fld_poly_var_content), [arg], p.pat_loc), Alias)
            :: argl;
          default = def;
        };
      ctx;
      pat = normalize_pat p;
    }

let divide_variant row ctx {cases = cl; args = al; default = def} =
  let row = Btype.row_repr row in
  let rec divide = function
    | (({pat_desc = Tpat_variant (lab, pato, _)} as p) :: patl, action) :: rem
      -> (
      let variants = divide rem in
      if
        try Btype.row_field_repr (List.assoc lab row.row_fields) = Rabsent
        with Not_found -> true
      then variants
      else
        let tag = Btype.hash_variant lab in
        let ( = ) ((a : string), (b : Types.constructor_tag)) (c, d) =
          a = c && Types.equal_tag b d
        in
        match pato with
        | None ->
          add
            (make_variant_matching_constant p lab def ctx)
            variants ( = ) (lab, Cstr_constant tag) (patl, action) al
        | Some pat ->
          add
            (make_variant_matching_nonconst p lab def ctx)
            variants ( = ) (lab, Cstr_block tag)
            (pat :: patl, action)
            al)
    | _ -> []
  in
  divide cl

(*
  Three ``no-test'' cases
  *)

(* Matching against a variable *)

let get_args_var _ rem = rem

let make_var_matching def = function
  | [] -> fatal_error "Matching.make_var_matching"
  | _ :: argl ->
    {cases = []; args = argl; default = make_default get_args_var def}

let divide_var ctx pm =
  divide_line ctx_lshift make_var_matching get_args_var omega ctx pm

(* Matching and forcing a lazy value *)

let get_arg_lazy p rem =
  match p with
  | {pat_desc = Tpat_any} -> omega :: rem
  | {pat_desc = Tpat_lazy arg} -> arg :: rem
  | _ -> assert false

let matcher_lazy p rem =
  match p.pat_desc with
  | Tpat_or (_, _, _) -> raise OrPat
  | Tpat_any | Tpat_var _ -> omega :: rem
  | Tpat_lazy arg -> arg :: rem
  | _ -> raise NoMatch

(* Inlining the tag tests before calling the primitive that works on
   lazy blocks. This is also used in translcore.ml.
   No other call than Obj.tag when the value has been forced before.
*)

let get_mod_field modname field =
  lazy
    (try
       let mod_ident = Ident.create_persistent modname in
       let env = Env.open_pers_signature modname Env.initial_safe_string in
       let p =
         try
           match Env.lookup_value (Longident.Lident field) env with
           | Path.Pdot (_, _, i), _ -> i
           | _ ->
             fatal_error ("Primitive " ^ modname ^ "." ^ field ^ " not found.")
         with Not_found ->
           fatal_error ("Primitive " ^ modname ^ "." ^ field ^ " not found.")
       in
       Lprim
         ( Pfield (p, Fld_module {name = field}),
           [Lprim (Pgetglobal mod_ident, [], Location.none)],
           Location.none )
     with Not_found -> fatal_error ("Module " ^ modname ^ " unavailable."))

let code_force = get_mod_field Primitive_modules.lazy_ "force"

(* inline_lazy_force inlines the beginning of the code of Lazy.force. When
   the value argument is tagged as:
   - forward, take field 0
   - lazy, call the primitive that forces (without testing again the tag)
   - anything else, return it

   Using Lswitch below relies on the fact that the GC does not shortcut
   Forward(val_out_of_heap).
*)

let inline_lazy_force arg loc =
  Lapply
    {
      ap_func = Lazy.force code_force;
      ap_inlined = Default_inline;
      ap_args = [arg];
      ap_loc = loc;
    }
let make_lazy_matching def = function
  | [] -> fatal_error "Matching.make_lazy_matching"
  | (arg, _mut) :: argl ->
    {
      cases = [];
      args = (inline_lazy_force arg Location.none, Strict) :: argl;
      default = make_default matcher_lazy def;
    }

let divide_lazy p ctx pm =
  divide_line (filter_ctx p) make_lazy_matching get_arg_lazy p ctx pm

(* Matching against a tuple pattern *)

let get_args_tuple arity p rem =
  match p with
  | {pat_desc = Tpat_any} -> omegas arity @ rem
  | {pat_desc = Tpat_tuple args} -> args @ rem
  | _ -> assert false

let matcher_tuple arity p rem =
  match p.pat_desc with
  | Tpat_or (_, _, _) -> raise OrPat
  | Tpat_any | Tpat_var _ -> omegas arity @ rem
  | Tpat_tuple args when List.length args = arity -> args @ rem
  | _ -> raise NoMatch

let make_tuple_matching loc arity def = function
  | [] -> fatal_error "Matching.make_tuple_matching"
  | (arg, _mut) :: argl ->
    let rec make_args pos =
      if pos >= arity then argl
      else
        (Lprim (Pfield (pos, Fld_tuple), [arg], loc), Alias)
        :: make_args (pos + 1)
    in
    {
      cases = [];
      args = make_args 0;
      default = make_default (matcher_tuple arity) def;
    }

let divide_tuple arity p ctx pm =
  divide_line (filter_ctx p)
    (make_tuple_matching p.pat_loc arity)
    (get_args_tuple arity) p ctx pm

(* Matching against a record pattern *)

let record_matching_line num_fields lbl_pat_list =
  let patv = Array.make num_fields omega in
  List.iter (fun (_, lbl, pat, _) -> patv.(lbl.lbl_pos) <- pat) lbl_pat_list;
  Array.to_list patv

let get_args_record num_fields p rem =
  match p with
  | {pat_desc = Tpat_any} -> record_matching_line num_fields [] @ rem
  | {pat_desc = Tpat_record (lbl_pat_list, _)} ->
    record_matching_line num_fields lbl_pat_list @ rem
  | _ -> assert false

let matcher_record num_fields p rem =
  match p.pat_desc with
  | Tpat_or (_, _, _) -> raise OrPat
  | Tpat_any | Tpat_var _ -> record_matching_line num_fields [] @ rem
  | Tpat_record ([], _) when num_fields = 0 -> rem
  | Tpat_record (((_, lbl, _, _) :: _ as lbl_pat_list), _)
    when Array.length lbl.lbl_all = num_fields ->
    record_matching_line num_fields lbl_pat_list @ rem
  | _ -> raise NoMatch

let make_record_matching loc all_labels def = function
  | [] -> fatal_error "Matching.make_record_matching"
  | (arg, _mut) :: argl ->
    let rec make_args pos =
      if pos >= Array.length all_labels then argl
      else
        let lbl = all_labels.(pos) in
        let access =
          match lbl.lbl_repres with
          | Record_float_unused -> assert false
          | Record_regular ->
            Lprim (Pfield (lbl.lbl_pos, Lambda.fld_record lbl), [arg], loc)
          | Record_inlined _ ->
            Lprim
              (Pfield (lbl.lbl_pos, Lambda.fld_record_inline lbl), [arg], loc)
          | Record_unboxed _ -> arg
          | Record_extension ->
            Lprim
              ( Pfield (lbl.lbl_pos + 1, Lambda.fld_record_extension lbl),
                [arg],
                loc )
        in
        let str =
          match lbl.lbl_mut with
          | Immutable -> Alias
          | Mutable -> StrictOpt
        in
        (access, str) :: make_args (pos + 1)
    in
    let nfields = Array.length all_labels in
    let def = make_default (matcher_record nfields) def in
    {cases = []; args = make_args 0; default = def}

let divide_record all_labels p ctx pm =
  let get_args = get_args_record (Array.length all_labels) in
  divide_line (filter_ctx p)
    (make_record_matching p.pat_loc all_labels)
    get_args p ctx pm

(* Matching against an array pattern *)

let get_key_array = function
  | {pat_desc = Tpat_array patl} -> List.length patl
  | _ -> assert false

let get_args_array p rem =
  match p with
  | {pat_desc = Tpat_array patl} -> patl @ rem
  | _ -> assert false

let matcher_array len p rem =
  match p.pat_desc with
  | Tpat_or (_, _, _) -> raise OrPat
  | Tpat_array args when List.length args = len -> args @ rem
  | Tpat_any -> Parmatch.omegas len @ rem
  | _ -> raise NoMatch

let make_array_matching p def ctx = function
  | [] -> fatal_error "Matching.make_array_matching"
  | (arg, _mut) :: argl ->
    let len = get_key_array p in
    let rec make_args pos =
      if pos >= len then argl
      else
        ( Lprim
            (Parrayrefu, [arg; Lconst (Const_base (Const_int pos))], p.pat_loc),
          StrictOpt )
        :: make_args (pos + 1)
    in
    let def = make_default (matcher_array len) def and ctx = filter_ctx p ctx in
    {
      pm = {cases = []; args = make_args 0; default = def};
      ctx;
      pat = normalize_pat p;
    }

let divide_array ctx pm =
  divide make_array_matching ( = ) get_key_array get_args_array ctx pm

(*
   Specific string test sequence
   Will be called by the bytecode compiler, from bytegen.ml.
   The strategy is first dichotomic search (we perform 3-way tests
   with compare_string), then sequence of equality tests
   when there are less then T=strings_test_threshold static strings to match.

  Increasing T entails (slightly) less code, decreasing T
  (slightly) favors runtime speed.
  T=8 looks a decent tradeoff.
*)

(* Utilities *)

let strings_test_threshold = 8

let bind_sw arg k =
  match arg with
  | Lvar _ -> k arg
  | _ ->
    let id = Ident.create "switch" in
    Llet (Strict, Pgenval, id, arg, k (Lvar id))

(* Sequential equality tests *)

let make_string_test_sequence loc arg sw d =
  let d, sw =
    match d with
    | None -> (
      match sw with
      | (_, d) :: sw -> (d, sw)
      | [] -> assert false)
    | Some d -> (d, sw)
  in
  bind_sw arg (fun arg ->
      List.fold_right
        (fun (s, lam) k ->
          Lifthenelse
            ( Lprim (Pstringcomp Cneq, [arg; Lconst (Const_immstring s)], loc),
              k,
              lam ))
        sw d)

let rec split k xs =
  match xs with
  | [] -> assert false
  | x0 :: xs ->
    if k <= 1 then ([], x0, xs)
    else
      let xs, y0, ys = split (k - 2) xs in
      (x0 :: xs, y0, ys)

let zero_lam = Lconst (Const_base (Const_int 0))

let tree_way_test loc arg lt eq gt =
  Lifthenelse
    ( Lprim (Pintcomp Clt, [arg; zero_lam], loc),
      lt,
      Lifthenelse (Lprim (Pintcomp Clt, [zero_lam; arg], loc), gt, eq) )

(* Dichotomic tree *)

let rec do_make_string_test_tree loc arg sw delta d =
  let len = List.length sw in
  if len <= strings_test_threshold + delta then
    make_string_test_sequence loc arg sw d
  else
    let lt, (s, act), gt = split len sw in
    bind_sw
      (Lprim (Pstringcomp Ceq, [arg; Lconst (Const_immstring s)], loc))
      (fun r ->
        tree_way_test loc r
          (do_make_string_test_tree loc arg lt delta d)
          act
          (do_make_string_test_tree loc arg gt delta d))

(* Entry point *)
let expand_stringswitch loc arg sw d =
  match d with
  | None -> bind_sw arg (fun arg -> do_make_string_test_tree loc arg sw 0 None)
  | Some e ->
    bind_sw arg (fun arg ->
        make_catch e (fun d -> do_make_string_test_tree loc arg sw 1 (Some d)))

(**********************)
(* Generic test trees *)
(**********************)

(* Sharing *)

(* Add handler, if shared *)
let handle_shared () =
  let hs = ref (fun x -> x) in
  let handle_shared act =
    match act with
    | Switch.Single act -> act
    | Switch.Shared act ->
      let i, h = make_catch_delayed act in
      let ohs = !hs in
      (hs := fun act -> h (ohs act));
      make_exit i
  in
  (hs, handle_shared)

let share_actions_tree sw d =
  let store = StoreExp.mk_store () in
  (* Default action is always shared *)
  let d =
    match d with
    | None -> None
    | Some d -> Some (store.Switch.act_store_shared d)
  in
  (* Store all other actions *)
  let sw = List.map (fun (cst, act) -> (cst, store.Switch.act_store act)) sw in

  (* Retrieve all actions, including potential default *)
  let acts = store.Switch.act_get_shared () in

  (* Array of actual actions *)
  let hs, handle_shared = handle_shared () in
  let acts = Array.map handle_shared acts in

  (* Reconstruct default and switch list *)
  let d =
    match d with
    | None -> None
    | Some d -> Some acts.(d)
  in
  let sw = List.map (fun (cst, j) -> (cst, acts.(j))) sw in
  (!hs, sw, d)

(* Note: dichotomic search requires sorted input with no duplicates *)
let rec uniq_lambda_list sw =
  match sw with
  | [] | [_] -> sw
  | ((c1, _) as p1) :: ((c2, _) :: sw2 as sw1) ->
    if const_compare c1 c2 = 0 then uniq_lambda_list (p1 :: sw2)
    else p1 :: uniq_lambda_list sw1

let sort_lambda_list l =
  let l = List.stable_sort (fun (x, _) (y, _) -> const_compare x y) l in
  uniq_lambda_list l

let rec cut n l =
  if n = 0 then ([], l)
  else
    match l with
    | [] -> raise (Invalid_argument "cut")
    | a :: l ->
      let l1, l2 = cut (n - 1) l in
      (a :: l1, l2)

let rec do_tests_fail loc fail tst arg = function
  | [] -> fail
  | (c, act) :: rem ->
    Lifthenelse
      ( Lprim (tst, [arg; Lconst (Const_base c)], loc),
        do_tests_fail loc fail tst arg rem,
        act )

let rec do_tests_nofail loc tst arg = function
  | [] -> fatal_error "Matching.do_tests_nofail"
  | [(_, act)] -> act
  | (c, act) :: rem ->
    Lifthenelse
      ( Lprim (tst, [arg; Lconst (Const_base c)], loc),
        do_tests_nofail loc tst arg rem,
        act )

let make_test_sequence loc fail tst lt_tst arg const_lambda_list =
  let const_lambda_list = sort_lambda_list const_lambda_list in
  let hs, const_lambda_list, fail = share_actions_tree const_lambda_list fail in

  let rec make_test_sequence const_lambda_list =
    if List.length const_lambda_list >= 4 && lt_tst <> Pignore then
      split_sequence const_lambda_list
    else
      match fail with
      | None -> do_tests_nofail loc tst arg const_lambda_list
      | Some fail -> do_tests_fail loc fail tst arg const_lambda_list
  and split_sequence const_lambda_list =
    let list1, list2 =
      cut (List.length const_lambda_list / 2) const_lambda_list
    in
    Lifthenelse
      ( Lprim (lt_tst, [arg; Lconst (Const_base (fst (List.hd list2)))], loc),
        make_test_sequence list1,
        make_test_sequence list2 )
  in
  hs (make_test_sequence const_lambda_list)

module SArg = struct
  type primitive = Lambda.primitive

  let eqint = Pintcomp Ceq
  let neint = Pintcomp Cneq
  let leint = Pintcomp Cle
  let ltint = Pintcomp Clt
  let geint = Pintcomp Cge
  let gtint = Pintcomp Cgt

  type act = Lambda.lambda

  let make_prim p args = Lprim (p, args, Location.none)
  let make_offset arg n =
    match n with
    | 0 -> arg
    | _ -> Lprim (Poffsetint n, [arg], Location.none)

  let bind arg body =
    let newvar, newarg =
      match arg with
      | Lvar v -> (v, arg)
      | _ ->
        let newvar = Ident.create "switcher" in
        (newvar, Lvar newvar)
    in
    bind Alias newvar arg (body newarg)
  let make_const i = Lconst (Const_base (Const_int i))
  let make_isout h arg = Lprim (Pisout, [h; arg], Location.none)
  let make_isin h arg = Lprim (Pnot, [make_isout h arg], Location.none)
  let make_if cond ifso ifnot = Lifthenelse (cond, ifso, ifnot)
  let make_switch loc arg cases acts ~offset sw_names =
    let l = ref [] in
    for i = Array.length cases - 1 downto 0 do
      l := (offset + i, acts.(cases.(i))) :: !l
    done;
    Lswitch
      ( arg,
        {
          sw_numconsts = Array.length cases;
          sw_consts = !l;
          sw_numblocks = 0;
          sw_blocks = [];
          sw_failaction = None;
          sw_names;
        },
        loc )
  let make_catch = make_catch_delayed
  let make_exit = make_exit
end

(* Action sharing for Lswitch argument *)
let share_actions_sw sw =
  (* Attempt sharing on all actions *)
  let store = StoreExp.mk_store () in
  let fail =
    match sw.sw_failaction with
    | None -> None
    | Some fail ->
      (* Fail is translated to exit, whatever happens *)
      Some (store.Switch.act_store_shared fail)
  in
  let consts =
    List.map (fun (i, e) -> (i, store.Switch.act_store e)) sw.sw_consts
  and blocks =
    List.map (fun (i, e) -> (i, store.Switch.act_store e)) sw.sw_blocks
  in
  let acts = store.Switch.act_get_shared () in
  let hs, handle_shared = handle_shared () in
  let acts = Array.map handle_shared acts in
  let fail =
    match fail with
    | None -> None
    | Some fail -> Some acts.(fail)
  in
  ( !hs,
    {
      sw with
      sw_consts = List.map (fun (i, j) -> (i, acts.(j))) consts;
      sw_blocks = List.map (fun (i, j) -> (i, acts.(j))) blocks;
      sw_failaction = fail;
    } )

(* Reintroduce fail action in switch argument,
   for the sake of avoiding carrying over huge switches *)

let reintroduce_fail sw =
  match sw.sw_failaction with
  | None ->
    let t = Hashtbl.create 17 in
    let seen (_, l) =
      match as_simple_exit l with
      | Some i ->
        let old = try Hashtbl.find t i with Not_found -> 0 in
        Hashtbl.replace t i (old + 1)
      | None -> ()
    in
    List.iter seen sw.sw_consts;
    List.iter seen sw.sw_blocks;
    let i_max = ref (-1) and max = ref (-1) in
    Hashtbl.iter
      (fun i c ->
        if c > !max then (
          i_max := i;
          max := c))
      t;
    if !max >= 3 then
      let default = !i_max in
      let remove ls =
        Ext_list.filter ls (fun (_, lam) ->
            match as_simple_exit lam with
            | Some j -> j <> default
            | None -> true)
      in
      {
        sw with
        sw_consts = remove sw.sw_consts;
        sw_blocks = remove sw.sw_blocks;
        sw_failaction = Some (make_exit default);
      }
    else sw
  | Some _ -> sw

module Switcher = Switch.Make (SArg)
open Switch

let rec last def = function
  | [] -> def
  | [(x, _)] -> x
  | _ :: rem -> last def rem

let get_edges low high l =
  match l with
  | [] -> (low, high)
  | (x, _) :: _ -> (x, last high l)

let as_interval_canfail fail low high l =
  let store = StoreExp.mk_store () in

  let do_store _tag act =
    let i = store.act_store act in
    (*
    eprintf "STORE [%s] %i %s\n" tag i (string_of_lam act) ;
*)
    i
  in

  let rec nofail_rec cur_low cur_high cur_act = function
    | [] ->
      if cur_high = high then [(cur_low, cur_high, cur_act)]
      else [(cur_low, cur_high, cur_act); (cur_high + 1, high, 0)]
    | (i, act_i) :: rem as all ->
      let act_index = do_store "NO" act_i in
      if cur_high + 1 = i then
        if act_index = cur_act then nofail_rec cur_low i cur_act rem
        else if act_index = 0 then (cur_low, i - 1, cur_act) :: fail_rec i i rem
        else (cur_low, i - 1, cur_act) :: nofail_rec i i act_index rem
      else if act_index = 0 then
        (cur_low, cur_high, cur_act)
        :: fail_rec (cur_high + 1) (cur_high + 1) all
      else
        (cur_low, cur_high, cur_act)
        :: (cur_high + 1, i - 1, 0)
        :: nofail_rec i i act_index rem
  and fail_rec cur_low cur_high = function
    | [] -> [(cur_low, cur_high, 0)]
    | (i, act_i) :: rem ->
      let index = do_store "YES" act_i in
      if index = 0 then fail_rec cur_low i rem
      else (cur_low, i - 1, 0) :: nofail_rec i i index rem
  in

  let init_rec = function
    | [] -> [(low, high, 0)]
    | (i, act_i) :: rem ->
      let index = do_store "INIT" act_i in
      if index = 0 then fail_rec low i rem
      else if low < i then (low, i - 1, 0) :: nofail_rec i i index rem
      else nofail_rec i i index rem
  in

  assert (do_store "FAIL" fail = 0);
  (* fail has action index 0 *)
  let r = init_rec l in
  (Array.of_list r, store)

let as_interval_nofail l =
  let store = StoreExp.mk_store () in
  let rec some_hole = function
    | [] | [_] -> false
    | (i, _) :: ((j, _) :: _ as rem) -> j > i + 1 || some_hole rem
  in
  let rec i_rec cur_low cur_high cur_act = function
    | [] -> [(cur_low, cur_high, cur_act)]
    | (i, act) :: rem ->
      let act_index = store.act_store act in
      if act_index = cur_act then i_rec cur_low i cur_act rem
      else (cur_low, cur_high, cur_act) :: i_rec i i act_index rem
  in
  let inters =
    match l with
    | (i, act) :: rem ->
      let act_index =
        (* In case there is some hole and that a switch is emitted,
           action 0 will be used as the action of unreachable
           cases (cf. switch.ml, make_switch).
           Hence, this action will be shared *)
        if some_hole rem then store.act_store_shared act
        else store.act_store act
      in
      assert (act_index = 0);
      i_rec i i act_index rem
    | _ -> assert false
  in

  (Array.of_list inters, store)

let sort_int_lambda_list l =
  List.sort
    (fun (i1, _) (i2, _) -> if i1 < i2 then -1 else if i2 < i1 then 1 else 0)
    l

let as_interval fail low high l =
  let l = sort_int_lambda_list l in
  ( get_edges low high l,
    match fail with
    | None -> as_interval_nofail l
    | Some act -> as_interval_canfail act low high l )

let call_switcher loc fail arg low high int_lambda_list sw_names =
  let edges, (cases, actions) = as_interval fail low high int_lambda_list in
  Switcher.zyva loc edges arg cases actions sw_names

let rec list_as_pat = function
  | [] -> fatal_error "Matching.list_as_pat"
  | [pat] -> pat
  | pat :: rem -> {pat with pat_desc = Tpat_or (pat, list_as_pat rem, None)}

let complete_pats_constrs = function
  | p :: _ as pats ->
    List.map (pat_of_constr p)
      (complete_constrs p (List.map get_key_constr pats))
  | _ -> assert false

(*
     Following two ``failaction'' function compute n, the trap handler
    to jump to in case of failure of elementary tests
*)

let mk_failaction_neg partial ctx def =
  match partial with
  | Partial -> (
    match def with
    | (_, idef) :: _ ->
      (Some (Lstaticraise (idef, [])), jumps_singleton idef ctx)
    | [] ->
      (* Act as Total, this means
         If no appropriate default matrix exists,
         then this switch cannot fail *)
      (None, jumps_empty))
  | Total -> (None, jumps_empty)

(* In line with the article and simpler than before *)
let mk_failaction_pos partial seen ctx defs =
  if dbg then (
    prerr_endline "**POS**";
    pretty_def defs;
    ());
  let rec scan_def env to_test defs =
    match (to_test, defs) with
    | [], _ | _, [] ->
      List.fold_left
        (fun (klist, jumps) (pats, i) ->
          let action = Lstaticraise (i, []) in
          let klist =
            List.fold_right
              (fun pat r -> (get_key_constr pat, action) :: r)
              pats klist
          and jumps = jumps_add i (ctx_lub (list_as_pat pats) ctx) jumps in
          (klist, jumps))
        ([], jumps_empty) env
    | _, (pss, idef) :: rem -> (
      let now, later =
        List.partition (fun (_p, p_ctx) -> ctx_match p_ctx pss) to_test
      in
      match now with
      | [] -> scan_def env to_test rem
      | _ -> scan_def ((List.map fst now, idef) :: env) later rem)
  in

  let fail_pats = complete_pats_constrs seen in
  if List.length fail_pats < 32 then (
    let fail, jmps =
      scan_def [] (List.map (fun pat -> (pat, ctx_lub pat ctx)) fail_pats) defs
    in
    if dbg then (
      eprintf "POSITIVE JUMPS [%i]:\n" (List.length fail_pats);
      pretty_jumps jmps);
    (None, fail, jmps))
  else (
    (* Too many non-matched constructors -> reduced information *)
    if dbg then eprintf "POS->NEG!!!\n%!";
    let fail, jumps = mk_failaction_neg partial ctx defs in
    if dbg then
      eprintf "FAIL: %s\n"
        (match fail with
        | None -> "<none>"
        | Some lam -> string_of_lam lam);
    (fail, [], jumps))

let combine_constant names loc arg cst partial ctx def
    (const_lambda_list, total, _pats) =
  let fail, local_jumps = mk_failaction_neg partial ctx def in
  let lambda1 =
    match cst with
    | Const_int _ ->
      let int_lambda_list =
        List.map
          (function
            | Const_int n, l -> (n, l)
            | _ -> assert false)
          const_lambda_list
      in
      call_switcher loc fail arg min_int max_int int_lambda_list names
    | Const_char _ ->
      let int_lambda_list =
        List.map
          (function
            | Const_char c, l -> (c, l)
            | _ -> assert false)
          const_lambda_list
      in
      call_switcher loc fail arg 0 max_int int_lambda_list names
    | Const_string _ ->
      (* Note as the bytecode compiler may resort to dichotomic search,
         the clauses of stringswitch  are sorted with duplicates removed.
         This partly applies to the native code compiler, which requires
         no duplicates *)
      let const_lambda_list = sort_lambda_list const_lambda_list in
      let sw =
        List.map
          (fun (c, act) ->
            match c with
            | Const_string (s, _) -> (s, act)
            | _ -> assert false)
          const_lambda_list
      in
      let hs, sw, fail = share_actions_tree sw fail in
      hs (Lstringswitch (arg, sw, fail, loc))
    | Const_float _ ->
      make_test_sequence loc fail (Pfloatcomp Cneq) (Pfloatcomp Clt) arg
        const_lambda_list
    | Const_int32 _ -> assert false
    | Const_int64 _ -> assert false
    | Const_bigint _ ->
      make_test_sequence loc fail (Pbigintcomp Cneq) (Pbigintcomp Clt) arg
        const_lambda_list
  in
  (lambda1, jumps_union local_jumps total)

let split_cases tag_lambda_list =
  let rec split_rec = function
    | [] -> ([], [])
    | (cstr, act) :: rem -> (
      let consts, nonconsts = split_rec rem in
      match cstr with
      | Cstr_constant n -> ((n, act) :: consts, nonconsts)
      | Cstr_block n -> (consts, (n, act) :: nonconsts)
      | Cstr_unboxed -> (consts, (0, act) :: nonconsts)
      | Cstr_extension _ -> assert false)
  in
  let const, nonconst = split_rec tag_lambda_list in
  (sort_int_lambda_list const, sort_int_lambda_list nonconst)

(* refine [split_cases] and [split_variant_cases] *)
let split_variant_cases tag_lambda_list =
  let rec split_rec = function
    | [] -> ([], [])
    | ((name, cstr), act) :: rem -> (
      let consts, nonconsts = split_rec rem in
      match cstr with
      | Cstr_constant n -> ((n, (name, act)) :: consts, nonconsts)
      | Cstr_block n -> (consts, (n, (name, act)) :: nonconsts)
      | Cstr_unboxed -> assert false
      | Cstr_extension _ -> assert false)
  in
  let const, nonconst = split_rec tag_lambda_list in
  (sort_int_lambda_list const, sort_int_lambda_list nonconst)

let get_extension_cases tag_lambda_list =
  let rec split_rec = function
    | [] -> []
    | (cstr, act) :: rem -> (
      let nonconsts = split_rec rem in
      match cstr with
      | Cstr_extension path -> (path, act) :: nonconsts
      | _ -> assert false)
  in
  split_rec tag_lambda_list

let combine_constructor sw_names loc arg ex_pat cstr partial ctx def
    (tag_lambda_list, total1, pats) =
  if cstr.cstr_consts < 0 then
    (* Special cases for extensions *)
    let fail, local_jumps = mk_failaction_neg partial ctx def in
    let lambda1 =
      let extension_cases = get_extension_cases tag_lambda_list in
      let default, extension_cases =
        match fail with
        | None -> (
          match extension_cases with
          | (_, act) :: rem -> (act, rem)
          | _ -> assert false)
        | Some fail -> (fail, extension_cases)
      in
      match extension_cases with
      | [] -> default
      | _ ->
        let tag = Ident.create "tag" in
        let tests =
          List.fold_right
            (fun (path, act) rem ->
              let ext = transl_extension_path ex_pat.pat_env path in
              Lifthenelse
                (Lprim (Pextension_slot_eq, [Lvar tag; ext], loc), act, rem))
            extension_cases default
        in
        Llet (Alias, Pgenval, tag, arg, tests)
    in
    (lambda1, jumps_union local_jumps total1)
  else
    (* Regular concrete type *)
    let ncases = List.length tag_lambda_list
    and nconstrs = cstr.cstr_consts + cstr.cstr_nonconsts in
    let sig_complete = ncases = nconstrs in
    let fail_opt, fails, local_jumps =
      if sig_complete then (None, [], jumps_empty)
      else mk_failaction_pos partial pats ctx def
    in

    let tag_lambda_list = fails @ tag_lambda_list in
    let consts, nonconsts = split_cases tag_lambda_list in
    let lambda1 =
      match (fail_opt, same_actions tag_lambda_list) with
      | None, Some act -> act (* Identical actions, no failure *)
      | _ -> (
        match (cstr.cstr_consts, cstr.cstr_nonconsts, consts, nonconsts) with
        | 1, 1, [(0, act1)], [(0, act2)]
          when cstr.cstr_name = "::" || cstr.cstr_name = "[]"
               || Datarepr.constructor_has_optional_shape cstr ->
          (* Typically, match on lists, will avoid isint primitive in that
             case *)
          let arg =
            if Datarepr.constructor_has_optional_shape cstr then
              Lprim (Pis_not_none, [arg], loc)
            else
              Lprim (Pjscomp Cneq, [arg; Lconst (Const_base (Const_int 0))], loc)
          in
          Lifthenelse (arg, act2, act1)
        | 2, 0, [(i1, act1); (_, act2)], []
          when cstr.cstr_name = "true" || cstr.cstr_name = "false" ->
          if i1 = 0 then Lifthenelse (arg, act2, act1)
          else Lifthenelse (arg, act1, act2)
        | n, 0, _, [] when false (* relies on tag being an int *) ->
          (* The type defines constant constructors only *)
          call_switcher loc fail_opt arg 0 (n - 1) consts sw_names
        | n, _, _, _ -> (
          let act0 =
            (* = Some act when all non-const constructors match to act *)
            match (fail_opt, nonconsts) with
            | Some a, [] -> Some a
            | Some _, _ ->
              if List.length nonconsts = cstr.cstr_nonconsts then
                same_actions nonconsts
              else None
            | None, _ -> same_actions nonconsts
          in
          match act0 with
          | Some act when false (* relies on tag being an int *) ->
            Lifthenelse
              ( Lprim (Pisint, [arg], loc),
                call_switcher loc fail_opt arg 0 (n - 1) consts sw_names,
                act )
          (* Emit a switch, as bytecode implements this sophisticated instruction *)
          | _ ->
            let sw =
              {
                sw_numconsts = cstr.cstr_consts;
                sw_consts = consts;
                sw_numblocks = cstr.cstr_nonconsts;
                sw_blocks = nonconsts;
                sw_failaction = fail_opt;
                sw_names;
              }
            in
            let hs, sw = share_actions_sw sw in
            let sw = reintroduce_fail sw in
            hs (Lswitch (arg, sw, loc))))
    in
    (lambda1, jumps_union local_jumps total1)

let make_test_sequence_variant_constant fail arg int_lambda_list =
  let _, (cases, actions) =
    as_interval fail min_int max_int
      (List.map (fun (a, (_, c)) -> (a, c)) int_lambda_list)
  in
  Switcher.test_sequence arg cases actions

let call_switcher_variant_constant loc fail arg int_lambda_list names =
  call_switcher loc fail arg min_int max_int
    (List.map (fun (a, (_, c)) -> (a, c)) int_lambda_list)
    names

let call_switcher_variant_constr loc fail arg int_lambda_list names =
  let v = Ident.create "variant" in
  Llet
    ( Alias,
      Pgenval,
      v,
      Lprim (Pfield (0, Fld_poly_var_tag), [arg], loc),
      call_switcher loc fail (Lvar v) min_int max_int
        (List.map (fun (a, (_, c)) -> (a, c)) int_lambda_list)
        names )

let call_switcher_variant_constant :
    (Location.t ->
    Lambda.lambda option ->
    Lambda.lambda ->
    (int * (string * Lambda.lambda)) list ->
    Ast_untagged_variants.switch_names option ->
    Lambda.lambda)
    ref =
  ref call_switcher_variant_constant

let call_switcher_variant_constr :
    (Location.t ->
    Lambda.lambda option ->
    Lambda.lambda ->
    (int * (string * Lambda.lambda)) list ->
    Ast_untagged_variants.switch_names option ->
    Lambda.lambda)
    ref =
  ref call_switcher_variant_constr

let make_test_sequence_variant_constant :
    (Lambda.lambda option ->
    Lambda.lambda ->
    (int * (string * Lambda.lambda)) list ->
    Lambda.lambda)
    ref =
  ref make_test_sequence_variant_constant

let combine_variant names loc row arg partial ctx def
    (tag_lambda_list, total1, _pats) =
  let row = Btype.row_repr row in
  let num_constr = ref 0 in
  if row.row_closed then
    List.iter
      (fun (_, f) ->
        match Btype.row_field_repr f with
        | Rabsent | Reither (true, _ :: _, _, _) -> ()
        | _ -> incr num_constr)
      row.row_fields
  else num_constr := max_int;
  let test_int_or_block arg if_int if_block =
    Lifthenelse (Lprim (Pis_poly_var_block, [arg], loc), if_block, if_int)
  in
  let sig_complete = List.length tag_lambda_list = !num_constr
  and one_action = same_actions tag_lambda_list in
  (* reduandant work under bs context *)
  let fail, local_jumps =
    if
      sig_complete
      ||
      match partial with
      | Total -> true
      | _ -> false
    then (None, jumps_empty)
    else mk_failaction_neg partial ctx def
  in
  let consts, nonconsts = split_variant_cases tag_lambda_list in
  let lambda1 =
    match (fail, one_action) with
    | None, Some act -> act
    | _, _ -> (
      match (consts, nonconsts) with
      | [(_, (_, act1))], [(_, (_, act2))] when fail = None ->
        test_int_or_block arg act1 act2
      | _, [] ->
        (* One can compare integers and pointers *)
        !make_test_sequence_variant_constant fail arg consts
      | [], _ -> (
        let lam = !call_switcher_variant_constr loc fail arg nonconsts names in
        (* One must not dereference integers *)
        match fail with
        | None -> lam
        | Some fail -> test_int_or_block arg fail lam)
      | _, _ ->
        let lam_const =
          !call_switcher_variant_constant loc fail arg consts names
        and lam_nonconst =
          !call_switcher_variant_constr loc fail arg nonconsts names
        in
        test_int_or_block arg lam_const lam_nonconst)
  in
  (lambda1, jumps_union local_jumps total1)

let combine_array names loc arg partial ctx def (len_lambda_list, total1, _pats)
    =
  let fail, local_jumps = mk_failaction_neg partial ctx def in
  let lambda1 =
    let newvar = Ident.create "len" in
    let switch =
      call_switcher loc fail (Lvar newvar) 0 max_int len_lambda_list names
    in
    bind Alias newvar (Lprim (Parraylength, [arg], loc)) switch
  in
  (lambda1, jumps_union local_jumps total1)

(* Insertion of debugging events *)

let[@inline] event_branch _repr lam = lam

(*
   This exception is raised when the compiler cannot produce code
   because control cannot reach the compiled clause,

   Unused is raised initially in compile_test.

   compile_list (for compiling switch results) catch Unused

   comp_match_handlers (for compiling splitted matches)
   may reraise Unused


*)

exception Unused

let compile_list compile_fun division =
  let rec c_rec totals = function
    | [] -> ([], jumps_unions totals, [])
    | (key, cell) :: rem -> (
      match cell.ctx with
      | [] -> c_rec totals rem
      | _ -> (
        try
          let lambda1, total1 = compile_fun cell.ctx cell.pm in
          let c_rem, total, new_pats =
            c_rec (jumps_map ctx_combine total1 :: totals) rem
          in
          ((key, lambda1) :: c_rem, total, cell.pat :: new_pats)
        with Unused -> c_rec totals rem))
  in
  c_rec [] division

let compile_orhandlers compile_fun lambda1 total1 ctx to_catch =
  let rec do_rec r total_r = function
    | [] -> (r, total_r)
    | (mat, i, vars, pm) :: rem -> (
      try
        let ctx = select_columns mat ctx in
        let handler_i, total_i = compile_fun ctx pm in
        match raw_action r with
        | Lstaticraise (j, args) ->
          if i = j then
            ( List.fold_right2 (bind Alias) vars args handler_i,
              jumps_map (ctx_rshift_num (ncols mat)) total_i )
          else do_rec r total_r rem
        | _ ->
          do_rec
            (Lstaticcatch (r, (i, vars), handler_i))
            (jumps_union (jumps_remove i total_r)
               (jumps_map (ctx_rshift_num (ncols mat)) total_i))
            rem
      with Unused ->
        do_rec (Lstaticcatch (r, (i, vars), lambda_unit)) total_r rem)
  in
  do_rec lambda1 total1 to_catch

let compile_test compile_fun partial divide combine ctx to_match =
  let division = divide ctx to_match in
  let c_div = compile_list compile_fun division in
  match c_div with
  | [], _, _ -> (
    match mk_failaction_neg partial ctx to_match.default with
    | None, _ -> raise Unused
    | Some l, total -> (l, total))
  | _ -> combine ctx to_match.default c_div

(* Attempt to avoid some useless bindings by lowering them *)

(* Approximation of v present in lam *)
let rec approx_present v = function
  | Lconst _ -> false
  | Lstaticraise (_, args) -> List.exists (fun lam -> approx_present v lam) args
  | Lprim (_, args, _) -> List.exists (fun lam -> approx_present v lam) args
  | Llet (Alias, _k, _, l1, l2) -> approx_present v l1 || approx_present v l2
  | Lvar vv -> Ident.same v vv
  | _ -> true

let rec lower_bind v arg lam =
  match lam with
  | Lifthenelse (cond, ifso, ifnot) -> (
    let pcond = approx_present v cond
    and pso = approx_present v ifso
    and pnot = approx_present v ifnot in
    match (pcond, pso, pnot) with
    | false, false, false -> lam
    | false, true, false -> Lifthenelse (cond, lower_bind v arg ifso, ifnot)
    | false, false, true -> Lifthenelse (cond, ifso, lower_bind v arg ifnot)
    | _, _, _ -> bind Alias v arg lam)
  | Lswitch (ls, ({sw_consts = [(i, act)]; sw_blocks = []} as sw), loc)
    when not (approx_present v ls) ->
    Lswitch (ls, {sw with sw_consts = [(i, lower_bind v arg act)]}, loc)
  | Lswitch (ls, ({sw_consts = []; sw_blocks = [(i, act)]} as sw), loc)
    when not (approx_present v ls) ->
    Lswitch (ls, {sw with sw_blocks = [(i, lower_bind v arg act)]}, loc)
  | Llet (Alias, k, vv, lv, l) ->
    if approx_present v lv then bind Alias v arg lam
    else Llet (Alias, k, vv, lv, lower_bind v arg l)
  | Lvar u when Ident.same u v && Ident.name u = "*sth*" ->
    arg (* eliminate let *sth* = from_option x in *sth* *)
  | _ -> bind Alias v arg lam

let bind_check str v arg lam =
  match (str, arg) with
  | _, Lvar _ -> bind str v arg lam
  | Alias, _ -> lower_bind v arg lam
  | _, _ -> bind str v arg lam

let comp_exit ctx m =
  match m.default with
  | (_, i) :: _ -> (Lstaticraise (i, []), jumps_singleton i ctx)
  | _ -> fatal_error "Matching.comp_exit"

let rec comp_match_handlers comp_fun partial ctx arg first_match next_matchs =
  match next_matchs with
  | [] -> comp_fun partial ctx arg first_match
  | rem -> (
    let rec c_rec body total_body = function
      | [] -> (body, total_body)
      (* Hum, -1 means never taken
         | (-1,pm)::rem -> c_rec body total_body rem *)
      | (i, pm) :: rem -> (
        let ctx_i, total_rem = jumps_extract i total_body in
        match ctx_i with
        | [] -> c_rec body total_body rem
        | _ -> (
          try
            let li, total_i =
              comp_fun
                (match rem with
                | [] -> partial
                | _ -> Partial)
                ctx_i arg pm
            in
            c_rec
              (Lstaticcatch (body, (i, []), li))
              (jumps_union total_i total_rem)
              rem
          with Unused ->
            c_rec (Lstaticcatch (body, (i, []), lambda_unit)) total_rem rem))
    in
    try
      let first_lam, total = comp_fun Partial ctx arg first_match in
      c_rec first_lam total rem
    with Unused -> (
      match next_matchs with
      | [] -> raise Unused
      | (_, x) :: xs -> comp_match_handlers comp_fun partial ctx arg x xs))

(* To find reasonable names for variables *)

let rec name_pattern default = function
  | (pat :: _, _) :: rem -> (
    match Typecore.id_of_pattern pat with
    | Some id -> id
    | None -> name_pattern default rem)
  | _ -> Ident.create default

let arg_to_var arg cls =
  match arg with
  | Lvar v -> (v, arg)
  | _ ->
    let v = name_pattern "match" cls in
    (v, Lvar v)

(* To be set by Lam_compile *)
let names_from_construct_pattern :
    (pattern -> Ast_untagged_variants.switch_names option) ref =
  ref (fun _ -> None)

(*
  The main compilation function.
   Input:
      repr=used for inserting debug events
      partial=exhaustiveness information from Parmatch
      ctx=a context
      m=a pattern matching

   Output: a lambda term, a jump summary {..., exit number -> context, .. }
*)

let rec compile_match repr partial ctx m =
  match m with
  | {cases = []; args = []} -> comp_exit ctx m
  | {cases = ([], action) :: rem} ->
    if is_guarded action then
      let lambda, total = compile_match None partial ctx {m with cases = rem} in
      (event_branch repr (patch_guarded lambda action), total)
    else (event_branch repr action, jumps_empty)
  | {args = (arg, str) :: argl} ->
    let v, newarg = arg_to_var arg m.cases in
    let first_match, rem =
      split_precompile (Some v) {m with args = (newarg, Alias) :: argl}
    in
    let lam, total =
      comp_match_handlers
        ((if dbg then do_compile_matching_pr else do_compile_matching) repr)
        partial ctx newarg first_match rem
    in
    (bind_check str v arg lam, total)
  | _ -> assert false

(* verbose version of do_compile_matching, for debug *)

and do_compile_matching_pr repr partial ctx arg x =
  prerr_string "COMPILE: ";
  prerr_endline
    (match partial with
    | Partial -> "Partial"
    | Total -> "Total");
  prerr_endline "MATCH";
  pretty_precompiled x;
  prerr_endline "CTX";
  pretty_ctx ctx;
  let ((_, jumps) as r) = do_compile_matching repr partial ctx arg x in
  prerr_endline "JUMPS";
  pretty_jumps jumps;
  r

and do_compile_matching repr partial ctx arg pmh =
  match pmh with
  | Pm pm -> (
    let pat = what_is_cases pm.cases in
    match pat.pat_desc with
    | Tpat_any -> compile_no_test divide_var ctx_rshift repr partial ctx pm
    | Tpat_tuple patl ->
      compile_no_test
        (divide_tuple (List.length patl) (normalize_pat pat))
        ctx_combine repr partial ctx pm
    | Tpat_record ((_, lbl, _, _) :: _, _) ->
      compile_no_test
        (divide_record lbl.lbl_all (normalize_pat pat))
        ctx_combine repr partial ctx pm
    | Tpat_constant cst ->
      let names = None in
      compile_test
        (compile_match repr partial)
        partial divide_constant
        (combine_constant names pat.pat_loc arg cst partial)
        ctx pm
    | Tpat_construct (_, cstr, _) ->
      let sw_names = !names_from_construct_pattern pat in
      compile_test
        (compile_match repr partial)
        partial divide_constructor
        (combine_constructor sw_names pat.pat_loc arg pat cstr partial)
        ctx pm
    | Tpat_array _ ->
      let names = None in
      compile_test
        (compile_match repr partial)
        partial divide_array
        (combine_array names pat.pat_loc arg partial)
        ctx pm
    | Tpat_lazy _ ->
      compile_no_test
        (divide_lazy (normalize_pat pat))
        ctx_combine repr partial ctx pm
    | Tpat_variant (_, _, row) ->
      let names = None in
      compile_test
        (compile_match repr partial)
        partial (divide_variant !row)
        (combine_variant names pat.pat_loc !row arg partial)
        ctx pm
    | _ -> assert false)
  | PmVar {inside = pmh; var_arg = arg} ->
    let lam, total =
      do_compile_matching repr partial (ctx_lshift ctx) arg pmh
    in
    (lam, jumps_map ctx_rshift total)
  | PmOr {body; handlers} ->
    let lam, total = compile_match repr partial ctx body in
    compile_orhandlers (compile_match repr partial) lam total ctx handlers

and compile_no_test divide up_ctx repr partial ctx to_match =
  let {pm = this_match; ctx = this_ctx} = divide ctx to_match in
  let lambda, total = compile_match repr partial this_ctx this_match in
  (lambda, jumps_map up_ctx total)

(* The entry points *)

(*
   If there is a guard in a matching or a lazy pattern,
   then set exhaustiveness info to Partial.
   (because of side effects, assume the worst).

   Notice that exhaustiveness information is trusted by the compiler,
   that is, a match flagged as Total should not fail at runtime.
   More specifically, for instance if match y with x::_ -> x is flagged
   total (as it happens during JoCaml compilation) then y cannot be []
   at runtime. As a consequence, the static Total exhaustiveness information
   have to be downgraded to Partial, in the dubious cases where guards
   or lazy pattern execute arbitrary code that may perform side effects
   and change the subject values.
LM:
   Lazy pattern was PR#5992, initial patch by lpw25.
   I have  generalized the patch, so as to also find mutable fields.
*)

let find_in_pat pred =
  let rec find_rec p =
    pred p.pat_desc
    ||
    match p.pat_desc with
    | Tpat_alias (p, _, _) | Tpat_variant (_, Some p, _) | Tpat_lazy p ->
      find_rec p
    | Tpat_tuple ps | Tpat_construct (_, _, ps) | Tpat_array ps ->
      List.exists find_rec ps
    | Tpat_record (lpats, _) ->
      List.exists (fun (_, _, p, _) -> find_rec p) lpats
    | Tpat_or (p, q, _) -> find_rec p || find_rec q
    | Tpat_constant _ | Tpat_var _ | Tpat_any | Tpat_variant (_, None, _) ->
      false
  in
  find_rec

let is_lazy_pat = function
  | Tpat_lazy _ -> true
  | Tpat_alias _ | Tpat_variant _ | Tpat_record _ | Tpat_tuple _
  | Tpat_construct _ | Tpat_array _ | Tpat_or _ | Tpat_constant _ | Tpat_var _
  | Tpat_any ->
    false

let is_lazy p = find_in_pat is_lazy_pat p

let have_mutable_field p =
  match p with
  | Tpat_record (lps, _) ->
    List.exists
      (fun (_, lbl, _, _) ->
        match lbl.Types.lbl_mut with
        | Mutable -> true
        | Immutable -> false)
      lps
  | Tpat_alias _ | Tpat_variant _ | Tpat_lazy _ | Tpat_tuple _
  | Tpat_construct _ | Tpat_array _ | Tpat_or _ | Tpat_constant _ | Tpat_var _
  | Tpat_any ->
    false

let is_mutable p = find_in_pat have_mutable_field p

(* Downgrade Total when
   1. Matching accesses some mutable fields;
   2. And there are  guards or lazy patterns.
*)

let check_partial is_mutable is_lazy pat_act_list = function
  | Partial -> Partial
  | Total ->
    if
      pat_act_list = []
      ||
      (* allow empty case list *)
      List.exists
        (fun (pats, lam) -> is_mutable pats && (is_guarded lam || is_lazy pats))
        pat_act_list
    then Partial
    else Total

let check_partial_list =
  check_partial (List.exists is_mutable) (List.exists is_lazy)
let check_partial = check_partial is_mutable is_lazy

(* have toplevel handler when appropriate *)

let start_ctx n = [{left = []; right = omegas n}]

let check_total total lambda i handler_fun =
  if jumps_is_empty total then lambda
  else Lstaticcatch (lambda, (i, []), handler_fun ())

let compile_matching repr handler_fun arg pat_act_list partial =
  let partial = check_partial pat_act_list partial in
  match partial with
  | Partial -> (
    let raise_num = next_raise_count () in
    let pm =
      {
        cases = List.map (fun (pat, act) -> ([pat], act)) pat_act_list;
        args = [(arg, Strict)];
        default = [([[omega]], raise_num)];
      }
    in
    try
      let lambda, total = compile_match repr partial (start_ctx 1) pm in
      check_total total lambda raise_num handler_fun
    with Unused -> assert false (* ; handler_fun() *))
  | Total ->
    let pm =
      {
        cases = List.map (fun (pat, act) -> ([pat], act)) pat_act_list;
        args = [(arg, Strict)];
        default = [];
      }
    in
    let lambda, total = compile_match repr partial (start_ctx 1) pm in
    assert (jumps_is_empty total);
    lambda

let partial_function loc () =
  (* [Location.get_pos_info] is too expensive *)
  let fname, line, char = Location.get_pos_info loc.Location.loc_start in
  let fname = Filename.basename fname in
  Lprim
    ( Praise Raise_regular,
      [
        Lprim
          ( Pmakeblock Blk_extension,
            [
              transl_normal_path Predef.path_match_failure;
              Lconst
                (Const_block
                   ( Blk_tuple,
                     [
                       Const_base (Const_string (fname, None));
                       Const_base (Const_int line);
                       Const_base (Const_int char);
                     ] ));
            ],
            loc );
      ],
      loc )

let for_function loc repr param pat_act_list partial =
  compile_matching repr (partial_function loc) param pat_act_list partial

(* In the following two cases, exhaustiveness info is not available! *)
let for_trywith param pat_act_list =
  compile_matching None
    (fun () -> Lprim (Praise Raise_reraise, [param], Location.none))
    param pat_act_list Partial

let simple_for_let loc param pat body =
  compile_matching None (partial_function loc) param [(pat, body)] Partial

(* Optimize binding of immediate tuples

   The goal of the implementation of 'for_let' below, which replaces
   'simple_for_let', is to avoid tuple allocation in cases such as
   this one:

     let (x,y) =
        let foo = ... in
        if foo then (1, 2) else (3,4)
     in bar

   The compiler easily optimizes the simple `let (x,y) = (1,2) in ...`
   case (call to Matching.for_multiple_match from Translcore), but
   didn't optimize situations where the rhs tuples are hidden under
   a more complex context.

   The idea comes from Alain Frisch who suggested and implemented
   the following compilation method, based on Lassign:

     let x = dummy in let y = dummy in
     begin
      let foo = ... in
      if foo then
        (let x1 = 1 in let y1 = 2 in x <- x1; y <- y1)
      else
        (let x2 = 3 in let y2 = 4 in x <- x2; y <- y2)
     end;
     bar

   The current implementation from Gabriel Scherer uses Lstaticcatch /
   Lstaticraise instead:

     catch
       let foo = ... in
       if foo then
         (let x1 = 1 in let y1 = 2 in exit x1 y1)
       else
        (let x2 = 3 in let y2 = 4 in exit x2 y2)
     with x y ->
       bar

   The catch/exit is used to avoid duplication of the let body ('bar'
   in the example), on 'if' branches for example; it is useless for
   linear contexts such as 'let', but we don't need to be careful to
   generate nice code because Simplif will remove such useless
   catch/exit.
*)

let for_let loc param pat body =
  match pat.pat_desc with
  | Tpat_any ->
    (* This eliminates a useless variable (and stack slot in bytecode)
       for "let _ = ...". See #6865. *)
    Lsequence (param, body)
  | Tpat_var (id, _) ->
    (* fast path, and keep track of simple bindings to unboxable numbers *)
    Llet (Strict, Pgenval, id, param, body)
  | _ -> simple_for_let loc param pat body

(* Handling of tupled functions and matchings *)

(* Easy case since variables are available *)
let for_tupled_function loc paraml pats_act_list partial =
  let partial = check_partial_list pats_act_list partial in
  let raise_num = next_raise_count () in
  let omegas = [List.map (fun _ -> omega) paraml] in
  let pm =
    {
      cases = pats_act_list;
      args = List.map (fun id -> (Lvar id, Strict)) paraml;
      default = [(omegas, raise_num)];
    }
  in
  try
    let lambda, total =
      compile_match None partial (start_ctx (List.length paraml)) pm
    in
    check_total total lambda raise_num (partial_function loc)
  with Unused -> partial_function loc ()

let flatten_pattern size p =
  match p.pat_desc with
  | Tpat_tuple args -> args
  | Tpat_any -> omegas size
  | _ -> raise Cannot_flatten

let rec flatten_pat_line size p k =
  match p.pat_desc with
  | Tpat_any -> omegas size :: k
  | Tpat_tuple args -> args :: k
  | Tpat_or (p1, p2, _) -> flatten_pat_line size p1 (flatten_pat_line size p2 k)
  | Tpat_alias (p, _, _) ->
    (* Note: if this 'as' pat is here, then this is a
       useless binding, solves PR#3780 *)
    flatten_pat_line size p k
  | _ -> fatal_error "Matching.flatten_pat_line"

let flatten_cases size cases =
  List.map
    (fun (ps, action) ->
      match ps with
      | [p] -> (flatten_pattern size p, action)
      | _ -> fatal_error "Matching.flatten_case")
    cases

let flatten_matrix size pss =
  List.fold_right
    (fun ps r ->
      match ps with
      | [p] -> flatten_pat_line size p r
      | _ -> fatal_error "Matching.flatten_matrix")
    pss []

let flatten_def size def =
  List.map (fun (pss, i) -> (flatten_matrix size pss, i)) def

let flatten_pm size args pm =
  {
    args;
    cases = flatten_cases size pm.cases;
    default = flatten_def size pm.default;
  }

let flatten_precompiled size args pmh =
  match pmh with
  | Pm pm -> Pm (flatten_pm size args pm)
  | PmOr {body = b; handlers = hs; or_matrix = m} ->
    PmOr
      {
        body = flatten_pm size args b;
        handlers =
          List.map
            (fun (mat, i, vars, pm) -> (flatten_matrix size mat, i, vars, pm))
            hs;
        or_matrix = flatten_matrix size m;
      }
  | PmVar _ -> assert false

(*
   compiled_flattened is a ``comp_fun'' argument to comp_match_handlers.
   Hence it needs a fourth argument, which it ignores
*)

let compile_flattened repr partial ctx _ pmh =
  match pmh with
  | Pm pm -> compile_match repr partial ctx pm
  | PmOr {body = b; handlers = hs} ->
    let lam, total = compile_match repr partial ctx b in
    compile_orhandlers (compile_match repr partial) lam total ctx hs
  | PmVar _ -> assert false

let do_for_multiple_match loc paraml pat_act_list partial =
  let repr = None in
  let partial = check_partial pat_act_list partial in
  let raise_num, pm1 =
    match partial with
    | Partial ->
      let raise_num = next_raise_count () in
      ( raise_num,
        {
          cases = List.map (fun (pat, act) -> ([pat], act)) pat_act_list;
          args = [(Lprim (Pmakeblock Blk_tuple, paraml, loc), Strict)];
          default = [([[omega]], raise_num)];
        } )
    | _ ->
      ( -1,
        {
          cases = List.map (fun (pat, act) -> ([pat], act)) pat_act_list;
          args = [(Lprim (Pmakeblock Blk_tuple, paraml, loc), Strict)];
          default = [];
        } )
  in

  try
    try
      (* Once for checking that compilation is possible *)
      let next, nexts = split_precompile None pm1 in

      let size = List.length paraml
      and idl = List.map (fun _ -> Ident.create "match") paraml in
      let args = List.map (fun id -> (Lvar id, Alias)) idl in

      let flat_next = flatten_precompiled size args next
      and flat_nexts =
        List.map (fun (e, pm) -> (e, flatten_precompiled size args pm)) nexts
      in

      let lam, total =
        comp_match_handlers (compile_flattened repr) partial (start_ctx size) ()
          flat_next flat_nexts
      in
      List.fold_right2 (bind Strict) idl paraml
        (match partial with
        | Partial -> check_total total lam raise_num (partial_function loc)
        | Total ->
          assert (jumps_is_empty total);
          lam)
    with Cannot_flatten -> (
      let lambda, total = compile_match None partial (start_ctx 1) pm1 in
      match partial with
      | Partial -> check_total total lambda raise_num (partial_function loc)
      | Total ->
        assert (jumps_is_empty total);
        lambda)
  with Unused -> assert false (* ; partial_function loc () *)

(* PR#4828: Believe it or not, the 'paraml' argument below
   may not be side effect free. *)

let param_to_var param =
  match param with
  | Lvar v -> (v, None)
  | _ -> (Ident.create "match", Some param)

let bind_opt (v, eo) k =
  match eo with
  | None -> k
  | Some e -> Lambda.bind Strict v e k

let for_multiple_match loc paraml pat_act_list partial =
  let v_paraml = List.map param_to_var paraml in
  let paraml = List.map (fun (v, _) -> Lvar v) v_paraml in
  List.fold_right bind_opt v_paraml
    (do_for_multiple_match loc paraml pat_act_list partial)
