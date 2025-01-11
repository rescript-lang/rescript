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

type error = Illegal_letrec_expr

exception Error of Location.t * error

module Rec_context = struct
  type access =
    | Dereferenced
        (** [Dereferenced] indicates that the value (not just the address) of a
        variable is accessed *)
    | Guarded
        (** [Guarded] indicates that the address of a variable is used in a
        guarded context, i.e. under a constructor.  A variable that is
        dereferenced within a function body or lazy context is also considered
        guarded. *)
    | Unguarded
        (** [Unguarded] indicates that the address of a variable is used in an
        unguarded context, i.e. not under a constructor. *)

  (** [guard] represents guarded contexts such as [C -] and [{l = -}] *)
  let guard : access -> access = function
    | Dereferenced -> Dereferenced
    | Guarded -> Guarded
    | Unguarded -> Guarded

  (** [inspect] represents elimination contexts such as [match - with cases],
      [e -] and [- e] *)
  let inspect : access -> access = function
    | Dereferenced -> Dereferenced
    | Guarded -> Dereferenced
    | Unguarded -> Dereferenced

  (** [delay] represents contexts that delay evaluation such as [fun p -> -]
      or [lazy -] *)
  let delay : access -> access = function
    | Dereferenced -> Guarded
    | Guarded -> Guarded
    | Unguarded -> Guarded

  module Use : sig
    type t

    val guard : t -> t
    (** An expression appears in a guarded context *)

    val discard : t -> t
    (** The address of a subexpression is not used, but may be bound *)

    val inspect : t -> t
    (** The value of a subexpression is inspected with match, application, etc. *)

    val delay : t -> t
    (** An expression appears under 'fun p ->' or 'lazy' *)

    val join : t -> t -> t
    (** Combine the access information of two expressions *)

    val single : Ident.t -> access -> t
    (** Combine the access information of two expressions *)

    val empty : t
    (** No variables are accessed in an expression; it might be a
        constant or a global identifier *)

    val unguarded : t -> Ident.t list
    (** The list of identifiers that are used in an unguarded context *)

    val dependent : t -> Ident.t list
    (** The list of all used identifiers *)
  end = struct
    module M = Map.Make (Ident)

    type t = access M.t
    (** A "t" maps each rec-bound variable to an access status *)

    let map f tbl = M.map f tbl

    let guard t = map guard t

    let inspect t = map inspect t

    let delay t = map delay t

    let discard = guard

    let prec x y =
      match (x, y) with
      | Dereferenced, _ | _, Dereferenced -> Dereferenced
      | Unguarded, _ | _, Unguarded -> Unguarded
      | _ -> Guarded

    let join x y =
      M.fold
        (fun id v tbl ->
          let v' = try M.find id tbl with Not_found -> Guarded in
          M.add id (prec v v') tbl)
        x y

    let single id access = M.add id access M.empty

    let empty = M.empty

    let list_matching p t =
      let r = ref [] in
      M.iter (fun id v -> if p v then r := id :: !r) t;
      !r

    let unguarded =
      list_matching (function
        | Unguarded | Dereferenced -> true
        | _ -> false)

    let dependent = list_matching (function _ -> true)
  end

  module Env = struct
    (* A typing environment maps identifiers to types *)
    type env = Use.t Ident.tbl

    let empty = Ident.empty

    let join x y =
      let r =
        Ident.fold_all
          (fun id v tbl ->
            let v' = try Ident.find_same id tbl with Not_found -> Use.empty in
            Ident.add id (Use.join v v') tbl)
          x y
      in
      r
  end
end

let rec pattern_variables : Typedtree.pattern -> Ident.t list =
 fun pat ->
  match pat.pat_desc with
  | Tpat_any -> []
  | Tpat_var (id, _) -> [id]
  | Tpat_alias (pat, id, _) -> id :: pattern_variables pat
  | Tpat_constant _ -> []
  | Tpat_tuple pats -> List.concat (List.map pattern_variables pats)
  | Tpat_construct (_, _, pats) -> List.concat (List.map pattern_variables pats)
  | Tpat_variant (_, Some pat, _) -> pattern_variables pat
  | Tpat_variant (_, None, _) -> []
  | Tpat_record (fields, _) ->
    List.concat (List.map (fun (_, _, p, _) -> pattern_variables p) fields)
  | Tpat_array pats -> List.concat (List.map pattern_variables pats)
  | Tpat_or (l, r, _) -> pattern_variables l @ pattern_variables r
  | Tpat_lazy p -> pattern_variables p

open Rec_context
open Asttypes
open Typedtree

let build_unguarded_env : Ident.t list -> Env.env =
 fun idlist ->
  List.fold_left
    (fun env id -> Ident.add id (Use.single id Unguarded) env)
    Env.empty idlist

let is_ref : Types.value_description -> bool = function
  | {
      Types.val_kind =
        Types.Val_prim {Primitive.prim_name = "%makeref"; prim_arity = 1};
    } ->
    true
  | _ -> false

type sd = Static | Dynamic

let value_default f ~default a =
  match a with
  | None -> default
  | Some a -> f a

let rec classify_expression : Typedtree.expression -> sd =
 fun exp ->
  match exp.exp_desc with
  | Texp_let (_, _, e)
  | Texp_letmodule (_, _, _, e)
  | Texp_sequence (_, e)
  | Texp_letexception (_, e) ->
    classify_expression e
  | Texp_ident _ | Texp_for _ | Texp_constant _ | Texp_tuple _ | Texp_array _
  | Texp_construct _ | Texp_variant _ | Texp_record _ | Texp_setfield _
  | Texp_while _ | Texp_pack _ | Texp_function _ | Texp_lazy _
  | Texp_extension_constructor _ ->
    Static
  | Texp_apply {funct = {exp_desc = Texp_ident (_, _, vd)}} when is_ref vd ->
    Static
  | Texp_apply _ | Texp_match _ | Texp_ifthenelse _ | Texp_send _ | Texp_field _
  | Texp_assert _ | Texp_try _ ->
    Dynamic

let rec expression : Env.env -> Typedtree.expression -> Use.t =
 fun env exp ->
  match exp.exp_desc with
  | Texp_ident (pth, _, _) -> path env pth
  | Texp_let (rec_flag, bindings, body) ->
    let env', ty = value_bindings rec_flag env bindings in
    (* Here and in other binding constructs 'discard' is used in a
       similar way to the way it's used in sequence: uses are
       propagated, but unguarded access are not. *)
    Use.join (Use.discard ty) (expression (Env.join env env') body)
  | Texp_letmodule (x, _, m, e) ->
    let ty = modexp env m in
    Use.join (Use.discard ty) (expression (Ident.add x ty env) e)
  | Texp_match (e, val_cases, exn_cases, _) ->
    let t = expression env e in
    let exn_case env {Typedtree.c_rhs} = expression env c_rhs in
    let cs = list (case ~scrutinee:t) env val_cases
    and es = list exn_case env exn_cases in
    Use.(join cs es)
  | Texp_for (_, _, e1, e2, _, e3) ->
    Use.(
      join
        (join (inspect (expression env e1)) (inspect (expression env e2)))
        (* The body is evaluated, but not used, and not available
           for inclusion in another value *)
        (discard (expression env e3)))
  | Texp_constant _ -> Use.empty
  | Texp_apply
      {funct = {exp_desc = Texp_ident (_, _, vd)}; args = [(_, Some arg)]}
    when is_ref vd ->
    Use.guard (expression env arg)
  | Texp_apply {funct = e; args} ->
    let arg env (_, eo) = option expression env eo in
    Use.(join (inspect (expression env e)) (inspect (list arg env args)))
  | Texp_tuple exprs -> Use.guard (list expression env exprs)
  | Texp_array exprs -> Use.guard (list expression env exprs)
  | Texp_construct (_, desc, exprs) ->
    let access_constructor =
      match desc.cstr_tag with
      | Cstr_extension pth -> Use.inspect (path env pth)
      | _ -> Use.empty
    in
    let use =
      match desc.cstr_tag with
      | Cstr_unboxed -> fun x -> x
      | Cstr_constant _ | Cstr_block _ | Cstr_extension _ -> Use.guard
    in
    Use.join access_constructor (use (list expression env exprs))
  | Texp_variant (_, eo) -> Use.guard (option expression env eo)
  | Texp_record {fields = es; extended_expression = eo; representation = rep} ->
    let use =
      match rep with
      | Record_unboxed _ -> fun x -> x
      | Record_float_unused -> assert false
      | Record_regular | Record_inlined _ | Record_extension -> Use.guard
    in
    let field env = function
      | _, Kept _, _ -> Use.empty
      | _, Overridden (_, e), _ -> expression env e
    in
    Use.join (use (array field env es)) (option expression env eo)
  | Texp_ifthenelse (cond, ifso, ifnot) ->
    Use.(
      join
        (inspect (expression env cond))
        (join (expression env ifso) (option expression env ifnot)))
  | Texp_setfield (e1, _, _, e2) ->
    Use.(join (inspect (expression env e1)) (inspect (expression env e2)))
  | Texp_sequence (e1, e2) ->
    Use.(join (discard (expression env e1)) (expression env e2))
  | Texp_while (e1, e2) ->
    Use.(join (inspect (expression env e1)) (discard (expression env e2)))
  | Texp_send (e1, _, eo) ->
    Use.(
      join (inspect (expression env e1)) (inspect (option expression env eo)))
  | Texp_field (e, _, _) -> Use.(inspect (expression env e))
  | Texp_letexception (_, e) -> expression env e
  | Texp_assert e -> Use.inspect (expression env e)
  | Texp_pack m -> modexp env m
  | Texp_try (e, cases) ->
    (* This is more permissive than the old check. *)
    let case env {Typedtree.c_rhs} = expression env c_rhs in
    Use.join (expression env e) (list case env cases)
  | Texp_function {case = case_} ->
    Use.delay (list (case ~scrutinee:Use.empty) env [case_])
  | Texp_lazy e -> (
    match Typeopt.classify_lazy_argument e with
    | `Constant_or_function | `Identifier _ | `Float -> expression env e
    | `Other -> Use.delay (expression env e))
  | Texp_extension_constructor _ -> Use.empty

and option : 'a. (Env.env -> 'a -> Use.t) -> Env.env -> 'a option -> Use.t =
 fun f env -> value_default (f env) ~default:Use.empty

and list : 'a. (Env.env -> 'a -> Use.t) -> Env.env -> 'a list -> Use.t =
 fun f env ->
  List.fold_left (fun typ item -> Use.join (f env item) typ) Use.empty

and array : 'a. (Env.env -> 'a -> Use.t) -> Env.env -> 'a array -> Use.t =
 fun f env ->
  Array.fold_left (fun typ item -> Use.join (f env item) typ) Use.empty

and modexp : Env.env -> Typedtree.module_expr -> Use.t =
 fun env m ->
  match m.mod_desc with
  | Tmod_ident (pth, _) -> path env pth
  | Tmod_structure s -> structure env s
  | Tmod_functor (_, _, _, e) -> Use.delay (modexp env e)
  | Tmod_apply (f, p, _) ->
    Use.(join (inspect (modexp env f)) (inspect (modexp env p)))
  | Tmod_constraint (m, _, _, Tcoerce_none) -> modexp env m
  | Tmod_constraint (m, _, _, _) -> Use.inspect (modexp env m)
  | Tmod_unpack (e, _) -> expression env e

and path : Env.env -> Path.t -> Use.t =
 fun env pth ->
  match pth with
  | Path.Pident x -> ( try Ident.find_same x env with Not_found -> Use.empty)
  | Path.Pdot (t, _, _) -> Use.inspect (path env t)
  | Path.Papply (f, p) -> Use.(inspect (join (path env f) (path env p)))

and structure : Env.env -> Typedtree.structure -> Use.t =
 fun env s ->
  let _, ty =
    List.fold_left
      (fun (env, ty) item ->
        let env', ty' = structure_item env item in
        (Env.join env env', Use.join ty ty'))
      (env, Use.empty) s.str_items
  in
  Use.guard ty

and structure_item : Env.env -> Typedtree.structure_item -> Env.env * Use.t =
 fun env s ->
  match s.str_desc with
  | Tstr_eval (e, _) -> (Env.empty, expression env e)
  | Tstr_value (rec_flag, valbinds) -> value_bindings rec_flag env valbinds
  | Tstr_module {mb_id; mb_expr} ->
    let ty = modexp env mb_expr in
    (Ident.add mb_id ty Env.empty, ty)
  | Tstr_recmodule mbs ->
    let modbind env {mb_expr} = modexp env mb_expr in
    (* Over-approximate: treat any access as a use *)
    (Env.empty, Use.inspect (list modbind env mbs))
  | Tstr_primitive _ -> (Env.empty, Use.empty)
  | Tstr_type _ -> (Env.empty, Use.empty)
  | Tstr_typext _ -> (Env.empty, Use.empty)
  | Tstr_exception _ -> (Env.empty, Use.empty)
  | Tstr_modtype _ -> (Env.empty, Use.empty)
  | Tstr_open _ -> (Env.empty, Use.empty)
  | Tstr_include inc ->
    (* This is a kind of projection.  There's no need to add
       anything to the environment because everything is used in
       the type component already *)
    (Env.empty, Use.inspect (modexp env inc.incl_mod))
  | Tstr_attribute _ -> (Env.empty, Use.empty)

and case : Env.env -> Typedtree.case -> scrutinee:Use.t -> Use.t =
 fun env {Typedtree.c_lhs; c_guard; c_rhs} ~scrutinee:ty ->
  let ty =
    if is_destructuring_pattern c_lhs then Use.inspect ty else Use.discard ty
    (* as in 'let' *)
  in
  let vars = pattern_variables c_lhs in
  let env = List.fold_left (fun env id -> Ident.add id ty env) env vars in
  Use.(
    join ty
      (join (expression env c_rhs) (inspect (option expression env c_guard))))

and value_bindings :
    rec_flag -> Env.env -> Typedtree.value_binding list -> Env.env * Use.t =
 fun rec_flag env bindings ->
  match rec_flag with
  | Recursive ->
    (* Approximation:
          let rec y =
            let rec x1 = e1
                and x2 = e2
              in e
       treated as
          let rec y =
             let rec x = (e1, e2)[x1:=fst x, x2:=snd x] in
                e[x1:=fst x, x2:=snd x]
       Further, use the fact that x1,x2 cannot occur unguarded in e1, e2
       to avoid recursive trickiness.
    *)
    let ids, ty =
      List.fold_left
        (fun (pats, tys) {vb_pat = p; vb_expr = e} ->
          (pattern_variables p @ pats, Use.join (expression env e) tys))
        ([], Use.empty) bindings
    in
    ( List.fold_left
        (fun (env : Env.env) (id : Ident.t) -> Ident.add id ty env)
        Env.empty ids,
      ty )
  | Nonrecursive ->
    List.fold_left
      (fun (env2, ty) binding ->
        let env', ty' = value_binding env binding in
        (Env.join env2 env', Use.join ty ty'))
      (Env.empty, Use.empty) bindings

and value_binding : Env.env -> Typedtree.value_binding -> Env.env * Use.t =
 (* NB: returns new environment only *)
 fun env {vb_pat; vb_expr} ->
  let vars = pattern_variables vb_pat in
  let ty = expression env vb_expr in
  let ty = if is_destructuring_pattern vb_pat then Use.inspect ty else ty in
  (List.fold_left (fun env id -> Ident.add id ty env) Env.empty vars, ty)

and is_destructuring_pattern : Typedtree.pattern -> bool =
 fun pat ->
  match pat.pat_desc with
  | Tpat_any -> false
  | Tpat_var (_, _) -> false
  | Tpat_alias (pat, _, _) -> is_destructuring_pattern pat
  | Tpat_constant _ -> true
  | Tpat_tuple _ -> true
  | Tpat_construct (_, _, _) -> true
  | Tpat_variant _ -> true
  | Tpat_record (_, _) -> true
  | Tpat_array _ -> true
  | Tpat_or (l, r, _) ->
    is_destructuring_pattern l || is_destructuring_pattern r
  | Tpat_lazy _ -> true

let check_recursive_expression idlist expr =
  let ty = expression (build_unguarded_env idlist) expr in
  match (Use.unguarded ty, Use.dependent ty, classify_expression expr) with
  | _ :: _, _, _ (* The expression inspects rec-bound variables *)
  | _, _ :: _, Dynamic ->
    (* The expression depends on rec-bound variables
       and its size is unknown *)
    raise (Error (expr.exp_loc, Illegal_letrec_expr))
  | [], _, Static (* The expression has known size *) | [], [], Dynamic ->
    (* The expression has unknown size,
       but does not depend on rec-bound variables *)
    ()

let check_recursive_bindings valbinds =
  let ids =
    List.concat (List.map (fun b -> pattern_variables b.vb_pat) valbinds)
  in
  Ext_list.iter valbinds (fun {vb_expr} ->
      match vb_expr.exp_desc with
      | Texp_record
          {fields = [|(_, Overridden (_, {exp_desc = Texp_function _}), _)|]}
      | Texp_function _ ->
        ()
      (*TODO: add uncurried function too*)
      | _ -> check_recursive_expression ids vb_expr)

let report_error ppf = function
  | Illegal_letrec_expr ->
    Format.fprintf ppf
      "This kind of expression is not allowed as right-hand side of `let rec'"

let () =
  Location.register_error_of_exn (function
    | Error (loc, err) -> Some (Location.error_of_printer loc report_error err)
    | _ -> None)
