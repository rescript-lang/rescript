let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let int i = Lambda.const (Lambda.const_int i)

(* [for i in {let s = 1; s} to 10 { i }]: a binder in the start expression *)
let i = Ident.create "i"

let s = Ident.create "s"

let loop =
  Lambda.for_ i
    (Lambda.let_ Strict s (int 1) (Lambda.var s))
    (int 10) Upto (Lambda.var i)

(* [rewrite] gives each bound variable one fresh name and records it in the
   map: the binder in the result, its uses and the map agree, and the map has
   one entry per binder. Renaming the start expression twice gave its binder
   a second name the map didn't record. *)
let test_for_start_is_renamed_once _ =
  let map = Hash_ident.create 4 in
  match Lam_bounded_vars.rewrite map loop with
  | Lfor (i', Llet (_, s', _, Lvar s_use), _, _, Lvar i_use) ->
    OUnit.assert_bool "the loop variable is renamed" (not (Ident.same i' i));
    OUnit.assert_bool "its use follows" (Ident.same i_use i');
    OUnit.assert_bool "the start binder is renamed" (not (Ident.same s' s));
    OUnit.assert_bool "its use follows" (Ident.same s_use s');
    (match Hash_ident.find_opt map s with
    | Some (Lvar recorded) ->
      OUnit.assert_bool "the map records the binder's name"
        (Ident.same recorded s')
    | _ -> OUnit.assert_failure "the start binder is in the map");
    OUnit.assert_equal ~printer:string_of_int ~msg:"one entry per binder" 2
      (Hash_ident.length map)
  | _ -> OUnit.assert_failure "a for loop with a let in its start"

let r = Ident.create "r"

let outer = Ident.create "outer"

(* [let r = 1; r = r; outer = r]: an assignment to a bound variable and one to
   a variable bound outside *)
let assignments =
  Lambda.let_ Variable r (int 1)
    (Lambda.seq
       (Lambda.assign r (Lambda.var r))
       (Lambda.assign outer (Lambda.var r)))

(* An assignment follows its variable's fresh name, as a read does. *)
let test_assign_is_renamed _ =
  match Lam_bounded_vars.rewrite (Hash_ident.create 4) assignments with
  | Llet
      ( Variable,
        r',
        _,
        Lsequence
          (Lassign (assigned, Lvar r_use), Lassign (outer', Lvar r_use_2)) ) ->
    OUnit.assert_bool "the variable is renamed" (not (Ident.same r' r));
    OUnit.assert_bool "its assignment follows" (Ident.same assigned r');
    OUnit.assert_bool "its first use follows" (Ident.same r_use r');
    OUnit.assert_bool "its second use follows" (Ident.same r_use_2 r');
    OUnit.assert_bool "a variable bound outside keeps its name"
      (Ident.same outer' outer)
  | _ -> OUnit.assert_failure "a let with two assignments"

let suites =
  __FILE__
  >::: [
         "for_start_is_renamed_once" >:: test_for_start_is_renamed_once;
         "assign_is_renamed" >:: test_assign_is_renamed;
       ]
