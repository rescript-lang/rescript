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

let suites =
  __FILE__ >::: ["for_start_is_renamed_once" >:: test_for_start_is_renamed_once]
