let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let int i = Lam.const (Const_int {i = Int32.of_int i; comment = None})

let r = Ident.create "r"

let outer = Ident.create "outer"

(* [let r = 1; r = r; outer = r]: an assignment to a bound variable and one to
   a variable bound outside *)
let assignments =
  Lam.let_ Variable r (int 1)
    (Lam.seq (Lam.assign r (Lam.var r)) (Lam.assign outer (Lam.var r)))

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

let suites = __FILE__ >::: ["assign_is_renamed" >:: test_assign_is_renamed]
