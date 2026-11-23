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

let rewrite lam = Lam_bounded_vars.rewrite (Hash_ident.create 4) lam

let assert_exit msg expected actual =
  OUnit.assert_equal ~printer:string_of_int ~msg expected actual

let assert_fresh_exit msg original fresh =
  OUnit.assert_bool msg (original <> fresh)

(* Each copy of a catch has its own exit id, its raises follow, and the bound
   variables of the handler are renamed. *)
let test_exit_is_renamed_per_copy _ =
  let exit = Lambda_exits.next_raise_count () in
  let x = Ident.create "x" in
  let lam =
    Lambda.staticcatch
      (Lambda.staticraise exit [int 1])
      (exit, [x]) (Lambda.var x)
  in
  let check_copy lam =
    match (lam : Lambda.t) with
    | Lstaticcatch (Lstaticraise (raised, [_]), (caught, [x']), Lvar x_use) ->
      assert_fresh_exit "the exit is renamed" exit caught;
      assert_exit "its raise follows" caught raised;
      OUnit.assert_bool "the handler's variable is renamed"
        (not (Ident.same x' x));
      OUnit.assert_bool "its use follows" (Ident.same x_use x');
      caught
    | _ -> OUnit.assert_failure "a catch of a raise with one argument"
  in
  let first = check_copy (rewrite lam) in
  let second = check_copy (rewrite lam) in
  assert_fresh_exit "the copies have different exits" first second

(* A raise to an exit caught outside the term keeps its id. *)
let test_outer_exit_is_kept _ =
  let outer = Lambda_exits.next_raise_count () in
  let exit = Lambda_exits.next_raise_count () in
  let lam =
    Lambda.staticcatch
      (Lambda.seq (Lambda.staticraise outer []) (Lambda.staticraise exit []))
      (exit, [])
      (Lambda.staticraise outer [])
  in
  match rewrite lam with
  | Lstaticcatch
      ( Lsequence (Lstaticraise (in_body, []), Lstaticraise (raised, [])),
        (caught, []),
        Lstaticraise (in_handler, []) ) ->
    assert_fresh_exit "the caught exit is renamed" exit caught;
    assert_exit "its raise follows" caught raised;
    assert_exit "the outer exit is kept in the body" outer in_body;
    assert_exit "the outer exit is kept in the handler" outer in_handler
  | _ -> OUnit.assert_failure "a catch of two raises"

(* A catch scopes over its body only: in its handler the same id is the
   enclosing catch's exit, or one caught outside the term. *)
let test_exit_scopes_over_the_body_only _ =
  let exit = Lambda_exits.next_raise_count () in
  let raise_exit = Lambda.staticraise exit [] in
  let inner = Lambda.staticcatch raise_exit (exit, []) raise_exit in
  let lam = Lambda.staticcatch inner (exit, []) raise_exit in
  match rewrite lam with
  | Lstaticcatch
      ( Lstaticcatch
          ( Lstaticraise (in_inner_body, []),
            (inner_caught, []),
            Lstaticraise (in_inner_handler, []) ),
        (outer_caught, []),
        Lstaticraise (in_outer_handler, []) ) ->
    assert_fresh_exit "the inner exit is renamed" exit inner_caught;
    assert_fresh_exit "the outer exit is renamed" exit outer_caught;
    assert_fresh_exit "the two catches have different exits" inner_caught
      outer_caught;
    assert_exit "a raise in the inner body goes to the inner catch" inner_caught
      in_inner_body;
    assert_exit "a raise in the inner handler goes to the outer catch"
      outer_caught in_inner_handler;
    assert_exit "a raise in the outer handler is left alone" exit
      in_outer_handler
  | _ -> OUnit.assert_failure "a catch in the body of a catch"

(* Negative ids mark the exception cases of matching and stay negative. *)
let test_negative_exit_stays_negative _ =
  let exit = Lambda_exits.next_negative_raise_count () in
  let lam =
    Lambda.staticcatch (Lambda.staticraise exit []) (exit, []) (int 1)
  in
  match rewrite lam with
  | Lstaticcatch (Lstaticraise (raised, []), (caught, []), Lconst _) ->
    assert_fresh_exit "the exit is renamed" exit caught;
    assert_exit "its raise follows" caught raised;
    OUnit.assert_bool "the new id is negative" (caught < 0)
  | _ -> OUnit.assert_failure "a catch of a raise"

let suites =
  __FILE__
  >::: [
         "for_start_is_renamed_once" >:: test_for_start_is_renamed_once;
         "assign_is_renamed" >:: test_assign_is_renamed;
         "exit_is_renamed_per_copy" >:: test_exit_is_renamed_per_copy;
         "outer_exit_is_kept" >:: test_outer_exit_is_kept;
         "exit_scopes_over_the_body_only"
         >:: test_exit_scopes_over_the_body_only;
         "negative_exit_stays_negative" >:: test_negative_exit_stays_negative;
       ]
