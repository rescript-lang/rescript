(* [Ctype.instance] and [instance_def] return a type that is neither generic
   nor marked by an enclosing copy session as it is, without a copy session of
   their own. These tests pin the cases where that is safe and the ones that
   must still copy. *)

let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let assert_bool = OUnit.assert_bool

let instance ?partial ty = Ctype.instance ?partial Env.empty ty

let test_non_generic_is_shared _ =
  let ty = Ctype.newvar () in
  assert_bool "instance returns the node itself" (instance ty == ty);
  assert_bool "instance_def returns the node itself"
    (Ctype.instance_def ty == ty);
  let pair = Ctype.newty (Types.Ttuple [ty; Predef.type_int ()]) in
  assert_bool "a non-generic structure is shared too" (instance pair == pair)

let test_link_is_followed _ =
  let source = Ctype.newvar () and target = Ctype.newvar () in
  Btype.link_type source target;
  assert_bool "instance resolves the link" (instance source == target);
  assert_bool "instance_def resolves the link"
    (Ctype.instance_def source == target)

let test_generic_is_copied _ =
  let scheme = Btype.newgenvar () in
  List.iter
    (fun (name, copy) ->
      assert_bool
        (name ^ " gives a generic variable a fresh instance")
        (copy != scheme);
      assert_bool
        (name ^ " gives an instance that is not generic")
        ((Ctype.repr copy).level <> Btype.generic_level))
    [("instance", instance scheme); ("instance_def", Ctype.instance_def scheme)]

(* The shortcut looks at the representative: a link to a generic node or to a
   marked one is no shortcut *)
let test_link_target_decides _ =
  let generic = Btype.newgenvar () and source = Ctype.newvar () in
  Btype.link_type source generic;
  let copy = instance source in
  assert_bool "a link to a generic variable gives a fresh instance"
    (copy != generic && (Ctype.repr copy).level <> Btype.generic_level);
  Btype.with_copy_session (fun () ->
      let marked = Ctype.newvar () and target = Ctype.newvar () in
      let source = Ctype.newvar () in
      Btype.link_type source marked;
      Btype.save_desc marked marked.desc;
      marked.desc <- Types.Tsubst target;
      assert_bool "a link to a marked node gives the session's copy"
        (instance source == target);
      assert_bool "instance_def too" (Ctype.instance_def source == target))

(* Inside an enclosing copy session, a node marked with [Tsubst] stands for its
   copy; instantiating it must still give that copy. *)
let test_enclosing_session_mark_is_reused _ =
  Btype.with_copy_session (fun () ->
      let marked = Ctype.newvar () and target = Ctype.newvar () in
      Btype.save_desc marked marked.desc;
      marked.desc <- Types.Tsubst target;
      assert_bool "instance gives the enclosing session's copy"
        (instance marked == target);
      assert_bool "instance_def gives the enclosing session's copy"
        (Ctype.instance_def marked == target))

(* A partial instance replaces a non-generic variable without free univars by
   a fresh one, so it must not take the shortcut. *)
let test_partial_still_copies _ =
  let ty = Ctype.newvar () in
  let kept = instance ~partial:true ty in
  assert_bool "partial:true gives a fresh variable" (kept != ty);
  OUnit.assert_equal ~printer:string_of_int ~msg:"partial:true keeps the level"
    (Ctype.repr ty).level (Ctype.repr kept).level;
  let forgotten = instance ~partial:false ty in
  assert_bool "partial:false gives a fresh variable" (forgotten != ty)

let suites =
  __FILE__
  >::: [
         "non_generic_is_shared" >:: test_non_generic_is_shared;
         "link_is_followed" >:: test_link_is_followed;
         "generic_is_copied" >:: test_generic_is_copied;
         "link_target_decides" >:: test_link_target_decides;
         "enclosing_session_mark_is_reused"
         >:: test_enclosing_session_mark_is_reused;
         "partial_still_copies" >:: test_partial_still_copies;
       ]
