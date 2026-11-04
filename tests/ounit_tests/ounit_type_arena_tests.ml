(* Tests for the stores of [Types] and for [Type_arena], the boundary between
   the boxed graphs of [Types_boxed] and the store. The boundary must keep one
   node per boxed record and one block per boxed block, which is checked the
   strict way: the marshalled bytes of [export (import sg)] equal the bytes
   of [sg] as written in the runtime package's .cmi files. *)

let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))
let assert_bool = OUnit.assert_bool
let assert_equal = OUnit.assert_equal

module A = Type_arena

(* The runtime package's compiled interfaces, found from the current
   directory or the test executable upwards. *)
let runtime_cmi_dir () =
  let rel = Filename.concat "packages" "@rescript/runtime/lib/ocaml" in
  let rec up dir n =
    let candidate = Filename.concat dir rel in
    if Sys.file_exists candidate && Sys.is_directory candidate then
      Some candidate
    else if n = 0 then None
    else
      let parent = Filename.dirname dir in
      if parent = dir then None else up parent (n - 1)
  in
  match up (Sys.getcwd ()) 8 with
  | Some d -> d
  | None -> (
    let exe =
      if Filename.is_relative Sys.executable_name then
        Filename.concat (Sys.getcwd ()) Sys.executable_name
      else Sys.executable_name
    in
    match up (Filename.dirname exe) 10 with
    | Some d -> d
    | None ->
      OUnit.assert_failure "runtime .cmi files not found; run `make lib` first")

let cmis_in dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun f -> Filename.check_suffix f ".cmi")
  |> List.sort compare
  |> List.map (Filename.concat dir)

let runtime_cmis () = cmis_in (runtime_cmi_dir ())

(* Belt's interfaces too, when built ([make lib] builds them). *)
let all_cmis () =
  let belt =
    Filename.concat
      (Filename.dirname
         (Filename.dirname (Filename.dirname (runtime_cmi_dir ()))))
      "belt/lib/ocaml"
  in
  runtime_cmis ()
  @ if Sys.file_exists belt && Sys.is_directory belt then cmis_in belt else []

let read_file path = In_channel.with_open_bin path In_channel.input_all

(* The signature of a .cmi file and the bytes it was marshalled to. *)
let cmi_signature path =
  let contents = read_file path in
  let start = String.length Config.cmi_magic_number in
  let size = Marshal.total_size (Bytes.unsafe_of_string contents) start in
  let (name : string), (sign : Types_boxed.signature) =
    Marshal.from_string contents start
  in
  (name, sign, String.sub contents start size)

let round_trip_bytes name sign =
  let imported = A.Import.run (fun c -> A.Import.signature c sign) in
  let exported = A.Export.run (fun e -> A.Export.signature e imported) in
  Marshal.to_string (name, exported) []

let check_cmi path =
  let name, sign, bytes = cmi_signature path in
  let bytes' = Types.Store.with_store (fun () -> round_trip_bytes name sign) in
  assert_bool
    (Printf.sprintf "%s: export (import sig) changed the marshalled bytes"
       (Filename.basename path))
    (String.equal bytes bytes')

let test_runtime_round_trip _ =
  let cmis = all_cmis () in
  assert_bool "expected the runtime package's .cmi files" (List.length cmis > 50);
  List.iter check_cmi cmis

(* Importing a signature twice in one session is the identity; in two
   sessions it gives distinct nodes with the same ids. *)
let test_import_memo _ =
  Types.Store.with_store (fun () ->
      let path =
        List.find
          (fun p -> Filename.basename p = "Stdlib_Array.cmi")
          (runtime_cmis ())
      in
      let _, sign, _ = cmi_signature path in
      let s1, n =
        A.Import.run (fun c ->
            let s1 = A.Import.signature c sign in
            let n = A.Import.nodes c in
            let s2 = A.Import.signature c sign in
            assert_bool "same session gives the same signature" (s1 == s2);
            assert_equal ~printer:string_of_int n (A.Import.nodes c);
            (s1, n))
      in
      assert_equal ~printer:string_of_int n
        (Types.Store.length (Types.Store.current ()));
      let s3 = A.Import.run (fun c -> A.Import.signature c sign) in
      assert_bool "a new session copies" (s3 != s1);
      assert_equal ~printer:string_of_int (2 * n)
        (Types.Store.length (Types.Store.current ())))

let boxed desc ~id = {Types_boxed.desc; level = 3; id}

let test_cycles _ =
  Types.Store.with_store (fun () ->
      (* t = (t) => t, through a [Tlink]-free cycle, and a cycle through a
         link: u = Tlink v, v = Ttuple [u]. *)
      let t = boxed Types_boxed.Tnil ~id:1 in
      t.desc <- Types_boxed.Tarrow ([{lbl = Asttypes.Nolabel; typ = t}], t);
      let u = boxed Types_boxed.Tnil ~id:2
      and v = boxed Types_boxed.Tnil ~id:3 in
      u.desc <- Types_boxed.Tlink v;
      v.desc <- Types_boxed.Ttuple [u];
      let t', u', v' =
        A.Import.run (fun c ->
            let im = A.Import.type_expr c in
            (im t, im u, im v))
      in
      (match Types.get_desc t' with
      | Types.Tarrow ([{typ}], ret) ->
        assert_bool "argument is the node itself" (typ == t');
        assert_bool "result is the node itself" (ret == t')
      | _ -> OUnit.assert_failure "expected an arrow");
      assert_bool "u is a link to v" (Types.link_target u' == v');
      (match Types.get_desc v' with
      | Types.Ttuple [x] -> assert_bool "tuple holds u" (x == u')
      | _ -> OUnit.assert_failure "expected a tuple");
      let t'', u'', v'' =
        A.Export.run (fun e ->
            let ex = A.Export.type_expr e in
            (ex t', ex u', ex v'))
      in
      (match t''.desc with
      | Types_boxed.Tarrow ([{typ}], ret) ->
        assert_bool "exported cycle" (typ == t'' && ret == t'')
      | _ -> OUnit.assert_failure "expected an exported arrow");
      assert_equal
        (Marshal.to_string (t, u, v) [])
        (Marshal.to_string (t'', u'', v'') []))

let test_sharing _ =
  Types.Store.with_store (fun () ->
      let int_path = Predef.path_int in
      (* One node used twice, one argument list used by two nodes, one
         descriptor used by two nodes, two equal but distinct nodes, and
         one abbreviation cell used by two descriptors. *)
      let shared =
        boxed (Types_boxed.Tconstr (int_path, [], ref Types_boxed.Mnil)) ~id:10
      in
      let distinct =
        boxed (Types_boxed.Tconstr (int_path, [], ref Types_boxed.Mnil)) ~id:11
      in
      let args = [shared; distinct; shared] in
      let a = boxed (Types_boxed.Ttuple args) ~id:12 in
      let cell = ref Types_boxed.Mnil in
      let b = boxed (Types_boxed.Tconstr (int_path, args, cell)) ~id:13 in
      let b2 = boxed (Types_boxed.Tconstr (int_path, [a], cell)) ~id:16 in
      let var = Types_boxed.Tvar None in
      let v1 = boxed var ~id:14 and v2 = boxed var ~id:15 in
      let root = (a, b, b2, v1, v2, [a; b], [a; b]) in
      let a', b', b2', v1', v2' =
        A.Import.run (fun c ->
            let im = A.Import.type_expr c in
            let a' = im a in
            let b' = im b in
            (a', b', im b2, im v1, im v2))
      in
      assert_bool "cells restored after the session" (!cell = Types_boxed.Mnil);
      (match (Types.get_desc a', Types.get_desc b', Types.get_desc b2') with
      | Types.Ttuple l1, Types.Tconstr (_, l2, c1), Types.Tconstr (_, _, c2)
        -> (
        assert_bool "shared argument list" (l1 == l2);
        assert_bool "shared abbreviation cell" (c1 == c2);
        match l1 with
        | [x; y; z] ->
          assert_bool "shared node" (x == z);
          assert_bool "equal but distinct nodes stay distinct" (x != y)
        | _ -> OUnit.assert_failure "expected three arguments")
      | _ -> OUnit.assert_failure "unexpected descriptors");
      assert_bool "shared descriptor" (Types.get_desc v1' == Types.get_desc v2');
      let root' =
        A.Export.run (fun e ->
            let ex = A.Export.type_expr e in
            ( ex a',
              ex b',
              ex b2',
              ex v1',
              ex v2',
              [ex a'; ex b'],
              [ex a'; ex b'] ))
      in
      assert_equal (Marshal.to_string root []) (Marshal.to_string root' []);
      (* A session cannot be used after it ends. *)
      let escaped = A.Import.run (fun c -> c) in
      OUnit.assert_raises (Invalid_argument "Type_arena: session has ended")
        (fun () -> A.Import.type_expr escaped a))

let test_node_operations _ =
  Types.Store.with_store ~capacity:16 (fun () ->
      let s = Types.Store.current () in
      let n = 100_000 in
      let first = Types.Transient_expr.create Types.Tnil ~level:0 ~id:0 in
      for i = 1 to n - 1 do
        ignore
          (Types.Transient_expr.create (Types.Tvar None) ~level:i ~id:(1000 + i))
      done;
      assert_equal ~printer:string_of_int n (Types.Store.length s);
      let t = Types.Store.nth s 500 in
      assert_equal ~printer:string_of_int 500 (Types.get_level t);
      assert_equal ~printer:string_of_int 1500 (Types.get_id t);
      assert_bool "nodes are found in their store" (Types.Store.of_node t == s);
      let d = Types.get_desc t in
      Types.Transient_expr.set_desc t (Types.Tlink first);
      assert_bool "link target" (Types.link_target t == first);
      assert_bool "not a copy mark" (Types.subst_target t == t);
      (match Types.get_desc t with
      | Types.Tlink x -> assert_bool "link target" (x == first)
      | _ -> OUnit.assert_failure "expected a link");
      Types.Transient_expr.set_desc t (Types.Tsubst first);
      assert_bool "copy mark target" (Types.subst_target t == first);
      assert_bool "not a link" (Types.link_target t == t);
      Types.Transient_expr.set_desc t d;
      assert_bool "restored descriptor" (Types.get_desc t == d);
      Types.Transient_expr.set_level t 7;
      assert_equal ~printer:string_of_int 7 (Types.get_level t);
      assert_equal 0 (Types.Type_ops.compare t t);
      assert_bool "compare by id" (Types.Type_ops.compare first t < 0);
      (* Nested stores: the outer store's nodes stay readable. *)
      Types.Store.with_store (fun () ->
          let inner =
            Types.Transient_expr.create (Types.Ttuple [t]) ~level:1 ~id:1
          in
          assert_bool "inner store is current" (Types.Store.current () != s);
          assert_bool "inner node in the inner store"
            (Types.Store.of_node inner != s);
          assert_equal ~printer:string_of_int 7 (Types.get_level t);
          match Types.get_desc inner with
          | Types.Ttuple [x] -> assert_bool "cross-store reference" (x == t)
          | _ -> OUnit.assert_failure "expected a tuple");
      assert_bool "outer store restored" (Types.Store.current () == s))

(* Several domains, each with its own stores, run round trips and allocation
   at the same time; each domain checks only its own results. *)
let test_domains _ =
  let cmis = runtime_cmis () in
  let work k () =
    List.iteri
      (fun i path ->
        if i mod 2 = k mod 2 then check_cmi path;
        Types.Store.with_store (fun () ->
            let s = Types.Store.current () in
            let nodes =
              Array.init 2000 (fun j ->
                  Types.Transient_expr.create (Types.Tvar None)
                    ~level:((k * 1_000_000) + j)
                    ~id:j)
            in
            Array.iteri
              (fun j t ->
                if
                  Types.get_level t <> (k * 1_000_000) + j
                  || Types.Store.of_node t != s
                then failwith "node read back from another domain's store")
              nodes))
      cmis;
    true
  in
  let domains = List.init 4 (fun k -> Domain.spawn (work k)) in
  assert_bool "every domain round-trips"
    (List.for_all (fun d -> Domain.join d) domains)

let suites =
  __FILE__
  >::: [
         "runtime .cmi round trip" >:: test_runtime_round_trip;
         "import memo" >:: test_import_memo;
         "cycles" >:: test_cycles;
         "sharing" >:: test_sharing;
         "node operations" >:: test_node_operations;
         "domains" >:: test_domains;
       ]
