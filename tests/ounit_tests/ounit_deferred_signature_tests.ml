(* The copy of a compilation unit's signature that [Env] makes when it is
   first read is deferred when only [Env.normalize_path] asks for it: the
   stamps and type ids it takes are reserved then and used when the copy is
   made later. These tests check that a deferred copy, and everything created
   between the reservation and the copy, gets exactly the stamps and type ids
   it gets when the copy is made at once, for every interface of the runtime
   and Belt (nested modules, functors, module types, aliases). *)

open OUnit

let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let interface_dirs () =
  let runtime = Runtime_package.get_path () in
  let belt = Filename.concat (Filename.dirname runtime) "belt" in
  List.filter Sys.file_exists
    [Filename.concat runtime "lib/ocaml"; Filename.concat belt "lib/ocaml"]

let unit_names dirs =
  List.concat_map
    (fun dir ->
      Sys.readdir dir |> Array.to_list
      |> List.filter (fun file -> Filename.check_suffix file ".cmi")
      |> List.map (fun file ->
          (Cmi_format.read_cmi (Filename.concat dir file)).cmi_name))
    dirs
  |> List.sort_uniq compare

let unit_path name = Path.Pident (Ident.create_persistent name)

let probe () =
  (Ident.binding_time (Ident.create "probe"), (Btype.newgenvar ()).Types.id)

(* Ask whether every unit is an alias, creating an identifier and a type node
   after each question, then read every signature in reverse order. With
   [~eager], each signature is read before the question, so its copy is made
   then, as before the copies were deferred. The result is marshalled: equal
   strings mean equal stamps, type ids and sharing. *)
let run ~eager dirs names =
  Compiler_request_state.with_fresh (fun () ->
      Request_local.with_fresh (fun () ->
          Ident.with_fresh (fun () ->
              Config.set_load_path dirs;
              let env = Env.empty in
              let asked =
                List.map
                  (fun name ->
                    let path = unit_path name in
                    if eager then ignore (Env.find_module path env);
                    let normalized = Env.normalize_path None env path in
                    (normalized, probe ()))
                  names
              in
              let read =
                List.rev_map
                  (fun name -> (Env.find_module (unit_path name) env).md_type)
                  names
              in
              Marshal.to_string (asked, read, probe ()) [])))

let deferred_copy_tests _ =
  let dirs = interface_dirs () in
  let names = unit_names dirs in
  assert_bool "runtime interfaces found" (List.length names > 50);
  (* The eager run also records how many stamps and ids each copy takes. *)
  let eager = run ~eager:true dirs names in
  let reserved_before, fallbacks_before =
    Env.deferred_signature_copy_stats ()
  in
  let deferred = run ~eager:false dirs names in
  let reserved_after, fallbacks_after = Env.deferred_signature_copy_stats () in
  assert_equal ~printer:string_of_int ~msg:"every copy was deferred"
    (List.length names)
    (reserved_after - reserved_before);
  assert_equal ~printer:string_of_int
    ~msg:"every deferred copy took exactly its reserved stamps and type ids"
    fallbacks_before fallbacks_after;
  assert_bool
    "deferred copies give the same stamps, type ids and graphs as eager ones"
    (String.equal eager deferred)

(* A copy that is never read still takes its stamps and ids. *)
let unread_copy_tests _ =
  let dirs = interface_dirs () in
  let names = ["Belt_Array"; "Stdlib_Array"; "Belt_Id"; "Stdlib"] in
  let ask_only ~eager =
    Compiler_request_state.with_fresh (fun () ->
        Request_local.with_fresh (fun () ->
            Ident.with_fresh (fun () ->
                Config.set_load_path dirs;
                List.map
                  (fun name ->
                    let path = unit_path name in
                    if eager then ignore (Env.find_module path Env.empty);
                    ignore (Env.normalize_path None Env.empty path);
                    probe ())
                  names)))
  in
  let eager = ask_only ~eager:true in
  assert_equal eager (ask_only ~eager:false)

let suites =
  __FILE__
  >::: [
         "deferred_copy_matches_eager_copy" >:: deferred_copy_tests;
         "unread_copy_reserves_stamps_and_ids" >:: unread_copy_tests;
       ]
