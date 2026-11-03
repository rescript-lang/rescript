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
                (* [List.map] applies its function from the head: read the
                   last unit's signature first. *)
                List.map
                  (fun name -> (Env.find_module (unit_path name) env).md_type)
                  (List.rev names)
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

(* [Ident.with_current_time] puts the counter back also when its function
   raises. *)
let with_current_time_restores_on_exception _ =
  Ident.with_fresh (fun () ->
      let before = Ident.current_time () in
      let inside = ref 0 in
      (match
         Ident.with_current_time (before + 100) (fun () ->
             inside := Ident.binding_time (Ident.create "inside");
             failwith "raised inside")
       with
      | _ -> assert_failure "the function should have raised"
      | exception Failure _ -> ());
      assert_equal ~printer:string_of_int (before + 101) !inside;
      assert_equal ~printer:string_of_int ~msg:"counter restored after raise"
        before (Ident.current_time ());
      let (), final =
        Ident.with_current_time (before + 7) (fun () ->
            ignore (Ident.create "a");
            ignore (Ident.create "b"))
      in
      assert_equal ~printer:string_of_int (before + 9) final;
      assert_equal ~printer:string_of_int ~msg:"counter restored after return"
        before (Ident.current_time ()))

(* A compilation unit [Forged] whose .cmi claims the CRC of different contents:
   the counts learnt from the first contents are wrong for the second, so its
   deferred copy takes more stamps and ids than were reserved, and is made
   again with fresh ones. *)
let value name =
  Types.Sig_value
    ( Ident.create name,
      {
        val_type = Predef.type_int ();
        val_kind = Val_reg;
        val_loc = Location.none;
        val_attributes = [];
      } )

let forged_crc = Digest.string "ounit_deferred_signature_tests forged crc"

let with_forged_unit sign action =
  let cmi =
    {
      Cmi_format.cmi_name = "Forged";
      cmi_sign = sign;
      cmi_crcs = [("Forged", Some forged_crc)];
      cmi_flags = [];
    }
  in
  let saved_load = !Env.Persistent_signature.load in
  (Env.Persistent_signature.load :=
     fun ~unit_name ->
       if unit_name = "Forged" then
         Some {Env.Persistent_signature.filename = "Forged.cmi"; cmi}
       else saved_load ~unit_name);
  Fun.protect
    ~finally:(fun () -> Env.Persistent_signature.load := saved_load)
    (fun () ->
      Compiler_request_state.with_fresh (fun () ->
          Request_local.with_fresh (fun () -> Ident.with_fresh action)))

let count_mismatch_fallback_tests _ =
  let path = unit_path "Forged" in
  (* Learn the counts of a one-value signature. *)
  with_forged_unit
    [value "x"]
    (fun () -> ignore (Env.find_module path Env.empty));
  let reserved_before, fallbacks_before =
    Env.deferred_signature_copy_stats ()
  in
  let stamps, probe_stamp =
    with_forged_unit
      [value "x"; value "y"]
      (fun () ->
        ignore (Env.normalize_path None Env.empty path);
        let probe_stamp = Ident.binding_time (Ident.create "probe") in
        match (Env.find_module path Env.empty).md_type with
        | Mty_signature items ->
          ( List.map
              (function
                | Types.Sig_value (id, _) -> Ident.binding_time id
                | _ -> assert_failure "expected values")
              items,
            probe_stamp )
        | _ -> assert_failure "expected a signature")
  in
  let reserved_after, fallbacks_after = Env.deferred_signature_copy_stats () in
  assert_equal ~printer:string_of_int ~msg:"the copy was deferred"
    (reserved_before + 1) reserved_after;
  assert_equal ~printer:string_of_int ~msg:"the copy fell back"
    (fallbacks_before + 1) fallbacks_after;
  assert_bool "the fallback copy has both values with fresh stamps"
    (List.length stamps = 2
    && List.for_all (fun stamp -> stamp > probe_stamp) stamps)

let suites =
  __FILE__
  >::: [
         "deferred_copy_matches_eager_copy" >:: deferred_copy_tests;
         "unread_copy_reserves_stamps_and_ids" >:: unread_copy_tests;
         "with_current_time_restores_on_exception"
         >:: with_current_time_restores_on_exception;
         "count_mismatch_falls_back_to_fresh_ids"
         >:: count_mismatch_fallback_tests;
       ]
