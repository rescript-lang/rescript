(* The values of a compilation unit's components are copied when
   they are first read, once the process knows how many type ids each of them
   takes; the ids are reserved when the components are built. These tests check
   that reading them later, in another order or never, gives exactly the type
   ids, stamps and graphs of components built at once, both directly through
   [Env] on every runtime interface and through whole compiler requests that
   use dependencies through functors, aliases, includes, module type of,
   recursive modules, shadowing, first-class modules and error messages. *)

open OUnit

let ( >:: ), ( >::: ) = OUnit.(( >:: ), ( >::: ))

let runtime_dir () =
  Filename.concat (Runtime_package.get_path ()) "lib/rescript"

let probe () =
  (Ident.binding_time (Ident.create "probe"), (Btype.newgenvar ()).Types.id)

let in_fresh_request dirs action =
  Compiler_request_state.with_fresh (fun () ->
      Request_local.with_fresh (fun () ->
          Ident.with_fresh (fun () ->
              Config.set_load_path dirs;
              action ())))

(* What reading one item of a signature gives. *)
type read =
  | Value of Path.t * Types.value_description
  | Type of
      Path.t
      * Types.type_declaration
      * Env.type_descriptions
      * Types.constructor_description list list
      * Types.label_description list list
  | Module of string * read list
  | Missing of string

(* Read the items of [sg], the signature of module [lid], from the last to
   the first, and the submodules declared with a signature recursively. *)
let rec read_signature env lid sg =
  List.fold_left
    (fun acc item -> read_item env lid item :: acc)
    [] (List.rev sg)
  |> List.rev

and read_item env lid item =
  let field name = Longident.Ldot (lid, name) in
  match item with
  | Types.Sig_value (id, _) -> (
    match Env.lookup_value (field (Ident.name id)) env with
    | path, decl -> Value (path, decl)
    | exception Not_found -> Missing (Ident.name id))
  | Sig_type (id, decl, _) -> (
    match Env.lookup_type (field (Ident.name id)) env with
    | path ->
      let constructors =
        match decl.type_kind with
        | Type_variant (cds, _) ->
          List.map
            (fun (cd : Types.constructor_declaration) ->
              List.map fst
                (Env.lookup_all_constructors (field (Ident.name cd.cd_id)) env))
            cds
        | Type_record _ | Type_abstract | Type_open -> []
      in
      let labels =
        match decl.type_kind with
        | Type_record (lds, _) ->
          List.map
            (fun (ld : Types.label_declaration) ->
              List.map fst
                (Env.lookup_all_labels (field (Ident.name ld.ld_id)) env))
            lds
        | Type_variant _ | Type_abstract | Type_open -> []
      in
      Type
        ( path,
          Env.find_type path env,
          Env.find_type_descrs path env,
          constructors,
          labels )
    | exception Not_found -> Missing (Ident.name id))
  | Sig_module (id, {md_type = Mty_signature sg}, _) ->
    Module (Ident.name id, read_signature env (field (Ident.name id)) sg)
  | Sig_module (id, _, _) | Sig_modtype (id, _) | Sig_typext (id, _, _) ->
    Missing (Ident.name id)

let runtime_units dir =
  Sys.readdir dir |> Array.to_list
  |> List.filter (fun file -> Filename.check_suffix file ".cmi")
  |> List.map (fun file ->
      let cmi = Cmi_format.read_cmi (Filename.concat dir file) in
      (cmi.cmi_name, cmi.cmi_sign))
  |> List.sort (fun (a, _) (b, _) -> String.compare a b)

(* Build the components of every unit, creating an identifier and a type node
   after each, then read every item of every unit from the last unit to the
   first. The result is marshalled: equal strings mean equal stamps, type ids
   and sharing. *)
let read_all_units dir units =
  in_fresh_request [dir] (fun () ->
      let env = Env.empty in
      let built =
        List.map
          (fun (name, _) ->
            (match
               Env.lookup_value
                 (Longident.Ldot (Lident name, "not a component"))
                 env
             with
            | _ -> assert_failure "no such value"
            | exception Not_found -> ());
            probe ())
          units
      in
      let read =
        List.fold_left
          (fun acc (name, sg) ->
            (name, read_signature env (Longident.Lident name) sg) :: acc)
          [] (List.rev units)
      in
      Marshal.to_string (built, read, probe ()) [])

let deferred_matches_eager_tests _ =
  let dir = runtime_dir () in
  let units = runtime_units dir in
  assert_bool "runtime interfaces found" (List.length units > 50);
  Env.forget_component_counts ();
  let deferred_before, fallbacks_before = Env.deferred_component_stats () in
  let eager = read_all_units dir units in
  let deferred_eager, _ = Env.deferred_component_stats () in
  assert_equal ~printer:string_of_int ~msg:"nothing deferred before the counts"
    deferred_before deferred_eager;
  let deferred = read_all_units dir units in
  let deferred_after, fallbacks_after = Env.deferred_component_stats () in
  assert_bool "values were deferred" (deferred_after > deferred_eager);
  assert_equal ~printer:string_of_int
    ~msg:"every deferred copy took exactly its reserved type ids"
    fallbacks_before fallbacks_after;
  assert_bool
    "deferred copies give the same stamps, type ids and graphs as eager ones"
    (String.equal eager deferred)

(* Components whose values are never read still take their type ids. *)
let unread_components_tests _ =
  let dir = runtime_dir () in
  let names = ["Stdlib_Array"; "Stdlib_List"; "Stdlib_Option"; "Stdlib"] in
  let build_only () =
    in_fresh_request [dir] (fun () ->
        List.map
          (fun name ->
            (match
               Env.lookup_value
                 (Longident.Ldot (Lident name, "not a component"))
                 Env.empty
             with
            | _ -> assert_failure "no such value"
            | exception Not_found -> ());
            probe ())
          names)
  in
  Env.forget_component_counts ();
  let eager = build_only () in
  assert_equal eager (build_only ())

(* A compilation unit [Forged] whose .cmi claims the CRC of different
   contents. *)
let value name val_type =
  Types.Sig_value
    ( Ident.create name,
      {
        val_type;
        val_kind = Val_reg;
        val_loc = Location.none;
        val_attributes = [];
      } )

let forged_crc = Digest.string "ounit_deferred_components_tests forged crc"

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
    (fun () -> in_fresh_request [] action)

let forged = Longident.Lident "Forged"

let build_forged () =
  match
    Env.lookup_value (Longident.Ldot (forged, "not a component")) Env.empty
  with
  | _ -> assert_failure "no such value"
  | exception Not_found -> ()

let read_forged name =
  snd (Env.lookup_value (Longident.Ldot (forged, name)) Env.empty)

(* Learn that the value [x] of [Forged] takes one type id. *)
let learn_forged_counts () =
  with_forged_unit
    [value "x" (Predef.type_int ())]
    (fun () -> ignore (read_forged "x"))

(* The counts learnt from the first contents are wrong for the second: the
   deferred copy takes more type ids than were reserved, and is made again with
   fresh ones. *)
let count_mismatch_fallback_tests _ =
  learn_forged_counts ();
  let deferred_before, fallbacks_before = Env.deferred_component_stats () in
  let probe_id, copy_id =
    with_forged_unit
      [
        value "x"
          (Btype.newgenty (Ttuple [Predef.type_int (); Predef.type_int ()]));
      ]
      (fun () ->
        build_forged ();
        let _, probe_id = probe () in
        (probe_id, (read_forged "x").val_type.id))
  in
  let deferred_after, fallbacks_after = Env.deferred_component_stats () in
  assert_equal ~printer:string_of_int ~msg:"the copy was deferred"
    (deferred_before + 1) deferred_after;
  assert_equal ~printer:string_of_int ~msg:"the copy fell back"
    (fallbacks_before + 1) fallbacks_after;
  assert_bool "the fallback copy has fresh type ids" (copy_id > probe_id)

(* Counts learnt from contents with a module type where the second contents
   have a value (same CRC, same number of items) do not fit them: the
   components are built at once instead of reserving from a missing count, and
   every value gets its own type ids. *)
let counts_without_value_slot_tests _ =
  let module_type name =
    Types.Sig_modtype
      ( Ident.create name,
        {mtd_type = None; mtd_attributes = []; mtd_loc = Location.none} )
  in
  with_forged_unit
    [value "x" (Predef.type_int ()); module_type "M"]
    (fun () -> ignore (read_forged "x"));
  let deferred_before, _ = Env.deferred_component_stats () in
  let x_id, y_id, probe_id =
    with_forged_unit
      [value "x" (Predef.type_int ()); value "y" (Predef.type_int ())]
      (fun () ->
        build_forged ();
        let _, probe_id = probe () in
        ((read_forged "x").val_type.id, (read_forged "y").val_type.id, probe_id))
  in
  let deferred_after, _ = Env.deferred_component_stats () in
  assert_equal ~printer:string_of_int ~msg:"nothing was deferred"
    deferred_before deferred_after;
  assert_bool "every value has its own type ids, taken when built"
    (x_id <> y_id && x_id < probe_id && y_id < probe_id)

(* A deferred copy that raises puts the type id and stamp counters back and
   stays to be made: reading it again raises again. *)
let raising_copy_tests _ =
  learn_forged_counts ();
  (* A variant type whose row ends in a tuple: copying it fails. *)
  let broken =
    Btype.newgenty
      (Tvariant
         {
           row_fields = [];
           row_more = Btype.newgenty (Ttuple []);
           row_closed = true;
           row_fixed = false;
           row_name = None;
         })
  in
  with_forged_unit
    [value "x" broken]
    (fun () ->
      build_forged ();
      let stamp, id = probe () in
      let read_raises () =
        match read_forged "x" with
        | _ -> assert_failure "the copy should have raised"
        | exception Assert_failure _ -> ()
      in
      read_raises ();
      assert_equal ~msg:"counters put back after the failed copy"
        (stamp + 1, id + 1)
        (probe ());
      read_raises ();
      assert_equal ~msg:"counters put back after the second failed copy"
        (stamp + 2, id + 2)
        (probe ()))

(* [Env.copy_types] replaces values bound by an open: the replaced ones are
   copies, the others stay as they were, and every way of reading the open
   sees the replacements. *)
let copy_types_in_open_tests _ =
  let dir = runtime_dir () in
  in_fresh_request [dir] (fun () ->
      let env =
        match
          Env.open_signature Asttypes.Fresh
            (Path.Pident (Ident.create_persistent "Stdlib_Array"))
            Env.empty
        with
        | Some env -> env
        | None -> assert_failure "Stdlib_Array opens"
      in
      let value env name = snd (Env.lookup_value (Longident.Lident name) env) in
      let names = ["length"; "map"; "reduce"] in
      let before = List.map (value env) names in
      let other_before = value env "concat" in
      let copied = Env.copy_types names env in
      let copied_twice = Env.copy_types ["map"] copied in
      List.iter2
        (fun name (decl : Types.value_description) ->
          let decl' = value copied name in
          assert_bool (name ^ " is a copy") (decl'.val_type != decl.val_type);
          assert_bool
            (name ^ " is the same type")
            (Ctype.equal Env.empty true [decl.val_type] [decl'.val_type]))
        names before;
      assert_bool "values not named are not copied"
        (value copied "concat" == other_before);
      assert_bool "the original environment is unchanged"
        (value env "length" == List.hd before);
      assert_bool "a second copy copies the first one"
        ((value copied_twice "map").val_type != (value copied "map").val_type);
      assert_bool "the second copy keeps the other copies"
        (value copied_twice "length" == value copied "length");
      let folded =
        Env.fold_values
          (fun name _ decl acc -> if name = "length" then decl :: acc else acc)
          None copied []
      in
      assert_bool "folding sees the copy"
        (List.exists (fun decl -> decl == value copied "length") folded);
      assert_bool "folding does not see the original"
        (not (List.exists (fun decl -> decl == List.hd before) folded)))

(* Whole compiler requests *)

let rec ensure_dir path =
  if not (Sys.file_exists path) then (
    ensure_dir (Filename.dirname path);
    try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())

let write_file path contents =
  ensure_dir (Filename.dirname path);
  let channel = open_out_bin path in
  Fun.protect
    ~finally:(fun () -> close_out_noerr channel)
    (fun () -> output_string channel contents)

let read_file path =
  let channel = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in_noerr channel)
    (fun () -> really_input_string channel (in_channel_length channel))

let rec remove_tree path =
  match (Unix.lstat path).st_kind with
  | Unix.S_DIR ->
    Array.iter
      (fun name -> remove_tree (Filename.concat path name))
      (Sys.readdir path);
    Unix.rmdir path
  | _ -> Sys.remove path
  | exception Unix.Unix_error (Unix.ENOENT, _, _) -> ()

let with_temp_dir action =
  let path = Filename.temp_file "deferred-components-" "" in
  Sys.remove path;
  Unix.mkdir path 0o755;
  let path = Unix.realpath path in
  Fun.protect ~finally:(fun () -> remove_tree path) (fun () -> action path)

let dep_source =
  {|type color = Red | Green | Blue(int)
type point = {x: int, y: int, label?: string}
type rec tree = Leaf | Node(tree, int, tree)
type box<'a> = {items: array<'a>, count: int}
type poly = [#a | #b(int)]
type inline = Inline({name: string, age: int})
exception Failed(string)
type ext = ..
type ext += Extended(int)
let origin = {x: 0, y: 0}
let make = items => {items, count: Array.length(items)}
let colorName = c =>
  switch c {
  | Red => "red"
  | Green => "green"
  | Blue(n) => Int.toString(n)
  }
module type Show = {
  type t
  let show: t => string
}
module IntShow: Show with type t = int = {
  type t = int
  let show = n => Int.toString(n)
}
module MakeShow = (S: Show) => {
  let showAll = xs => Array.map(xs, S.show)
}
module Nested = {
  type inner = {value: int}
  let unused = 1
  module Deep = {
    let deep = 42
    type d = D(inner)
  }
}
module Alias = Nested
module rec Even: {
  let even: int => bool
} = {
  let even = n => n == 0 || Odd.odd(n - 1)
}
and Odd: {
  let odd: int => bool
} = {
  let odd = n => n != 0 && Even.even(n - 1)
}
let shadowed = 1
let shadowed = "now a string"
module type OfNested = module type of Nested
let packed: module(Show with type t = int) = module(IntShow)
|}

let main_source =
  {|open Dep
let c = Blue(3)
let p = {x: 1, y: 2}
let q = {...origin, label: "q"}
let n = colorName(c)
let tr = Node(Leaf, 1, Leaf)
let w = make([1, 2])
let v: poly = #b(1)
let i = Inline({name: "a", age: 1})
let e = Extended(2)
module S = MakeShow(IntShow)
let shown = S.showAll([1, 2])
module A = Dep.Alias
let deep = A.Deep.deep
let d = A.Deep.D({value: 3})
include Dep.Nested.Deep
module M: OfNested = Dep.Nested
let even = Even.even(4)
let s: string = shadowed
module P = unpack(packed)
let shownP = P.show(3)
let first = (module(IntShow): module(Show with type t = int))
let raised = try throw(Failed("x")) catch {
  | Failed(s) => s
  }
let arr = Array.map([1, 2], x => x + 1)
let opt = Option.map(Some(1), x => x)
let red = Dep.Red
|}

(* Each of these fails with a message that prints types of dependencies. *)
let error_sources =
  [
    ("ErrRecord.res", {|let p: Dep.point = {x: 1, y: "2"}|});
    ("ErrConstructor.res", {|let c: Dep.tree = Dep.Blue(1)|});
    ("ErrInline.res", {|let i = Dep.Inline({name: 1, age: 1})|});
    ("ErrLabel.res", {|let p: Dep.point = {x: 1, z: 2}|});
    ("ErrFunctor.res", {|module S = Dep.MakeShow(Dep.Nested)|});
    ("ErrAlias.res", {|let i: int = Dep.Alias.Deep.D({value: 1})|});
    ("ErrStdlib.res", {|let x: int = Array.map([1], x => x)|});
    ("ErrPacked.res", {|let x: string = Dep.packed|});
    ("ErrInclude.res", {|include Dep.Nested
let y: string = unused|});
  ]

let request root input =
  let argv =
    [
      "-nostdlib";
      "-I";
      runtime_dir ();
      "-I";
      root;
      "-bs-project-root";
      root;
      "-bs-package-name";
      "deferred-components-test";
      "-bs-package-output";
      "commonjs:.:.js";
      "-bs-no-version-header";
    ]
  in
  Rescript_compiler_driver.run_request ~run_external:None ~cwd:root ~argv ~input

let outputs root base =
  List.filter_map
    (fun ext ->
      let path = Filename.concat root (base ^ ext) in
      if Sys.file_exists path then Some (ext, read_file path) else None)
    [".js"; ".cmi"; ".cmj"; ".cmt"]

(* What compiling [input] gives: exit code, diagnostics and every output. *)
let compile root input =
  let base = Filename.remove_extension input in
  List.iter
    (fun (ext, _) -> Sys.remove (Filename.concat root (base ^ ext)))
    (outputs root base);
  let result = request root input in
  (result.Rescript_compiler_driver.exit_code, result.stderr, outputs root base)

let setup root =
  write_file (Filename.concat root "Dep.res") dep_source;
  write_file (Filename.concat root "Main.res") main_source;
  List.iter
    (fun (name, source) -> write_file (Filename.concat root name) source)
    error_sources;
  let code, stderr, _ = compile root "Dep.res" in
  if code <> 0 then assert_failure ("Dep.res: " ^ stderr)

let requests_tests _ =
  with_temp_dir (fun root ->
      setup root;
      let inputs = "Main.res" :: List.map fst error_sources in
      (* Each input is compiled once with no counts known (everything is
         built at once), then again with the counts that request learnt. *)
      let eager, deferred =
        List.split
          (List.map
             (fun input ->
               Env.forget_component_counts ();
               let eager = compile root input in
               let deferred_before, fallbacks_before =
                 Env.deferred_component_stats ()
               in
               let deferred = compile root input in
               let deferred_after, fallbacks_after =
                 Env.deferred_component_stats ()
               in
               assert_bool
                 (input ^ ": values were deferred")
                 (deferred_after > deferred_before);
               assert_equal ~printer:string_of_int
                 ~msg:
                   (input
                  ^ ": every deferred copy took exactly its reserved type ids")
                 fallbacks_before fallbacks_after;
               (eager, deferred))
             inputs)
      in
      List.iter2
        (fun input ((code, stderr, files) as eager) ->
          let main = input = "Main.res" in
          assert_bool
            (input ^ " exit code: " ^ stderr)
            (if main then code = 0 else code <> 0);
          if not main then
            assert_bool
              (input ^ " reports an error: " ^ stderr)
              (String.length stderr > 0);
          if main then
            assert_equal ~printer:string_of_int ~msg:"Main outputs" 4
              (List.length files);
          ignore eager)
        inputs eager;
      List.iter2
        (fun input (eager, deferred) ->
          let code, stderr, files = deferred in
          let eager_code, eager_stderr, eager_files = eager in
          assert_equal ~printer:string_of_int ~msg:(input ^ " exit code")
            eager_code code;
          assert_equal ~printer:Fun.id ~msg:(input ^ " diagnostics")
            eager_stderr stderr;
          assert_equal
            ~printer:(fun files ->
              String.concat ", "
                (List.map
                   (fun (ext, contents) ->
                     ext ^ " " ^ Digest.to_hex (Digest.string contents))
                   files))
            ~msg:(input ^ " outputs") eager_files files)
        inputs
        (List.combine eager deferred))

(* Requests on several domains at once, each building and reading the
   deferred components of its own environment, give the outputs of the
   eager request. *)
let concurrent_requests_tests _ =
  with_temp_dir (fun root ->
      let dirs =
        List.init 5 (fun index ->
            let dir = Filename.concat root (string_of_int index) in
            ensure_dir dir;
            setup dir;
            dir)
      in
      let portable (code, stderr, files) =
        (* The .cmt files record their own directory. *)
        (code, String.length stderr, List.remove_assoc ".cmt" files)
      in
      Env.forget_component_counts ();
      let eager = portable (compile (List.hd dirs) "Main.res") in
      let domains =
        List.map
          (fun dir ->
            Domain.spawn (fun () ->
                let results =
                  List.init 3 (fun _ -> portable (compile dir "Main.res"))
                in
                let deferred, fallbacks = Env.deferred_component_stats () in
                assert_bool "the domain deferred values" (deferred > 0);
                assert_equal ~printer:string_of_int
                  ~msg:"no deferred copy on the domain fell back" 0 fallbacks;
                results))
          (List.tl dirs)
      in
      (* Join every domain before reporting a failure. *)
      let results =
        List.map
          (fun domain ->
            match Domain.join domain with
            | results -> Ok results
            | exception error -> Error error)
          domains
      in
      List.iter
        (function
          | Ok results ->
            List.iter
              (fun result ->
                assert_bool "a concurrent request gives the eager outputs"
                  (result = eager))
              results
          | Error error -> raise error)
        results)

let suites =
  __FILE__
  >::: [
         "deferred_components_match_eager_ones" >:: deferred_matches_eager_tests;
         "unread_components_reserve_type_ids" >:: unread_components_tests;
         "count_mismatch_falls_back_to_fresh_ids"
         >:: count_mismatch_fallback_tests;
         "counts_without_value_slot_build_at_once"
         >:: counts_without_value_slot_tests;
         "raising_copy_restores_counters" >:: raising_copy_tests;
         "copy_types_in_open" >:: copy_types_in_open_tests;
         "requests_match_eager_requests" >:: requests_tests;
         "concurrent_requests_match_eager_request" >:: concurrent_requests_tests;
       ]
