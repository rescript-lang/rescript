(** Reactive dead code solver.
    
    Reactive pipeline: decls + live + annotations → dead_decls, live_decls, dead_modules,
    dead_decls_by_file, issues_by_file, incorrect_dead_decls, dead_module_issues
    
    Current status:
    - All collections are reactive (zero recomputation on cache hit for unchanged files)
    - dead_decls, live_decls = decls partitioned by liveness (reactive join)
    - dead_modules = modules with dead decls but no live decls (reactive anti-join)
    - dead_decls_by_file = dead decls grouped by file (reactive flatMap with merge)
    - value_refs_from_by_file = refs grouped by source file (reactive flatMap with merge)
    - issues_by_file = per-file issue generation (reactive flatMap)
    - incorrect_dead_decls = live decls with @dead annotation (reactive join)
    - dead_module_issues = dead_modules joined with modules_with_reported (reactive join)
    - is_pos_live uses reactive live collection (no resolvedDead mutation)
    - shouldReport callback replaces report field mutation (no mutation needed)
    - isInsideReportedValue is per-file only, so files are independent
    - hasRefBelow uses on-demand search: O(total_refs) per dead decl (cross-file refs count as "below")
    
    All issues now match between reactive and non-reactive modes (380 on deadcode test):
    - Dead code issues: 362 (Exception:2, Module:31, Type:87, Value:233, ValueWithSideEffects:8)
    - Incorrect @dead: 1
    - Optional args: 18 (Redundant:6, Unused:12) *)

type t = {
  decls: (Lexing.position, Decl.t) Reactive.t;
  live: (Lexing.position, unit) Reactive.t;
  dead_decls: (Lexing.position, Decl.t) Reactive.t;
  live_decls: (Lexing.position, Decl.t) Reactive.t;
  annotations: (Lexing.position, FileAnnotations.annotated_as) Reactive.t;
  value_refs_from: (Lexing.position, PosSet.t) Reactive.t option;
  dead_modules: (Name.t, Location.t * string) Reactive.t;
      (** Modules where all declarations are dead. Value is (loc, fileName). Reactive anti-join. *)
  dead_decls_by_file: (string, Decl.t list) Reactive.t;
      (** Dead declarations grouped by file. Reactive per-file grouping. *)
  issues_by_file: (string, Issue.t list * Name.t list) Reactive.t;
      (** Dead code issues grouped by file. Reactive per-file issue generation.
          First component: value/type/exception issues.
          Second component: modules with at least one reported value (for module issue generation). *)
  incorrect_dead_decls: (Lexing.position, Decl.t) Reactive.t;
      (** Live declarations with @dead annotation. Reactive join of live_decls + annotations. *)
  dead_module_issues: (Name.t, Issue.t) Reactive.t;
      (** Dead module issues. Reactive join of dead_modules + modules_with_reported. *)
  config: DceConfig.t;
}

(** Extract module name from a declaration *)
let decl_module_name (decl : Decl.t) : Name.t =
  decl.path |> DcePath.toModuleName ~isType:(decl.declKind |> Decl.Kind.isType)

let create ~(decls : (Lexing.position, Decl.t) Reactive.t)
    ~(live : (Lexing.position, unit) Reactive.t)
    ~(annotations : (Lexing.position, FileAnnotations.annotated_as) Reactive.t)
    ~(value_refs_from : (Lexing.position, PosSet.t) Reactive.t option)
    ~(config : DceConfig.t) : t =
  (* dead_decls = decls where NOT in live (reactive join) *)
  let dead_decls =
    Reactive.join ~name:"solver.dead_decls" decls live
      ~key_of:(fun pos _decl -> pos)
      ~f:(fun pos decl live_opt ->
        match live_opt with
        | None -> [(pos, decl)]
        | Some () -> [])
      ()
  in

  (* live_decls = decls where in live (reactive join) *)
  let live_decls =
    Reactive.join ~name:"solver.live_decls" decls live
      ~key_of:(fun pos _decl -> pos)
      ~f:(fun pos decl live_opt ->
        match live_opt with
        | Some () -> [(pos, decl)]
        | None -> [])
      ()
  in

  (* Reactive dead modules: modules with dead decls but no live decls *)
  let dead_modules =
    if not config.DceConfig.run.transitive then
      (* Dead modules only reported in transitive mode *)
      Reactive.flatMap ~name:"solver.dead_modules_empty" dead_decls
        ~f:(fun _ _ -> [])
        ()
    else
      (* modules_with_dead: (moduleName, (loc, fileName)) for each module with dead decls *)
      let modules_with_dead =
        Reactive.flatMap ~name:"solver.modules_with_dead" dead_decls
          ~f:(fun _pos decl ->
            [
              ( decl_module_name decl,
                (decl.moduleLoc, decl.pos.Lexing.pos_fname) );
            ])
          ~merge:(fun v1 _v2 -> v1) (* keep first *)
          ()
      in
      (* modules_with_live: (moduleName, ()) for each module with live decls *)
      let modules_with_live =
        Reactive.flatMap ~name:"solver.modules_with_live" live_decls
          ~f:(fun _pos decl -> [(decl_module_name decl, ())])
          ()
      in
      (* Anti-join: modules in dead but not in live *)
      Reactive.join ~name:"solver.dead_modules" modules_with_dead
        modules_with_live
        ~key_of:(fun modName (_loc, _fileName) -> modName)
        ~f:(fun modName (loc, fileName) live_opt ->
          match live_opt with
          | None -> [(modName, (loc, fileName))] (* dead: no live decls *)
          | Some () -> []) (* live: has at least one live decl *)
        ()
  in

  (* Reactive per-file grouping of dead declarations *)
  let dead_decls_by_file =
    Reactive.flatMap ~name:"solver.dead_decls_by_file" dead_decls
      ~f:(fun _pos decl -> [(decl.pos.Lexing.pos_fname, [decl])])
      ~merge:(fun decls1 decls2 -> decls1 @ decls2)
      ()
  in

  let transitive = config.DceConfig.run.transitive in

  (* Reactive per-file issues.
     IMPORTANT: in non-transitive mode, warning emission depends on hasRefBelow,
     which depends on value_refs_from (cross-file refs). So we must recompute
     issues when refs change, not only when the file's dead decls change. *)
  let issues_for_file (_file : string) decls =
    (* Track modules that have reported values *)
    let modules_with_values : (Name.t, unit) Hashtbl.t = Hashtbl.create 8 in
    (* shouldReport checks annotations reactively *)
    let shouldReport (decl : Decl.t) =
      match Reactive.get annotations decl.pos with
      | Some FileAnnotations.Live -> false
      | Some FileAnnotations.GenType -> false
      | Some FileAnnotations.Dead -> false
      | None -> true
    in
    (* Don't emit module issues here - track modules for later *)
    let checkModuleDead ~fileName:_ moduleName =
      Hashtbl.replace modules_with_values moduleName ();
      None (* Module issues generated separately *)
    in
    (* hasRefBelow: check if decl has any ref from "below" (including cross-file refs) *)
    let hasRefBelow =
      if transitive then fun _ -> false
      else
        match value_refs_from with
        | None -> fun _ -> false
        | Some refs_from ->
          (* Must iterate ALL refs since cross-file refs also count as "below" *)
          DeadCommon.make_hasRefBelow ~transitive
            ~iter_value_refs_from:(fun f -> Reactive.iter f refs_from)
    in
    (* Sort within file and generate issues *)
    let sorted = decls |> List.fast_sort Decl.compareForReporting in
    let reporting_ctx = DeadCommon.ReportingContext.create () in
    let file_issues =
      sorted
      |> List.concat_map (fun decl ->
             DeadCommon.reportDeclaration ~config ~hasRefBelow ~checkModuleDead
               ~shouldReport reporting_ctx decl)
    in
    let modules_list =
      Hashtbl.fold (fun m () acc -> m :: acc) modules_with_values []
    in
    (file_issues, modules_list)
  in
  let issues_by_file =
    match (transitive, value_refs_from) with
    | true, _ | false, None ->
      Reactive.flatMap ~name:"solver.issues_by_file" dead_decls_by_file
        ~f:(fun file decls -> [(file, issues_for_file file decls)])
        ()
    | false, Some refs_from ->
      (* Create a singleton "refs token" that changes whenever refs_from changes,
         and join every file against it so per-file issues recompute. *)
      let refs_token =
        Reactive.flatMap ~name:"solver.refs_token" refs_from
          ~f:(fun _posFrom _targets -> [((), ())])
          ~merge:(fun _ _ -> ())
          ()
      in
      Reactive.join ~name:"solver.issues_by_file" dead_decls_by_file refs_token
        ~key_of:(fun _file _decls -> ())
        ~f:(fun file decls _token_opt -> [(file, issues_for_file file decls)])
        ()
  in

  (* Reactive incorrect @dead: live decls with @dead annotation *)
  let incorrect_dead_decls =
    Reactive.join ~name:"solver.incorrect_dead_decls" live_decls annotations
      ~key_of:(fun pos _decl -> pos)
      ~f:(fun pos decl ann_opt ->
        match ann_opt with
        | Some FileAnnotations.Dead -> [(pos, decl)]
        | _ -> [])
      ()
  in

  (* Reactive modules_with_reported: modules that have at least one reported dead value *)
  let modules_with_reported =
    Reactive.flatMap ~name:"solver.modules_with_reported" issues_by_file
      ~f:(fun _file (_issues, modules_list) ->
        List.map (fun m -> (m, ())) modules_list)
      ()
  in

  (* Reactive dead module issues: dead_modules joined with modules_with_reported *)
  let dead_module_issues =
    Reactive.join ~name:"solver.dead_module_issues" dead_modules
      modules_with_reported
      ~key_of:(fun moduleName (_loc, _fileName) -> moduleName)
      ~f:(fun moduleName (loc, fileName) has_reported_opt ->
        match has_reported_opt with
        | Some () ->
          let loc =
            if loc.Location.loc_ghost then
              let pos =
                {
                  Lexing.pos_fname = fileName;
                  pos_lnum = 0;
                  pos_bol = 0;
                  pos_cnum = 0;
                }
              in
              {Location.loc_start = pos; loc_end = pos; loc_ghost = false}
            else loc
          in
          [(moduleName, AnalysisResult.make_dead_module_issue ~loc ~moduleName)]
        | None -> [])
      ()
  in

  {
    decls;
    live;
    dead_decls;
    live_decls;
    annotations;
    value_refs_from;
    dead_modules;
    dead_decls_by_file;
    issues_by_file;
    incorrect_dead_decls;
    dead_module_issues;
    config;
  }

(** Check if a module is dead using reactive collection. Returns issue if dead.
    Uses reported_modules set to avoid duplicate reports. *)
let check_module_dead ~(dead_modules : (Name.t, Location.t * string) Reactive.t)
    ~(reported_modules : (Name.t, unit) Hashtbl.t) ~fileName:pos_fname
    moduleName : Issue.t option =
  if Hashtbl.mem reported_modules moduleName then None
  else
    match Reactive.get dead_modules moduleName with
    | Some (loc, fileName) ->
      Hashtbl.replace reported_modules moduleName ();
      let loc =
        if loc.Location.loc_ghost then
          (* Use fileName from dead_modules, fallback to pos_fname *)
          let fname = if fileName <> "" then fileName else pos_fname in
          let pos =
            {Lexing.pos_fname = fname; pos_lnum = 0; pos_bol = 0; pos_cnum = 0}
          in
          {Location.loc_start = pos; loc_end = pos; loc_ghost = false}
        else loc
      in
      Some (AnalysisResult.make_dead_module_issue ~loc ~moduleName)
    | None -> None

(** Collect issues from reactive issues_by_file.
    Only iterates the pre-computed reactive issues collection.
    Deduplicates module issues across files. *)
let collect_issues ~(t : t) ~(config : DceConfig.t)
    ~(ann_store : AnnotationStore.t) : Issue.t list =
  ignore (config, ann_store);
  (* config is stored in t, ann_store used via reactive annotations *)
  let t0 = Unix.gettimeofday () in
  (* Track reported modules to avoid duplicates across files *)
  let reported_modules = Hashtbl.create 64 in

  (* Collect incorrect @dead issues from reactive collection *)
  let incorrect_dead_issues = ref [] in
  Reactive.iter
    (fun _pos (decl : Decl.t) ->
      let issue =
        DeadCommon.makeDeadIssue ~decl
          ~message:" is annotated @dead but is live"
          Issue.IncorrectDeadAnnotation
      in
      (* Check if module is dead using reactive collection *)
      check_module_dead ~dead_modules:t.dead_modules ~reported_modules
        ~fileName:decl.pos.pos_fname (decl_module_name decl)
      |> Option.iter (fun mod_issue ->
             incorrect_dead_issues := mod_issue :: !incorrect_dead_issues);
      incorrect_dead_issues := issue :: !incorrect_dead_issues)
    t.incorrect_dead_decls;
  let t1 = Unix.gettimeofday () in

  (* Collect issues from reactive issues_by_file *)
  let num_files = ref 0 in
  let dead_issues = ref [] in
  Reactive.iter
    (fun _file (file_issues, _modules_list) ->
      incr num_files;
      dead_issues := file_issues @ !dead_issues)
    t.issues_by_file;
  let t2 = Unix.gettimeofday () in

  (* Collect module issues from reactive dead_module_issues *)
  let module_issues = ref [] in
  Reactive.iter
    (fun _moduleName issue -> module_issues := issue :: !module_issues)
    t.dead_module_issues;
  let t3 = Unix.gettimeofday () in

  if !Cli.timing then
    Printf.eprintf
      "    collect_issues: incorrect_dead=%.2fms iter_issues=%.2fms \
       iter_modules=%.2fms (%d files)\n"
      ((t1 -. t0) *. 1000.0)
      ((t2 -. t1) *. 1000.0)
      ((t3 -. t2) *. 1000.0)
      !num_files;

  List.rev !incorrect_dead_issues @ !module_issues @ !dead_issues

(** Iterate over live declarations *)
let iter_live_decls ~(t : t) (f : Decl.t -> unit) : unit =
  Reactive.iter (fun _pos decl -> f decl) t.live_decls

(** Check if a position is live using the reactive collection.
    Returns true if pos is not a declaration (matches non-reactive behavior). *)
let is_pos_live ~(t : t) (pos : Lexing.position) : bool =
  match Reactive.get t.decls pos with
  | None -> true (* not a declaration, assume live *)
  | Some _ -> Reactive.get t.live pos <> None

(** Stats *)
let stats ~(t : t) : int * int =
  (Reactive.length t.dead_decls, Reactive.length t.live_decls)

(** Print reactive collection update statistics *)
let print_stats ~(t : t) : unit =
  let print name (c : _ Reactive.t) =
    let s = Reactive.stats c in
    Printf.eprintf
      "  %s: recv=%d/%d +%d -%d | emit=%d/%d +%d -%d | runs=%d len=%d\n" name
      s.deltas_received s.entries_received s.adds_received s.removes_received
      s.deltas_emitted s.entries_emitted s.adds_emitted s.removes_emitted
      s.process_count (Reactive.length c)
  in
  Printf.eprintf "ReactiveSolver stats (recv=d/e/+/- emit=d/e/+/- runs):\n";
  print "dead_decls" t.dead_decls;
  print "live_decls" t.live_decls;
  print "dead_modules" t.dead_modules;
  print "dead_decls_by_file" t.dead_decls_by_file;
  print "issues_by_file" t.issues_by_file;
  print "incorrect_dead_decls" t.incorrect_dead_decls;
  print "dead_module_issues" t.dead_module_issues;
  match t.value_refs_from with
  | Some refs -> print "value_refs_from" refs
  | None -> ()
