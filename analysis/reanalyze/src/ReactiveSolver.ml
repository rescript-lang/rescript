(** Reactive dead code solver.
    
    Reactive pipeline: decls + live → dead_decls, live_decls, dead_modules, dead_decls_by_file, issues_by_file
    
    Current status:
    - All collections are reactive (zero recomputation on cache hit for unchanged files)
    - dead_decls, live_decls = decls partitioned by liveness (reactive join)
    - dead_modules = modules with dead decls but no live decls (reactive anti-join)
    - dead_decls_by_file = dead decls grouped by file (reactive flatMap with merge)
    - value_refs_from_by_file = refs grouped by source file (reactive flatMap with merge)
    - issues_by_file = per-file issue generation (reactive flatMap)
    - is_pos_live uses reactive live collection (no resolvedDead mutation)
    - shouldReport callback replaces report field mutation (no mutation needed)
    - isInsideReportedValue is per-file only, so files are independent
    - hasRefBelow uses per-file refs: O(file_refs) per dead decl (was O(total_refs))
    - Module issues generated in collect_issues from dead_modules + modules_with_reported_values
    
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
  dead_modules: (Name.t, Location.t) Reactive.t;
      (** Modules where all declarations are dead. Reactive anti-join. *)
  dead_decls_by_file: (string, Decl.t list) Reactive.t;
      (** Dead declarations grouped by file. Reactive per-file grouping. *)
  issues_by_file: (string, Issue.t list * Name.t list) Reactive.t;
      (** Dead code issues grouped by file. Reactive per-file issue generation.
          First component: value/type/exception issues.
          Second component: modules with at least one reported value (for module issue generation). *)
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
    Reactive.join decls live
      ~key_of:(fun pos _decl -> pos)
      ~f:(fun pos decl live_opt ->
        match live_opt with
        | None -> [(pos, decl)]
        | Some () -> [])
      ()
  in

  (* live_decls = decls where in live (reactive join) *)
  let live_decls =
    Reactive.join decls live
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
      Reactive.flatMap dead_decls ~f:(fun _ _ -> []) ()
    else
      (* modules_with_dead: (moduleName, loc) for each module with dead decls *)
      let modules_with_dead =
        Reactive.flatMap dead_decls
          ~f:(fun _pos decl -> [(decl_module_name decl, decl.moduleLoc)])
          ~merge:(fun loc1 _loc2 -> loc1) (* keep first location *)
          ()
      in
      (* modules_with_live: (moduleName, ()) for each module with live decls *)
      let modules_with_live =
        Reactive.flatMap live_decls
          ~f:(fun _pos decl -> [(decl_module_name decl, ())])
          ()
      in
      (* Anti-join: modules in dead but not in live *)
      Reactive.join modules_with_dead modules_with_live
        ~key_of:(fun modName _loc -> modName)
        ~f:(fun modName loc live_opt ->
          match live_opt with
          | None -> [(modName, loc)] (* dead: no live decls *)
          | Some () -> []) (* live: has at least one live decl *)
        ()
  in

  (* Reactive per-file grouping of dead declarations *)
  let dead_decls_by_file =
    Reactive.flatMap dead_decls
      ~f:(fun _pos decl -> [(decl.pos.Lexing.pos_fname, [decl])])
      ~merge:(fun decls1 decls2 -> decls1 @ decls2)
      ()
  in

  (* Reactive per-file grouping of value refs (for hasRefBelow optimization) *)
  let transitive = config.DceConfig.run.transitive in
  let value_refs_from_by_file =
    match value_refs_from with
    | None -> None
    | Some refs_from ->
      Some (
        Reactive.flatMap refs_from
          ~f:(fun posFrom posToSet -> [(posFrom.Lexing.pos_fname, [(posFrom, posToSet)])])
          ~merge:(fun refs1 refs2 -> refs1 @ refs2)
          ()
      )
  in

  (* Reactive per-file issues - recomputed when dead_decls_by_file changes.
     Returns (file, (value_issues, modules_with_reported_values)) where
     modules_with_reported_values are modules that have at least one reported dead value.
     Module issues are generated separately in collect_issues using dead_modules.
     
     hasRefBelow now uses per-file refs: O(file_refs) instead of O(total_refs). *)
  let issues_by_file =
    Reactive.flatMap dead_decls_by_file
      ~f:(fun file decls ->
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
        (* Per-file hasRefBelow: only scan refs from this file *)
        let hasRefBelow =
          if transitive then fun _ -> false
          else
            match value_refs_from_by_file with
            | None -> fun _ -> false
            | Some refs_by_file ->
              let file_refs = Reactive.get refs_by_file file in
              fun decl ->
                match file_refs with
                | None -> false
                | Some refs_list ->
                  List.exists (fun (posFrom, posToSet) ->
                    PosSet.mem decl.Decl.pos posToSet &&
                    DeadCommon.refIsBelow decl posFrom
                  ) refs_list
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
        let modules_list = Hashtbl.fold (fun m () acc -> m :: acc) modules_with_values [] in
        [(file, (file_issues, modules_list))])
      ()
  in

  {decls; live; dead_decls; live_decls; annotations; value_refs_from; dead_modules; dead_decls_by_file; issues_by_file; config}

(** Check if a module is dead using reactive collection. Returns issue if dead.
    Uses reported_modules set to avoid duplicate reports. *)
let check_module_dead ~(dead_modules : (Name.t, Location.t) Reactive.t)
    ~(reported_modules : (Name.t, unit) Hashtbl.t) ~fileName:pos_fname moduleName
    : Issue.t option =
  if Hashtbl.mem reported_modules moduleName then None
  else
    match Reactive.get dead_modules moduleName with
    | Some loc ->
      Hashtbl.replace reported_modules moduleName ();
      let loc =
        if loc.Location.loc_ghost then
          let pos =
            {Lexing.pos_fname; pos_lnum = 0; pos_bol = 0; pos_cnum = 0}
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
  ignore config; (* config is stored in t *)
  let t0 = Unix.gettimeofday () in
  (* Track reported modules to avoid duplicates across files *)
  let reported_modules = Hashtbl.create 64 in

  (* Check live declarations for incorrect @dead *)
  let incorrect_dead_issues = ref [] in
  Reactive.iter
    (fun _pos (decl : Decl.t) ->
      (* Check for incorrect @dead annotation on live decl *)
      if AnnotationStore.is_annotated_dead ann_store decl.pos then (
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
        incorrect_dead_issues := issue :: !incorrect_dead_issues))
    t.live_decls;
  let t1 = Unix.gettimeofday () in

  (* Collect issues from reactive issues_by_file *)
  let num_files = ref 0 in
  let dead_issues = ref [] in
  (* Track modules that have at least one reported value (for module issue generation) *)
  let modules_with_reported_values : (Name.t, unit) Hashtbl.t = Hashtbl.create 64 in
  Reactive.iter
    (fun _file (file_issues, modules_list) ->
      incr num_files;
      dead_issues := file_issues @ !dead_issues;
      (* Collect modules that have reported values *)
      List.iter
        (fun moduleName -> Hashtbl.replace modules_with_reported_values moduleName ())
        modules_list)
    t.issues_by_file;
  let t2 = Unix.gettimeofday () in

  (* Generate module issues: only for modules that are dead AND have a reported value *)
  let module_issues = ref [] in
  let reported_modules : (Name.t, unit) Hashtbl.t = Hashtbl.create 64 in
  Reactive.iter
    (fun moduleName loc ->
      (* Only report if module has at least one reported dead value *)
      if Hashtbl.mem modules_with_reported_values moduleName then
        if not (Hashtbl.mem reported_modules moduleName) then (
          Hashtbl.replace reported_modules moduleName ();
          let loc =
            if loc.Location.loc_ghost then
              let pos_fname = loc.loc_start.pos_fname in
              let pos =
                {Lexing.pos_fname; pos_lnum = 0; pos_bol = 0; pos_cnum = 0}
              in
              {Location.loc_start = pos; loc_end = pos; loc_ghost = false}
            else loc
          in
          module_issues := AnalysisResult.make_dead_module_issue ~loc ~moduleName :: !module_issues))
    t.dead_modules;
  let t3 = Unix.gettimeofday () in

  if !Cli.timing then
    Printf.eprintf
      "    collect_issues: iter_live=%.2fms iter_issues=%.2fms iter_modules=%.2fms (%d files)\n"
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
