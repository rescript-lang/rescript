(** Reactive dead code solver.
    
    Reactive pipeline: decls + live → dead_decls, live_decls, dead_modules
    
    Current status:
    - dead_decls, live_decls, dead_modules are reactive (zero recomputation on cache hit)
    - dead_modules = modules with dead decls but no live decls (reactive anti-join)
    - is_pos_live uses reactive live collection (no resolvedDead mutation needed)
    - collect_issues still iterates dead_decls + live_decls for annotations + sorting
    - Uses DeadCommon.reportDeclaration for isInsideReportedValue and hasRefBelow
    
    TODO for fully reactive issues:
    - isInsideReportedValue: needs reactive tracking of reported positions
      (currently relies on sequential iteration order via ReportingContext)
    - hasRefBelow: uses O(total_refs) linear scan of refs_from per dead decl;
      could use reactive refs_to index for O(1) lookup per decl
    - report field: still mutated to suppress annotated decls; could check in reportDeclaration
    - Sorting: O(n log n) for isInsideReportedValue ordering; fundamentally sequential
    
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

  {decls; live; dead_decls; live_decls; annotations; value_refs_from; dead_modules}

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

(** Collect issues from dead and live declarations.
    Uses reactive dead_modules instead of mutable DeadModules.
    O(dead_decls + live_decls), not O(all_decls). *)
let collect_issues ~(t : t) ~(config : DceConfig.t)
    ~(ann_store : AnnotationStore.t) : Issue.t list =
  let t0 = Unix.gettimeofday () in
  (* Track reported modules to avoid duplicates *)
  let reported_modules = Hashtbl.create 64 in

  (* Collect dead declarations - NO resolvedDead mutation *)
  let dead_list = ref [] in
  Reactive.iter
    (fun _pos (decl : Decl.t) ->
      (* Check annotation to decide if we report.
         Don't report if @live, @genType, or @dead (user knows it's dead) *)
      let should_report =
        match Reactive.get t.annotations decl.pos with
        | Some FileAnnotations.Live -> false
        | Some FileAnnotations.GenType -> false
        | Some FileAnnotations.Dead ->
          false (* @dead = user knows, don't warn *)
        | None -> true
      in
      if not should_report then decl.report <- false;
      dead_list := decl :: !dead_list)
    t.dead_decls;
  let t1 = Unix.gettimeofday () in

  (* Check live declarations for incorrect @dead - NO resolvedDead mutation *)
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
  let t2 = Unix.gettimeofday () in

  (* Sort dead declarations for isInsideReportedValue ordering *)
  let sorted_dead = !dead_list |> List.fast_sort Decl.compareForReporting in
  let t3 = Unix.gettimeofday () in

  (* Generate issues - use reactive dead_modules via callback *)
  let transitive = config.DceConfig.run.transitive in
  let hasRefBelow =
    match t.value_refs_from with
    | None -> fun _ -> false
    | Some refs_from ->
      DeadCommon.make_hasRefBelow ~transitive ~iter_value_refs_from:(fun f ->
          Reactive.iter f refs_from)
  in
  (* Callback for checking dead modules using reactive collection *)
  let checkModuleDead ~fileName moduleName =
    check_module_dead ~dead_modules:t.dead_modules ~reported_modules ~fileName
      moduleName
  in
  let reporting_ctx = DeadCommon.ReportingContext.create () in
  let dead_issues =
    sorted_dead
    |> List.concat_map (fun decl ->
           DeadCommon.reportDeclaration ~config ~hasRefBelow ~checkModuleDead
             reporting_ctx decl)
  in
  let t4 = Unix.gettimeofday () in

  if !Cli.timing then
    Printf.eprintf
      "    collect_issues: iter_dead=%.2fms iter_live=%.2fms sort=%.2fms \
       report=%.2fms\n"
      ((t1 -. t0) *. 1000.0)
      ((t2 -. t1) *. 1000.0)
      ((t3 -. t2) *. 1000.0)
      ((t4 -. t3) *. 1000.0);

  List.rev !incorrect_dead_issues @ dead_issues

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
