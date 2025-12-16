(** Reactive dead code solver.
    
    Reactive pipeline: decls + live → dead_decls, live_decls
    
    Current status:
    - dead_decls, live_decls are reactive (zero recomputation on cache hit)
    - collect_issues iterates ALL dead_decls + live_decls every call
      (linear in their total size, not in changes)
    - Uses DeadCommon.reportDeclaration for isInsideReportedValue and hasRefBelow
    
    TODO for fully reactive issues:
    - isInsideReportedValue: needs reactive tracking of reported positions
      (currently relies on sequential iteration order via ReportingContext)
    - hasRefBelow: uses O(total_refs) linear scan of refs_from per dead decl;
      could use reactive refs_to index for O(1) lookup per decl
    - Module marking: needs reactive module dead/live tracking
      (currently uses mutable DeadModules.markDead/markLive)
    
    Missing issues in reactive mode (18 total on deadcode test):
    - Optional args: 18 issues missing (6 Redundant Optional Argument + 12 Unused Argument)
      Needs reactive cross_file_items + liveness filtering
    
    Correct in reactive mode:
    - Dead code issues: all match (362 issues)
    - Incorrect @dead detection: matches (1 issue) *)

type t = {
  dead_decls: (Lexing.position, Decl.t) Reactive.t;
  live_decls: (Lexing.position, Decl.t) Reactive.t;
  annotations: (Lexing.position, FileAnnotations.annotated_as) Reactive.t;
  value_refs_from: (Lexing.position, PosSet.t) Reactive.t option;
}

let create ~(decls : (Lexing.position, Decl.t) Reactive.t)
    ~(live : (Lexing.position, unit) Reactive.t)
    ~(annotations : (Lexing.position, FileAnnotations.annotated_as) Reactive.t)
    ~(value_refs_from : (Lexing.position, PosSet.t) Reactive.t option)
    ~(config : DceConfig.t) : t =
  ignore config;

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

  {dead_decls; live_decls; annotations; value_refs_from}

(** Collect issues from dead and live declarations.
    Uses DeadCommon.reportDeclaration for correct filtering.
    O(dead_decls + live_decls), not O(all_decls). *)
let collect_issues ~(t : t) ~(config : DceConfig.t) ~(ann_store : AnnotationStore.t)
    : Issue.t list =
  (* Mark dead declarations and collect them *)
  let dead_list = ref [] in
  Reactive.iter
    (fun _pos (decl : Decl.t) ->
      decl.resolvedDead <- Some true;
      decl.path
      |> DeadModules.markDead ~config
           ~isType:(decl.declKind |> Decl.Kind.isType)
           ~loc:decl.moduleLoc;
      (* Check annotation to decide if we report.
         Don't report if @live, @genType, or @dead (user knows it's dead) *)
      let should_report =
        match Reactive.get t.annotations decl.pos with
        | Some FileAnnotations.Live -> false
        | Some FileAnnotations.GenType -> false
        | Some FileAnnotations.Dead -> false  (* @dead = user knows, don't warn *)
        | None -> true
      in
      if not should_report then decl.report <- false;
      dead_list := decl :: !dead_list)
    t.dead_decls;

  (* Mark live declarations *)
  let incorrect_dead_issues = ref [] in
  Reactive.iter
    (fun _pos (decl : Decl.t) ->
      decl.resolvedDead <- Some false;
      decl.path
      |> DeadModules.markLive ~config
           ~isType:(decl.declKind |> Decl.Kind.isType)
           ~loc:decl.moduleLoc;
      (* Check for incorrect @dead annotation on live decl *)
      if AnnotationStore.is_annotated_dead ann_store decl.pos then
        let issue =
          DeadCommon.makeDeadIssue ~decl
            ~message:" is annotated @dead but is live"
            Issue.IncorrectDeadAnnotation
        in
        decl.path
        |> DcePath.toModuleName ~isType:(decl.declKind |> Decl.Kind.isType)
        |> DeadModules.checkModuleDead ~config ~fileName:decl.pos.pos_fname
        |> Option.iter (fun mod_issue ->
               incorrect_dead_issues := mod_issue :: !incorrect_dead_issues);
        incorrect_dead_issues := issue :: !incorrect_dead_issues)
    t.live_decls;

  (* Sort and generate issues using DeadCommon.reportDeclaration *)
  let sorted_dead = !dead_list |> List.fast_sort Decl.compareForReporting in
  let transitive = config.DceConfig.run.transitive in
  let hasRefBelow =
    match t.value_refs_from with
    | None -> fun _ -> false
    | Some refs_from ->
      DeadCommon.make_hasRefBelow ~transitive ~iter_value_refs_from:(fun f ->
          Reactive.iter f refs_from)
  in
  let reporting_ctx = DeadCommon.ReportingContext.create () in
  let dead_issues =
    sorted_dead
    |> List.concat_map (fun decl ->
           DeadCommon.reportDeclaration ~config ~hasRefBelow reporting_ctx decl)
  in

  List.rev !incorrect_dead_issues @ dead_issues

(** Stats *)
let stats ~(t : t) : int * int =
  (Reactive.length t.dead_decls, Reactive.length t.live_decls)
