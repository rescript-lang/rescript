open DeadCommon

let active () = true

let addFunctionReference ~config ~decls ~cross_file ~(locFrom : Location.t)
    ~(locTo : Location.t) =
  if active () then
    let posTo = locTo.loc_start in
    let posFrom = locFrom.loc_start in
    let ownsNonEmptyOptionalArgs pos =
      match Declarations.find_opt_builder decls pos with
      | Some {declKind = Value {ownsOptionalArgs; optionalArgs}} ->
        ownsOptionalArgs && not (OptionalArgs.isEmpty optionalArgs)
      | _ -> false
    in
    let bothOwnNonEmptyOptionalArgs () =
      ownsNonEmptyOptionalArgs posFrom && ownsNonEmptyOptionalArgs posTo
    in
    (* Only declarations that own optional args should participate in
       optional-arg state merging. A function-valued alias like
       [let f = useNotification()] can have an optional-arg type, but it is not
       the declaration site that should receive warnings. *)
    let shouldAdd =
      if posTo.pos_fname <> posFrom.pos_fname then
        if
          fileIsImplementationOf posTo.pos_fname posFrom.pos_fname
          || fileIsImplementationOf posFrom.pos_fname posTo.pos_fname
        then ownsNonEmptyOptionalArgs posTo
        else bothOwnNonEmptyOptionalArgs ()
      else bothOwnNonEmptyOptionalArgs ()
    in
    if shouldAdd then (
      if config.DceConfig.cli.debug then
        Log_.item "OptionalArgs.addFunctionReference %s %s@."
          (posFrom |> Pos.toString) (posTo |> Pos.toString);
      CrossFileItems.add_function_reference cross_file ~pos_from:posFrom
        ~pos_to:posTo)

let rec hasOptionalArgs (texpr : Types.type_expr) =
  match texpr.desc with
  | _ when not (active ()) -> false
  | Tarrow ({lbl = Optional _}, _tTo, _, _) -> true
  | Tarrow (_, tTo, _, _) -> hasOptionalArgs tTo
  | Tlink t -> hasOptionalArgs t
  | Tsubst t -> hasOptionalArgs t
  | _ -> false

let rec fromTypeExpr (texpr : Types.type_expr) =
  match texpr.desc with
  | _ when not (active ()) -> []
  | Tarrow ({lbl = Optional {txt = s}}, tTo, _, _) -> s :: fromTypeExpr tTo
  | Tarrow (_, tTo, _, _) -> fromTypeExpr tTo
  | Tlink t -> fromTypeExpr t
  | Tsubst t -> fromTypeExpr t
  | _ -> []

let rec fromTypeExprWithArity (texpr : Types.type_expr) arity =
  if arity <= 0 then []
  else
    match texpr.desc with
    | _ when not (active ()) -> []
    | Tarrow ({lbl = Optional {txt = s}}, tTo, _, _) ->
      s :: fromTypeExprWithArity tTo (arity - 1)
    | Tarrow (_, tTo, _, _) -> fromTypeExprWithArity tTo (arity - 1)
    | Tlink t -> fromTypeExprWithArity t arity
    | Tsubst t -> fromTypeExprWithArity t arity
    | _ -> []

let addReferences ~config ~decls ~cross_file ~(locFrom : Location.t)
    ~(locTo : Location.t) ~(binding : Location.t) ~path (argNames, argNamesMaybe)
    =
  if active () then
    let posTo = locTo.loc_start in
    let posFrom = binding.loc_start in
    let callPos = locFrom.loc_start in
    let shouldAdd =
      if posTo.pos_fname <> callPos.pos_fname then true
      else
        match Declarations.find_opt_builder decls posTo with
        | Some {declKind = Value {ownsOptionalArgs; optionalArgs}} ->
          ownsOptionalArgs && not (OptionalArgs.isEmpty optionalArgs)
        | _ -> false
    in
    if shouldAdd then (
      CrossFileItems.add_optional_arg_call cross_file ~pos_from:posFrom
        ~pos_to:posTo ~arg_names:argNames ~arg_names_maybe:argNamesMaybe;
      if config.DceConfig.cli.debug then
        Log_.item
          "DeadOptionalArgs.addReferences %s called with optional argNames:%s \
           argNamesMaybe:%s %s@."
          (path |> DcePath.fromPathT |> DcePath.toString)
          (argNames |> String.concat ", ")
          (argNamesMaybe |> String.concat ", ")
          (callPos |> Pos.toString))

(** Check for optional args issues. Returns issues instead of logging.
    Uses optional_args_state map for final computed state. *)
let check ~optional_args_state ~ann_store ~config:_ decl : Issue.t list =
  match decl with
  | {Decl.declKind = Value {optionalArgs}}
    when active ()
         && not
              (AnnotationStore.is_annotated_gentype_or_live ann_store decl.pos)
    ->
    (* Look up computed state from map, fall back to declaration's initial state *)
    let state =
      match OptionalArgsState.find_opt optional_args_state decl.pos with
      | Some s -> s
      | None -> optionalArgs
    in
    let loc = decl |> declGetLoc in
    let unused_issues =
      OptionalArgs.foldUnused
        (fun s acc ->
          let issue : Issue.t =
            {
              name = "Warning Unused Argument";
              severity = Warning;
              loc;
              description =
                DeadOptional
                  {
                    deadOptional = WarningUnusedArgument;
                    message =
                      Format.asprintf
                        "optional argument @{<info>%s@} of function \
                         @{<info>%s@} is never used"
                        s
                        (decl.path |> DcePath.withoutHead);
                  };
            }
          in
          issue :: acc)
        state []
    in
    let redundant_issues =
      OptionalArgs.foldAlwaysUsed
        (fun s nCalls acc ->
          let issue : Issue.t =
            {
              name = "Warning Redundant Optional Argument";
              severity = Warning;
              loc;
              description =
                DeadOptional
                  {
                    deadOptional = WarningRedundantOptionalArgument;
                    message =
                      Format.asprintf
                        "optional argument @{<info>%s@} of function \
                         @{<info>%s@} is always supplied (%d calls)"
                        s
                        (decl.path |> DcePath.withoutHead)
                        nCalls;
                  };
            }
          in
          issue :: acc)
        state []
    in
    (* Reverse to maintain original order from iterUnused/iterAlwaysUsed *)
    List.rev unused_issues @ List.rev redundant_issues
  | _ -> []
