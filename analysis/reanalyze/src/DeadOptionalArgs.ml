open DeadCommon
open Common

let active () = true

type item = {
  posTo: Lexing.position;
  argNames: string list;
  argNamesMaybe: string list;
}

let delayedItems = (ref [] : item list ref)
let functionReferences = (ref [] : (Lexing.position * Lexing.position) list ref)

let addFunctionReference ~(locFrom : Location.t) ~(locTo : Location.t) =
  if active () then
    let posTo = locTo.loc_start in
    let posFrom = locFrom.loc_start in
    let shouldAdd =
      match PosHash.find_opt decls posTo with
      | Some {declKind = Value {optionalArgs}} ->
        not (OptionalArgs.isEmpty optionalArgs)
      | _ -> false
    in
    if shouldAdd then (
      if !Common.Cli.debug then
        Log_.item "OptionalArgs.addFunctionReference %s %s@."
          (posFrom |> posToString) (posTo |> posToString);
      functionReferences := (posFrom, posTo) :: !functionReferences)

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
  | Tarrow ({lbl = Optional s}, tTo, _, _) -> s :: fromTypeExpr tTo
  | Tarrow (_, tTo, _, _) -> fromTypeExpr tTo
  | Tlink t -> fromTypeExpr t
  | Tsubst t -> fromTypeExpr t
  | _ -> []

let addReferences ~(locFrom : Location.t) ~(locTo : Location.t) ~path
    (argNames, argNamesMaybe) =
  if active () then (
    let posTo = locTo.loc_start in
    let posFrom = locFrom.loc_start in
    delayedItems := {posTo; argNames; argNamesMaybe} :: !delayedItems;
    if !Common.Cli.debug then
      Log_.item
        "DeadOptionalArgs.addReferences %s called with optional argNames:%s \
         argNamesMaybe:%s %s@."
        (path |> Path.fromPathT |> Path.toString)
        (argNames |> String.concat ", ")
        (argNamesMaybe |> String.concat ", ")
        (posFrom |> posToString))

let forceDelayedItems () =
  let items = !delayedItems |> List.rev in
  delayedItems := [];
  items
  |> List.iter (fun {posTo; argNames; argNamesMaybe} ->
         match PosHash.find_opt decls posTo with
         | Some {declKind = Value r} ->
           r.optionalArgs |> OptionalArgs.call ~argNames ~argNamesMaybe
         | _ -> ());
  let fRefs = !functionReferences |> List.rev in
  functionReferences := [];
  fRefs
  |> List.iter (fun (posFrom, posTo) ->
         match
           (PosHash.find_opt decls posFrom, PosHash.find_opt decls posTo)
         with
         | Some {declKind = Value rFrom}, Some {declKind = Value rTo} ->
           OptionalArgs.combine rFrom.optionalArgs rTo.optionalArgs
         | _ -> ())

let check decl =
  match decl with
  | {declKind = Value {optionalArgs}}
    when active ()
         && not (ProcessDeadAnnotations.isAnnotatedGenTypeOrLive decl.pos) ->
    optionalArgs
    |> OptionalArgs.iterUnused (fun s ->
           Log_.warning ~loc:(decl |> declGetLoc)
             (DeadOptional
                {
                  deadOptional = WarningUnusedArgument;
                  message =
                    Format.asprintf
                      "optional argument @{<info>%s@} of function @{<info>%s@} \
                       is never used"
                      s
                      (decl.path |> Path.withoutHead);
                }));
    optionalArgs
    |> OptionalArgs.iterAlwaysUsed (fun s nCalls ->
           Log_.warning ~loc:(decl |> declGetLoc)
             (DeadOptional
                {
                  deadOptional = WarningRedundantOptionalArgument;
                  message =
                    Format.asprintf
                      "optional argument @{<info>%s@} of function @{<info>%s@} \
                       is always supplied (%d calls)"
                      s
                      (decl.path |> Path.withoutHead)
                      nCalls;
                }))
  | _ -> ()
