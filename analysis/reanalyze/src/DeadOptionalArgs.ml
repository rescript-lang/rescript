open DeadCommon
open Common

let collector = ref None

let with_collector c f =
  let previous = !collector in
  collector := Some c;
  Fun.protect ~finally:(fun () -> collector := previous) f

let find_decl pos =
  match !collector with
  | Some c -> Collector.find_decl c pos
  | None -> PosHash.find_opt decls pos

let replace_decl decl =
  match !collector with
  | Some c -> Collector.replace_decl c decl
  | None -> PosHash.replace decls decl.pos decl

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
      match find_decl posTo with
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
  | Tarrow ({lbl = Optional {txt = s}}, tTo, _, _) -> s :: fromTypeExpr tTo
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
         match find_decl posTo with
         | Some ({declKind = Value r} as decl) ->
           r.optionalArgs |> OptionalArgs.call ~argNames ~argNamesMaybe;
           replace_decl decl
         | _ -> ());
  let fRefs = !functionReferences |> List.rev in
  functionReferences := [];
  fRefs
  |> List.iter (fun (posFrom, posTo) ->
         match (find_decl posFrom, find_decl posTo) with
         | ( Some ({declKind = Value rFrom} as declFrom),
             Some ({declKind = Value rTo} as declTo) ) ->
           OptionalArgs.combine rFrom.optionalArgs rTo.optionalArgs;
           replace_decl declFrom;
           replace_decl declTo
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
