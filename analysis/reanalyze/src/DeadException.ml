open DeadCommon
open Common
open Collected_types

type item = {exceptionPath: Path.t; locFrom: Location.t}

let delayedItems = ref []
let declarations = Hashtbl.create 1
let collector_ref = ref None

let with_collector c f =
  let previous = !collector_ref in
  collector_ref := Some c;
  Fun.protect ~finally:(fun () -> collector_ref := previous) f

let point_location loc =
  {
    Location.loc_start = loc.Location.loc_start;
    loc_end = loc.Location.loc_start;
    loc_ghost = false;
  }

let record_value_reference ~(locFrom : Location.t) ~(locTo : Location.t)
    ?(target_path = None) () =
  match !collector_ref with
  | Some c ->
      let locFrom =
        match !Current.lastBinding = Location.none with
        | true -> locFrom
        | false -> !Current.lastBinding
      in
      if not locFrom.loc_ghost then
        Collector.add_value_reference c
          Collected_types.
            {
              loc_from = point_location locFrom;
              loc_to = point_location locTo;
              add_file_reference = true;
              target_path;
            }
  | None -> addValueReference ~addFileReference:true ~locFrom ~locTo

let add ~collector ~path ~loc ~(strLoc : Location.t) name =
  let exceptionPath = name :: path in
  Hashtbl.add declarations exceptionPath loc;
  Collector.add_decl collector
    (General_decl
       {
         name;
         path;
         loc;
         module_loc = (ModulePath.getCurrent ()).loc;
         decl_kind = Exception;
         pos_adjustment = Nothing;
         pos_start = Some strLoc.loc_start;
         pos_end = Some strLoc.loc_end;
       })

let forceDelayedItems () =
  let items = !delayedItems |> List.rev in
  delayedItems := [];
  items
  |> List.iter (fun {exceptionPath; locFrom} ->
         match Hashtbl.find_opt declarations exceptionPath with
         | None -> ()
         | Some locTo -> record_value_reference ~locFrom ~locTo
             ~target_path:(Some exceptionPath) ())

let replay_delayed_items ~collector =
  let items = !delayedItems |> List.rev in
  with_collector collector (fun () ->
      items
      |> List.iter (fun {exceptionPath; locFrom} ->
             match Hashtbl.find_opt declarations exceptionPath with
             | None -> ()
             | Some locTo -> record_value_reference ~locFrom ~locTo
                 ~target_path:(Some exceptionPath) ()))

let markAsUsed ~(locFrom : Location.t) ~(locTo : Location.t) path_ =
  if locTo.loc_ghost then
    (* Probably defined in another file, delay processing and check at the end *)
    let exceptionPath =
      path_ |> Path.fromPathT |> Path.moduleToImplementation
    in
    delayedItems := {exceptionPath; locFrom} :: !delayedItems
  else
    let target_path =
      Some (path_ |> Path.fromPathT |> Path.moduleToImplementation)
    in
    (match target_path with
    | None -> record_value_reference ~locFrom ~locTo ()
    | Some path ->
        record_value_reference ~locFrom ~locTo ~target_path:(Some path) ())
