open DeadCommon
open Common
open Collected_types

type item = {exceptionPath: Path.t; locFrom: Location.t}

let delayedItems = ref []
let declarations = Hashtbl.create 1

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
         | Some locTo ->
           addValueReference ~addFileReference:true ~locFrom ~locTo)

let markAsUsed ~(locFrom : Location.t) ~(locTo : Location.t) path_ =
  if locTo.loc_ghost then
    (* Probably defined in another file, delay processing and check at the end *)
    let exceptionPath =
      path_ |> Path.fromPathT |> Path.moduleToImplementation
    in
    delayedItems := {exceptionPath; locFrom} :: !delayedItems
  else addValueReference ~addFileReference:true ~locFrom ~locTo
