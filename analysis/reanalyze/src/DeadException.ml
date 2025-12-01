open DeadCommon
open Common

type item = {exceptionPath: Path.t; locFrom: Location.t}

let delayedItems = ref []
let declarations = Hashtbl.create 1

let add ~path ~loc ~(strLoc : Location.t) name =
  let exceptionPath = name :: path in
  Hashtbl.add declarations exceptionPath loc;
  name
  |> addDeclaration_ ~posEnd:strLoc.loc_end ~posStart:strLoc.loc_start
       ~declKind:Exception ~moduleLoc:(ModulePath.getCurrent ()).loc ~path ~loc

let forceDelayedItems () =
  let items = !delayedItems |> List.rev in
  delayedItems := [];
  items
  |> List.iter (fun {exceptionPath; locFrom} ->
         match Hashtbl.find_opt declarations exceptionPath with
         | None -> ()
         | Some locTo ->
           (* Delayed exception references don't need a binding context; use an empty state. *)
           DeadCommon.addValueReference ~binding:Location.none
             ~addFileReference:true ~locFrom ~locTo)

let markAsUsed ~(binding : Location.t) ~(locFrom : Location.t)
    ~(locTo : Location.t) path_ =
  if locTo.loc_ghost then
    (* Probably defined in another file, delay processing and check at the end *)
    let exceptionPath =
      path_ |> Path.fromPathT |> Path.moduleToImplementation
    in
    delayedItems := {exceptionPath; locFrom} :: !delayedItems
  else
    DeadCommon.addValueReference ~binding ~addFileReference:true ~locFrom ~locTo
