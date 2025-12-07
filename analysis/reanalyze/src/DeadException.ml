open DeadCommon
open Common

type item = {exceptionPath: Path.t; locFrom: Location.t}

let delayedItems = ref []
let declarations = Hashtbl.create 1

let add ~config ~decls ~file ~path ~loc ~(strLoc : Location.t) name =
  let exceptionPath = name :: path in
  Hashtbl.add declarations exceptionPath loc;
  name
  |> addDeclaration_ ~config ~decls ~file ~posEnd:strLoc.loc_end
       ~posStart:strLoc.loc_start ~declKind:Exception
       ~moduleLoc:(ModulePath.getCurrent ()).loc ~path ~loc

let forceDelayedItems ~config =
  let items = !delayedItems |> List.rev in
  delayedItems := [];
  items
  |> List.iter (fun {exceptionPath; locFrom} ->
         match Hashtbl.find_opt declarations exceptionPath with
         | None -> ()
         | Some locTo ->
           (* Delayed exception references don't need a binding context; use an empty state. *)
           addValueReference ~config ~binding:Location.none
             ~addFileReference:true ~locFrom ~locTo)

let markAsUsed ~config ~(binding : Location.t) ~(locFrom : Location.t)
    ~(locTo : Location.t) path_ =
  if locTo.loc_ghost then
    (* Probably defined in another file, delay processing and check at the end *)
    let exceptionPath =
      path_ |> Path.fromPathT |> Path.moduleToImplementation
    in
    delayedItems := {exceptionPath; locFrom} :: !delayedItems
  else addValueReference ~config ~binding ~addFileReference:true ~locFrom ~locTo
