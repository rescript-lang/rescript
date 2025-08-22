open SharedTypes

let unique_items (structure : Module.structure) : Module.item list =
  let namesUsed = Hashtbl.create 10 in
  structure.items
  |> List.filter (fun (it : Module.item) ->
         if Hashtbl.mem namesUsed it.name then false
         else (
           Hashtbl.add namesUsed it.name ();
           true))
