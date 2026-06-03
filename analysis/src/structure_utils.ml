open Shared_types

let unique_items (structure : Module.structure) : Module.item list =
  let names_used = Hashtbl.create 10 in
  structure.items
  |> List.filter (fun (it : Module.item) ->
         if Hashtbl.mem names_used it.name then false
         else (
           Hashtbl.add names_used it.name ();
           true))
