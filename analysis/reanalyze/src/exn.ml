(* An exception is its identity, as the compiler records it in [@res.throws]
   (see [Throws_attribute]), with a name to show in messages. Two names of one
   exception, as [Lib.E] and [E] after [open Lib], compare equal. *)
type t = {id: string; name: string}

let compare a b = String.compare a.id b.id
let make ~id ~name = {id; name}
let from_string s = {id = s; name = s}
let match_failure = from_string "Match_failure"
let to_string e = e.name

(* The exception a pattern or expression constructs, named as written *)
let of_constructor lid (cstr : Types.constructor_description) =
  let name = lid |> Longident.flatten |> String.concat "." in
  match Throws_attribute.identity cstr with
  | Some id -> {id; name}
  | None -> from_string name

let with_name name e = {e with name}

module Table = Hashtbl.Make (struct
  type nonrec t = t

  let equal a b = String.equal a.id b.id
  let hash a = Hashtbl.hash a.id
end)
