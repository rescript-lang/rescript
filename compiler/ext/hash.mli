module Make (Key : Hashtbl.HashedType) : Hash_gen.S with type key = Key.t
