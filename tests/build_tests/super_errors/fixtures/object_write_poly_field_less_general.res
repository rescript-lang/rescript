/* Assigning to a polymorphic field checks the value against the field's
   scheme: a monomorphic function must not satisfy one instance while reads
   keep instantiating the unchanged scheme. */
type t = {@set "id": 'a. 'a => 'a}
let f = (o: t) => {
  o["id"] = _x => 1
  o["id"]("hello")
}
