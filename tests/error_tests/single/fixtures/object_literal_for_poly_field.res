/* An object literal cannot satisfy a polymorphic field annotation: the
   literal's field is monomorphic. Pins the current Tpoly([]) vs
   Tpoly(['a]) unification failure. */
type t = {"f": 'a. 'a => 'a}
let x: t = {"f": x => x}
