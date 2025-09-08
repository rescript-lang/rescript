let sort = (type s, module(Set), l) => ()
let sort = (type s, module(Set : Set.S with type elt = s), l) => ()
let sort = (type s, module(Set : Set.S with type elt = s and type elt2 = t), l) => ()
let foo = (module(Foo), baz) => Foo.bar(baz);
let bump_list = (type a, module(B : Bumpable with type t = a), l : list<a>) =>
  List.map(~f=B.bump(l))

switch x {
| module(Set) => ()
| module(Set : Set.S with type elt = s) => ()
| module(Set : Set.S with type elt = s and type elt2 = t) => ()
}
