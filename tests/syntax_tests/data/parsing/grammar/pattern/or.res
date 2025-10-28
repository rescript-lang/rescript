switch x {
| Blue | Red => ()
| (Blue | Red) as colour => ()
| Blue as c1 | Red as c2 => ()
| (Blue as c1) | (Red as c2) => ()
| exception Exit | exception Continue => ()
| exception (Exit | exception Continue) => ()
| ("a" | "b" | "c") as #...f => ()
| (1 | 2) as #...n => ()
}
