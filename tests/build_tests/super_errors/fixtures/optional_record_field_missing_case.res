type t = {b?: option<int>}

let a: t = Obj.magic()

let _ = switch a {
| {b: None} => ()
| {b: Some(_)} => ()
}
