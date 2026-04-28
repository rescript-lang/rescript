type t = {b?: option<option<int>>}

let a: t = Obj.magic()

let _ = switch a {
| {b: Some(Some(_))} => ()
}
