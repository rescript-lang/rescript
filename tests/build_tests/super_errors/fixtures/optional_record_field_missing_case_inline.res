type t = A({b?: option<int>})

let v: t = Obj.magic()

let _ = switch v {
| A({b: None}) => ()
| A({b: Some(_)}) => ()
}
