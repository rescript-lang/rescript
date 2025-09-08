let _ = 1;
let _ | _ = 1;
let (_ : int) = 1;
let (_ | _ : int) = 1;
let ((_ | _) : int) = 1; // note: same ast as line above
let (_ :unit) | (_ : unit) = 1;
let _ as _x = 1;
let _ | _ as _x = 1;
let _ | (_ as _x) = 1;
let (_ | _) as _x = 1;
let _ as _y | _ as _x = 1;
let (_ as _y) | (_ as _x) = 1; // note: same ast as line above

switch () {
| _ => ()
| _ | _ => ()
| _ as _x => ()
| _ as _x | _ as _x => ()
| (_ : unit) => ()
| (_ : unit) | (_ : unit) => ()
}

let f = (_) => ()
let f = (_ as _x) => ()
let f = (_ : unit) => ()
let f = (_ : unit) => ()
let f = ((_ : unit) as _x) => ()

let g = (a, _) => ()
let g = (_, a) => ()

for _ in 0 to 10 { () }
for _ as _x in 0 to 10 { () }

for (_ in 0 to 10) { () }
for (_ as _x in 0 to 10) { () }

for ((_ : int) in 0 to 10) { () }
for ((_ : int) as _x in 0 to 10) { () }
