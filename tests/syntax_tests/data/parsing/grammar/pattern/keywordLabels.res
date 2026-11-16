let {type: kind} = r
let {x, type: kind, M.open: isOpen} = r
let {type: ?kind} = r
let {true: t, false: f} = r

switch x {
| {type: "a", in: ?i} => 1
| #type | #module(_) => 2
| #true => 3
| _ => 4
}
