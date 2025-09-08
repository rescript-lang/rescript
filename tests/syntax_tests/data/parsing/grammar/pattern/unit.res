let () = ()
let () as x = ()
let (()) = ()
let (() as x) = ()
let (()) as x = ()
let ((): unit) = ()
let ((): unit) as x = ()
let (((): unit) as x)  = ()

switch x {
| () => ()
| () as _u => ()
| (()) => ()
| (()) as _u => ()
| (() as _u) => ()
| ((): unit) => ()
| ((): unit) as _u => ()
| (((): unit) as _u) => ()
}

for () in () to () { () }
for () as _u in () to () { () }
for (() in () to ()) { () }
for (() as _u in () to ()) { () }
for ((()) in () to ()) { () }
for ((() as _u) in () to ()) { () }
for ((()) as _u in () to ()) { () }
for (((): unit) in () to ()) { () }
for (((): unit) as _u in () to ()) { () }
for ((() as _u: unit) in () to ()) { () }
for (((() : unit) as _u) in () to ()) { () }

let f = () => ()
let f = (()) => ()
let f = (() as _u) => ()
let f = ((), ()) => ()
let f = (() as _u, () as _u) => ()
let f = (() : unit) => ()
let f = (() as _u : unit) => ()
let f = ((() : unit) as _u) => ()
let f = ((() : unit)) => ()
let f = (((() : unit) as _u)) => ()
