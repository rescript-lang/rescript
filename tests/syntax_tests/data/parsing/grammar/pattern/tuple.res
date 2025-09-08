let (1, 2) = ()
let ((1, 2) as tup) = ()
let (1 as p1, 2 as p2) = ()
let (1, 2) = () // TODO: add trailing comma
let (1 : int, 2 : int) = ()
let ((1 : int, 2 : int) as tup) = ()
let ((1 : int, 2 : int) : (int, int)) = ()
let ((1 : int, 2 : int) as tup : (int, int)) = ()
let (((1 : int, 2 : int) : (int, int)) as tup) = ()

switch x {
| (1, 2) => () 
| (1, 2) as tup => () 
| (1, 2) => () // TODO: add trailing comma
| (1: int, 2 : int) => () 
| (1 as p1: int, 2 as p2 : int) as tup => () 
| ((1: int, 2 : int) : (int, int)) => () 
| ((1: int, 2 : int) : (int, int)) as tup => () 
}

let f = ((x)) => ()
let f = ((x)) => ()
let f = ((x, y)) => x + y // TODO: add trailing comma
let f = ((x as p1, y as p2) as tup) => x + y // TODO: add trailing comma
let f = ((x, y) : (int, int)) => ()
let f = (((x, y) : (int, int))) => ()
let f = (((x, y) as tup1 : (int, int)) as tup) => ()

for (x, y) in 0 to 10 { () }
for (x, y) as tup in 0 to 10 { () }
for ((x, y) in 0 to 10) { () }
for ((x as p1, y as p2) in 0 to 10) { () }
for ((x, y) as tup in 0 to 10) { () }
for (((x, y)) in 0 to 10) { () }
for (((x, y) as tup) in 0 to 10) { () }
for (((x as p1, y as p2)) as tup in 0 to 10) { () }
for (((x, y) : (int, int)) in 0 to 10) { () }
for (((x, y) : (int, int)) as ctup in 0 to 10) { () }
for ((((x, y) : (int, int)) as ctup) in 0 to 10) { () }
