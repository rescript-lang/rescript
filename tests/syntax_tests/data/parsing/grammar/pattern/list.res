let list{} = ()
let list{} as l = ()
let list{x} = ()
let list{x as p1} as l = ()
let list{x, ...xs} = ()
let list{x, ...xs as tail} = ()
let list{x, y, ...tail} = ()
let list{x, y} = ()
let list{x as p1, y as p2} = ()
let list{x, y,} = () // trailing comma
let list{x, ...xs,} = () // trailing comma
let list{x, y, ...tail,} = ()// trailing comma
let list{x, list{y, ...ys}, ...xs} = ()
let list{x as p1, list{y as p2, ...ys as tail1} as l2, ...xs as tail2} = ()
let list{x :int, list{y :int, ...ys : list<int>}, ...xs} = ()
let (list{x, ...xs} : list<int>) = ()
let (list{x, ...xs} : list<int>) as constrainedList = ()
let ((list{x, ...xs} : list<int>) as clist) = ()
let ((list{x, ...xs} : list<int>) : list<int>) = ()

switch x {
| list{} => ()
| list{} as l => ()
| list{x as p1} => ()
| list{x : int} => ()
| list{x, ...xs} => ()
| (list{x : int, ...xs : list<int>} :list<int>) => ()
| list{x as p1, ...xs as p2} as l => ()
| list{x, y, ...tail} => ()
| list{x, y} => ()
| list{x, y,} => () // trailing comma
| list{x, ...xs,} => () // trailing comma
| list{x, y, ...tail,} => ()// trailing comma
| list{x, list{y, ...ys}, ...xs} => ()
| list{x as p1, list{y as p2, ...ys as tail1}, ...xs as tail2} as l => ()
| (list{x, ...xs} : list<int>) => ()
}

let f = (list{}) => ()
let f = (list{} as p) => ()
let f = (list{x}) => ()
let f = (list{(x : int) as p}) => ()
let f = (list{x as p} as p2) => ()
let f = (list{x, ...xs}) => ()
let f = (list{x, ...xs as tail}) => ()
let f = (list{x, y, ...tail}) => ()
let f = (list{x, y}) => ()
let f = (list{x, y,}) => () // trailing comma
let f = (list{x, ...xs,}) => () // trailing comma
let f = (list{x, y, ...tail,}) => ()// trailing comma
let f = (list{x, list{y, ...ys}, ...xs}) => ()
let f = (list{x as p1, list{y as p2, ...ys as tail1}, ...xs as tail2} as l) => ()
let f = (list{x, ...xs} : list<int>) => ()

for list{} in x to y { () }
for list{} as l in x to y { () }
for (list{} in x to y) { () }
for (list{} as l in x to y) { () }
for ((list{}) in x to y) { () }
for ((list{}) as l in x to y) { () }
for ((list{} as l) in x to y) { () }
for list{x} in x to y { () }
for list{x as p} in x to y { () }
for list{(x : int) as p} in x to y { () }
for list{x as p : int} in x to y { () }
for (list{x} in x to y) { () }
for ((list{x}) in x to y) { () }
for list{x, ...xs} in x to y { () }
for (list{x, ...xs} in x to y) { () }
for ((list{x, ...xs}) in x to y) { () }
for (list{x as p1, list{y as p2, ...ys as tail1}, ...xs as tail2} as l in x to y) { () }
for ((list{x, ...xs} : list<int>) in x to y) { () }

