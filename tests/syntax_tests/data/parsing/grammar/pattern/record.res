let {a} = x
let {a} as p = x
let {a,} = x // trailing comma
let {a, b} = x
let {a: p1, b: p2} = x
let {a, b,} = x // trailing comma
let {ReasonReact.state} = x
let {ReasonReact.state: state as prevState} = x
let {ReasonReact.state: theState} = x 
let {a: u} = x
let {a: (u: int)} = x
let {a: (u as p: int)} = x
let {a: {x, y}} = x
let {a: {x: p1, y: p2}} = x
let {a, _ } = x
let {a, _, } = x
let ({a} : myRecord) = x

switch x {
| {a} => ()
| {a} as r => ()
| {a,} => () // trailing comma
| {a, b} => () 
| {a, b,} => () // trailing comma
| {ReasonReact.state} => ()
| {ReasonReact.state: state as prevState} => ()
| {ReasonReact.state: theState} => ()
| {a: u} => ()
| {a: (u: int)} => ()
| {a: (u as p: int) as p2} as p3 => ()
| {a: {x, y}} => ()
| {a: {x: p1, y: p2} as p3} => ()
| {a, _ } => ()
| {a, _, } => ()
| ({a} : myRecord) => () 
}

let f = ({a}) => ()
let f = ({a} as r) => ()
let f = ({a,}) => () // trailing comma
let f = ({a, b}) => () 
let f = ({a, b,}) => () // trailing comma
let f = ({ReasonReact.state}) => () 
let f = ({ReasonReact.state: state as prevState}) => () 
let f = ({ReasonReact.state: theState}) => () 
let f = ({a: u}) => ()
let f = ({a: (u: int)}) => ()
let f = ({a: (u as x: int) as r}) => ()
let f = ({a: {x, y}}) => () 
let f = ({a: {x: r, y: r2}}) => () 
let f = ({a, _ }) => () 
let f = ({a, _,}) => () 
let f = ({a}: myRecord) => () 
let f = (({a}: myRecord)) => () 
let f = (({a}: myRecord) as p) => () 


for {a} in 0 to 10 { () }
for {a} as p in 0 to 10 { () }
for (({a}) in 0 to 10) { () }
for (({a} as p) in 0 to 10) { () }
for ({a} in 0 to 10) { () }
for ({a} as p in 0 to 10) { () }

for {ReasonReact.state} in 0 to 10 { () }
for ({ReasonReact.state} in 0 to 10) { () }
for (({ReasonReact.state}) in 0 to 10) { () }

for {ReasonReact.state: state as prevState} in 0 to 10 { () }
for ({ReasonReact.state: state as prevState} in 0 to 10) { () }
for (({ReasonReact.state: state as prevState}) in 0 to 10) { () }
for {ReasonReact.state: theState} in 0 to 10 { () }
for ({ReasonReact.state: theState} in 0 to 10) { () }
for (({ReasonReact.state: theState}) in 0 to 10) { () }
for {a: u} in 0 to 10 { () }
for (({a: u}) in 0 to 10) { () }
for ({a: u} in 0 to 10) { () }
for {a: (u: int)} in 0 to 10 { () }
for {a: (u as sp: int) as p} in 0 to 10 { () }
for (({a: (u: int)}) in 0 to 10) { () }
for ({a: (u: int)} in 0 to 10) { () }
for {a: {x, y}} in 0 to 10 { () }
for {a: {x: p1, y: p2} as p3} in 0 to 10 { () }
for (({a: {x, y}}) in 0 to 10) { () }
for (({a: {x, y}} as p) as p2 in 0 to 10) { () }
for ({a: {x, y}} in 0 to 10) { () }
for {a, _} in 0 to 10 { () }
for (({a, _}) in 0 to 10) { () }
for ({a, _} in 0 to 10) { () }
for (({a} : myRecord) in 0 to 10) { () }
