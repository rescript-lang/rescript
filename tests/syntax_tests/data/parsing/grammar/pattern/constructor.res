let Instance = i; 
let Instance as inst = i; 
let React.Instance = i; 
let React.Instance as inst = i; 

let Instance(component) = i
let Instance(component) as inst = i
let Instance(component,) = i // trailing comma

let Instance({render, subtree}) = i
let Instance({render, subtree}) as x = i
let Instance({render, subtree} as inst) = i
let Instance({render, subtree}, inst) = i
let Instance({render, subtree} : Instance.t) = i
let Instance({render, subtree} : Instance.t) as inst = i
let Instance(({render, subtree} : Instance.t)) = i

let Instance(component, tree) = i
let Instance(component, tree) as x = i
let Instance(component as x, tree as y) = i
let Instance(component, tree) as inst = i
let Instance(component, tree,) = i // trailing comma

let (Instance : React.t) = i;
let ((Instance : React.t) as t) = i;
let (Instance : React.t) as x = i;
// constraints
let (Instance(component : comp) : React.t) = i
let (Instance(component : comp,) : React.t) = i

switch x {
| Instance => ()
| Instance as inst => ()
| Instance(comp) => ()
| Instance(comp) as inst => ()
| Instance({render, subtree}) => () 
| Instance({render, subtree}, inst) => () 
| Instance({render, subtree} : Instance.t) => ()
| Instance(({render, subtree} : Instance.t)) => ()
| Instance(comp, tree) => ()
| React.Instance(comp, tree,) => ()
| (Instance(comp: Component.t) : React.t) => ()
}


let f = (Instance) => i; 
let f = (Instance as i) => i; 
let f = (React.Instance) => i; 
let f = ((React.Instance as x)) => i; 

let f = (Instance(component)) => i
let f = (Instance(component,)) => i // trailing comma

let f = (Instance({render, subtree})) => i
let f = (Instance({render, subtree}, inst)) => i
let f = (Instance({render, subtree} : Instance.t)) => i
let f = (Instance(({render, subtree} : Instance.t))) => i

let f = (Instance(component, tree)) => i
let f = (Instance(component, tree,)) => i // trailing comma

let f = ((Instance : React.t)) => i;
let f = (Instance : React.t) => i;
let f = (Instance(comp: Component.t) : React.t) => ()

for Blue in x to y { () }
for Blue as c in x to y { () }
for (Blue in x to y) { () }
for (Blue as c in x to y) { () }
for ((Blue: Color.t) in x to y) { () }
for ((Blue: Color.t) as c in x to y) { () }
for (((Blue: Color.t) as c) in x to y) { () }

for Rgba(r, g, b) in x to y { () }
for Rgba(r, g, b) as c in x to y { () }
for (Rgba(r : float, g : float, b : float) in x to y) { () }
for (Rgba(r : float, g : float, b : float) as c in x to y) { () }
for ((Rgba(r, g, b) : Rgb.t) in x to y) { () }
for ((Rgba(r, g, b) : Rgb.t) as c in x to y) { () }
for (((Rgba(r, g, b) : Rgb.t) as c) in x to y) { () }

for Colour.Rgba(r, g, b) in x to y { () }
for (Colour.Rgba(r : float, g : float, b : float) in x to y) { () }
for ((Colour.Rgba(r, g, b) : Rgb.t) in x to y) { () }

for Point({x, y, z}) in x to y { () }
for (Point({x, y, z}) in x to y) { () }
for (Point({x, y, z}) as p in x to y) { () }

switch truth {
| true => Js.log("true")
| false => Js.log("false")
}
