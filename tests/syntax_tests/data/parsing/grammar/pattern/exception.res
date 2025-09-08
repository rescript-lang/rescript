let exception Foo = ()
let exception Foo as e = ()
let exception (Foo as e) = ()
let exception Foo(a, b) = ()
let exception Foo(a, b) as e = ()
let exception (Foo(a, b) as e) = ()
let (exception Foo) = ()
let (exception Foo) as e = ()
let (exception Foo as e) = ()
let (exception (Foo as e)) = ()
let (exception Foo(a, b)) = ()
let (exception Foo(a, b)) as e = ()
let (exception Foo(a, b) as e) = ()
let (exception Foo : exc<t>) = ()
let (exception Foo : exc<t>) as e = ()
let (exception (Foo : exc<t>) as e) = ()
let (exception ((Foo : exc<t>) as e)) = ()

switch x {
| exception Foo => ()
| exception Foo as e => ()
| exception (Foo as e) => ()
| exception Foo(a, b) => ()
| (exception Foo) => ()
| (exception Foo as e) => ()
| (exception (Foo as e)) => ()
| (exception Foo(a, b)) => ()
| (exception Foo : exc<t>) => ()
}

let f = (exception Foo) => ()
let f = (exception Foo as e) => ()
let f = (exception (Foo as e)) => ()
let f = (exception Foo(a, b)) => ()
let f = ((exception Foo)) => ()
let f = ((exception Foo) as e) => ()
let f = (exception Foo(a, b)) => ()
let f = (exception Foo : exc<t>) => ()

for exception Foo in 0 to 10 { () }
for exception Foo as e in 0 to 10 { () }
for (exception Foo in 0 to 10) { () }
for (exception Foo as e in 0 to 10) { () }
for ((exception Foo) in 0 to 10) { () }
for exception Foo(a, b) in 0 to 10 { () }
for (exception Foo(a, b) in 0 to 10) { () }
for ((exception Foo(a, b): exc<t>) in 0 to 10) { () } 
