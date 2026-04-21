let foo = (x, y) => x + y

let z = foo(3, 4)

let bar = (x, y) => x + y

let b = bar(3, 4)

let w = 3->foo(4)

let a = 3->foo(4)

Console.log(a) // Test automatic uncurried application

let _ = Array.map([1], x => x + 1)
