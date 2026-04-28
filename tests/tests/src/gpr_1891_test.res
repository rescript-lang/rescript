let foo = x =>
  switch x {
  | #Foo(3) => Console.log("1")
  | _ => Console.log("2")
  }

let foo2 = x =>
  switch x {
  | #Foo(3) => "xxxx"
  | _ => "xxx"
  }

let foo3 = x =>
  switch x {
  | #Foo(3) => 1
  | _ => 2
  }

let foo4 = (x, h) =>
  switch x {
  | #Foo(3) => h()
  | _ => ()
  }

let foo5 = x =>
  switch x {
  | #Foo(3) => Console.log("hi")
  | _ => Console.log("x")
  }
