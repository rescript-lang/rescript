let foo: int => int = %raw(`function add(x, y=5){ return x + y }`)

Console.log(foo(2))
