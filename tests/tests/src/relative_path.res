/* include (struct */
@module("./File.js") external foo: int = "foo"
/* end : sig val foo : int end ) */

@module("./File.js") external foo2: int => int = "foo2"

let bar = foo
