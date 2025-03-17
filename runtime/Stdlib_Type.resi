/***
Utilities for classifying the type of JavaScript values at runtime.
*/

/**
The possible types of JavaScript values.
*/
type t = [#undefined | #object | #boolean | #number | #bigint | #string | #symbol | #function]

/**
`typeof(someValue)`

Returns the underlying JavaScript type of any runtime value.

See [`typeof`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/typeof) on MDN.

## Examples
```rescript
Console.log(Type.typeof("Hello")) // Logs "string" to the console.

let someVariable = true

switch someVariable->Type.typeof {
| #boolean => Console.log("This is a bool, yay!")
| _ => Console.log("Oh, not a bool sadly...")
}
```
*/
external typeof: 'a => t = "#typeof"

module Classify: {
  /***
  Classifies JavaScript runtime values.
  */

  /**
  An abstract type representing a JavaScript function.

  See [`function`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function) on MDN.
  */
  type function

  /**
  An abstract type representing a JavaScript object.

  See [`object`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object) on MDN.
  */
  type object

  /**
  The type representing a classified JavaScript value.
  */
  type t =
    | Bool(bool)
    | Null
    | Undefined
    | String(string)
    | Number(float)
    | Object(object)
    | Function(function)
    | Symbol(Stdlib_Symbol.t)
    | BigInt(bigint)

  /**
`classify(anyValue)`
Classifies a JavaScript value.

## Examples
```rescript
switch %raw(`null`)->Type.Classify.classify {
| Null => Console.log("Yup, that's null.")
| _ => Console.log("This doesn't actually appear to be null...")
}
```
*/
  let classify: 'a => t
}
