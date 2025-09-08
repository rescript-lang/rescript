@ocaml.doc(" hello ") @genType
let flat = 34

@ocaml.doc("
  * Sign a message with a key.
  *
  * @param message - A message to be signed
  * @param key - The key with which to sign the message
  * @returns A signed message
 ")
@genType
let signMessage = (. message, key) => message ++ string_of_int(key)

@genType
let one = a => a + 0

@genType
let two = (a, b) => a + b + 0

@genType
let tree = (a, b, c) => a + b + c + 0

@genType
let oneU = (. a) => a + 0

@genType
let twoU = (. a, b) => a + b + 0

@genType
let treeU = (. a, b, c) => a + b + c + 0

@genType
let useParam = param => param + 34

@genType
let useParamU = (. param) => param + 34

@genType
let unnamed1 = (_: int) => 34

@genType
let unnamed1U = (. _: int) => 34

@genType
let unnamed2 = (_: int, _: int) => 34

@genType
let unnamed2U = (. _: int, _: int) => 34

@genType
let grouped = (~x, ~y, a, b, c, ~z) => x + y + a + b + c + z

@genType
let unitArgWithoutConversion = () => "abc"

@genType
let unitArgWithoutConversionU = (. ()) => "abc"

type t =
  | A
  | B

@genType
let unitArgWithConversion = () => A

@genType
let unitArgWithConversionU = (. ()) => A

