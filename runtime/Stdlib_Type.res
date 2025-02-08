type t = [#undefined | #object | #boolean | #number | #bigint | #string | #symbol | #function]

external typeof: 'a => t = "#typeof"

module Classify = {
  type function
  type object

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

  @val external _internalClass: 'a => string = "Object.prototype.toString.call"
  external _asBool: 'a => bool = "%identity"
  external _asString: 'a => string = "%identity"
  external _asFloat: 'a => float = "%identity"
  external _asObject: 'a => object = "%identity"
  external _asFunction: 'a => function = "%identity"
  external _asSymbol: 'a => Stdlib_Symbol.t = "%identity"
  external _asBigInt: 'a => bigint = "%identity"

  let classify = value => {
    switch _internalClass(value) {
    | "[object Boolean]" => Bool(_asBool(value))
    | "[object Null]" => Null
    | "[object Undefined]" => Undefined
    | "[object String]" => String(_asString(value))
    | "[object Number]" => Number(_asFloat(value))
    | "[object Function]"
    | "[object GeneratorFunction]"
    | "[object AsyncFunction]" =>
      Function(_asFunction(value))
    | "[object Symbol]" => Symbol(_asSymbol(value))
    | "[object BigInt]" => BigInt(_asBigInt(value))
    | _ => Object(_asObject(value))
    }
  }
}
