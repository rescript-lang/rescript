type t

let fromStringUnsafe: string => t = Obj.magic

// TODO: check that the string is the correct type
let make: string => option<t> = tokenId =>
  Helper.isPositiveStringInteger(tokenId) ? Some(tokenId->fromStringUnsafe) : None

let makeWithDefault: (string, int) => t = (tokenId, default) =>
  switch make(tokenId) {
  | Some(id) => id
  | None => default->Js.Math.abs_int->Belt.Int.toString->fromStringUnsafe
  }

// NOTE: I run abs on this int because negative integers are invalid
let makeFromInt: int => t = tokenId => tokenId->Js.Math.abs_int->Belt.Int.toString->fromStringUnsafe

let toString: t => string = Obj.magic
let toInt: t => option<int> = token => token->toString->int_of_string_opt
