/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/** Js symbol type only available in ES6 */
type symbol = Stdlib_Symbol.t

type obj_val = Stdlib_Type.Classify.object

/** This type has only one value `undefined` */
type undefined_val

/** This type has only one value `null` */
type null_val

type function_val = Stdlib_Type.Classify.function

type rec t<_> =
  | Undefined: t<undefined_val>
  | Null: t<null_val>
  | Boolean: t<bool>
  | Number: t<float>
  | String: t<string>
  | Function: t<function_val>
  | Object: t<obj_val>
  | Symbol: t<symbol>
  | BigInt: t<bigint>

type tagged_t =
  | JSFalse
  | JSTrue
  | JSNull
  | JSUndefined
  | JSNumber(float)
  | JSString(string)
  | JSFunction(function_val)
  | JSObject(obj_val)
  | JSSymbol(symbol)
  | JSBigInt(bigint)

let classify = (x: 'a): tagged_t => {
  let ty = Js_extern.typeof(x)
  if ty == "undefined" {
    JSUndefined
  } else if x === Obj.magic(Js_null.empty) {
    JSNull
  } else if ty == "number" {
    JSNumber(Obj.magic(x))
  } else if ty == "bigint" {
    JSBigInt(Obj.magic(x))
  } else if ty == "string" {
    JSString(Obj.magic(x))
  } else if ty == "boolean" {
    if Obj.magic(x) == true {
      JSTrue
    } else {
      JSFalse
    }
  } else if ty == "symbol" {
    JSSymbol(Obj.magic(x))
  } else if ty == "function" {
    JSFunction(Obj.magic(x))
  } else {
    JSObject(Obj.magic(x))
  }
}

let test = (type a, x: 'a, v: t<a>): bool =>
  switch v {
  | Number => Js_extern.typeof(x) == "number"
  | Boolean => Js_extern.typeof(x) == "boolean"
  | Undefined => Js_extern.typeof(x) == "undefined"
  | Null => x === Obj.magic(Js_null.empty)
  | String => Js_extern.typeof(x) == "string"
  | Function => Js_extern.typeof(x) == "function"
  | Object => Js_extern.typeof(x) == "object"
  | Symbol => Js_extern.typeof(x) == "symbol"
  | BigInt => Js_extern.typeof(x) == "bigint"
  }
