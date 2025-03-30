/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * In addition to the permissions granted to you by the LGPL, you may combine
 * or link a "work that uses the Library" with a publicly distributed version
 * of this file to produce a combined library or application, then distribute
 * that combined work under the terms of your choosing, with no requirement
 * to comply with the obligations normally placed on you by section 4 of the
 * LGPL version 3 (or the corresponding section of a later version of the LGPL
 * should you choose to use a later version).
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. */

/*** Efficient JSON encoding using JavaScript API */

@unboxed
type rec t = Stdlib_JSON.t =
  | Boolean(bool)
  | @as(null) Null
  | String(string)
  | Number(float)
  | Object(dict<t>)
  | Array(array<t>)

module Kind = {
  type json = t
  type rec t<_> =
    | String: t<Js_string.t>
    | Number: t<float>
    | Object: t<dict<json>>
    | Array: t<array<json>>
    | Boolean: t<bool>
    | Null: t<Js_types.null_val>
}

type tagged_t =
  | JSONFalse
  | JSONTrue
  | JSONNull
  | JSONString(string)
  | JSONNumber(float)
  | JSONObject(dict<t>)
  | JSONArray(array<t>)

let classify = (x: t): tagged_t => {
  let ty = Js_extern.typeof(x)
  if ty == "string" {
    JSONString(Obj.magic(x))
  } else if ty == "number" {
    JSONNumber(Obj.magic(x))
  } else if ty == "boolean" {
    if Obj.magic(x) == true {
      JSONTrue
    } else {
      JSONFalse
    }
  } else if Obj.magic(x) === Js_extern.null {
    JSONNull
  } else if Js_array2.isArray(x) {
    JSONArray(Obj.magic(x))
  } else {
    JSONObject(Obj.magic(x))
  }
}

let test = (type a, x: 'a, v: Kind.t<a>): bool =>
  switch v {
  | Kind.Number => Js_extern.typeof(x) == "number"
  | Kind.Boolean => Js_extern.typeof(x) == "boolean"
  | Kind.String => Js_extern.typeof(x) == "string"
  | Kind.Null => Obj.magic(x) === Js_extern.null
  | Kind.Array => Js_array2.isArray(x)
  | Kind.Object =>
    Obj.magic(x) !== Js_extern.null && (Js_extern.typeof(x) == "object" && !Js_array2.isArray(x))
  }

let decodeString = json =>
  if Js_extern.typeof(json) == "string" {
    Some((Obj.magic((json: t)): string))
  } else {
    None
  }

let decodeNumber = json =>
  if Js_extern.typeof(json) == "number" {
    Some((Obj.magic((json: t)): float))
  } else {
    None
  }

let decodeObject = json =>
  if (
    Js_extern.typeof(json) == "object" &&
      (!Js_array2.isArray(json) &&
      !((Obj.magic(json): Js_null.t<'a>) === Js_extern.null))
  ) {
    Some((Obj.magic((json: t)): dict<t>))
  } else {
    None
  }

let decodeArray = json =>
  if Js_array2.isArray(json) {
    Some((Obj.magic((json: t)): array<t>))
  } else {
    None
  }

let decodeBoolean = (json: t) =>
  if Js_extern.typeof(json) == "boolean" {
    Some((Obj.magic((json: t)): bool))
  } else {
    None
  }

let decodeNull = (json): option<Js_null.t<_>> =>
  if (Obj.magic(json): Js_null.t<'a>) === Js_extern.null {
    Some(Js_extern.null)
  } else {
    None
  }

/* external parse : string -> t = "parse"
 [@@val][@@scope "JSON"] */

@val @scope("JSON") external parseExn: string => t = "parse"

@val @scope("JSON") external stringifyAny: 'a => option<string> = "stringify"
/* TODO: more docs when parse error happens or stringify non-stringfy value */

@val external null: t = "null"
external string: string => t = "%identity"
external number: float => t = "%identity"
external boolean: bool => t = "%identity"
external object_: dict<t> => t = "%identity"

/* external array_ : t array -> t = "%identity" */

external array: array<t> => t = "%identity"
external stringArray: array<string> => t = "%identity"
external numberArray: array<float> => t = "%identity"
external booleanArray: array<bool> => t = "%identity"
external objectArray: array<dict<t>> => t = "%identity"
@val @scope("JSON") external stringify: t => string = "stringify"
@val @scope("JSON") external stringifyWithSpace: (t, @as(json`null`) _, int) => string = "stringify"

/* in memory modification does not work until your root is
   actually None, so we need wrap it as ``v`` and
   return the first element instead */

let patch: _ => _ = %raw(`function (json) {
  var x = [json];
  var q = [{ kind: 0, i: 0, parent: x }];
  while (q.length !== 0) {
    // begin pop the stack
    var cur = q[q.length - 1];
    if (cur.kind === 0) {
      cur.val = cur.parent[cur.i]; // patch the undefined value for array
      if (++cur.i === cur.parent.length) {
        q.pop();
      }
    } else {
      q.pop();
    }
    // finish
    var task = cur.val;
    if (typeof task === "object") {
      if (Array.isArray(task) && task.length !== 0) {
        q.push({ kind: 0, i: 0, parent: task, val: undefined });
      } else {
        for (var k in task) {
          if (k === "RE_PRIVATE_NONE") {
            if (cur.kind === 0) {
              cur.parent[cur.i - 1] = undefined;
            } else {
              cur.parent[cur.i] = undefined;
            }
            continue;
          }
          q.push({ kind: 1, i: k, parent: task, val: task[k] });
        }
      }
    }
  }
  return x[0];
}
`)

let serializeExn = (type t, x: t): string =>
  %raw(` function(obj){
  var output= JSON.stringify(obj,function(_,value){
      if(value===undefined){
          return {RE_PRIVATE_NONE : true}
      }
    return value
  });
  
 if(output === undefined){
   // JSON.stringify will raise TypeError when it detects cylic objects
   throw new TypeError("output is undefined")
 }
 return output 
 }
`)(x)

let deserializeUnsafe = (s: string): 'a => patch(parseExn(s))
