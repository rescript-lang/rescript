/* Copyright (C) 2017 Hongbo Zhang, Authors of ReScript
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
type t<'res, 'err> = result<'res, 'err> = Ok('res) | Error('err)

let getExn = x =>
  switch x {
  | Ok(x) => x
  | Error(_) => raise(Not_found)
  }

let mapOr = (opt, default, f) =>
  switch opt {
  | Ok(x) => f(x)
  | Error(_) => default
  }

let mapWithDefault = mapOr

let map = (opt, f) =>
  switch opt {
  | Ok(x) => Ok(f(x))
  | Error(_) as result => result
  }

let flatMap = (opt, f) =>
  switch opt {
  | Ok(x) => f(x)
  | Error(_) as result => result
  }

let getOr = (opt, default) =>
  switch opt {
  | Ok(x) => x
  | Error(_) => default
  }

let getWithDefault = getOr

let isOk = x =>
  switch x {
  | Ok(_) => true
  | Error(_) => false
  }

let isError = x =>
  switch x {
  | Ok(_) => false
  | Error(_) => true
  }

let equal = (a, b, f) =>
  switch (a, b) {
  | (Ok(a), Ok(b)) => f(a, b)
  | (Error(_), Ok(_))
  | (Ok(_), Error(_)) => false
  | (Error(_), Error(_)) => true
  }

let compare = (a, b, f) =>
  switch (a, b) {
  | (Ok(a), Ok(b)) => f(a, b)
  | (Error(_), Ok(_)) => Stdlib_Ordering.less
  | (Ok(_), Error(_)) => Stdlib_Ordering.greater
  | (Error(_), Error(_)) => Stdlib_Ordering.equal
  }

let forEach = (r, f) =>
  switch r {
  | Ok(ok) => f(ok)
  | Error(_) => ()
  }

let mapError = (r, f) =>
  switch r {
  | Ok(_) as result => result
  | Error(e) => Error(f(e))
  }

let all = results => {
  let acc = []
  let returnValue = ref(None)
  let index = ref(0)
  while returnValue.contents == None && index.contents < results->Stdlib_Array.length {
    switch results->Stdlib_Array.getUnsafe(index.contents) {
    | Error(_) as err => returnValue.contents = Some(err)
    | Ok(value) =>
      acc->Stdlib_Array.push(value)
      index.contents = index.contents + 1
    }
  }
  switch returnValue.contents {
  | Some(error) => error
  | None => Ok(acc)
  }
}

let all2 = ((a, b)) => {
  switch (a, b) {
  | (Ok(a), Ok(b)) => Ok((a, b))
  | (Error(a), _) => Error(a)
  | (_, Error(b)) => Error(b)
  }
}

let all3 = ((a, b, c)) => {
  switch (a, b, c) {
  | (Ok(a), Ok(b), Ok(c)) => Ok((a, b, c))
  | (Error(a), _, _) => Error(a)
  | (_, Error(b), _) => Error(b)
  | (_, _, Error(c)) => Error(c)
  }
}

let all4 = ((a, b, c, d)) => {
  switch (a, b, c, d) {
  | (Ok(a), Ok(b), Ok(c), Ok(d)) => Ok((a, b, c, d))
  | (Error(a), _, _, _) => Error(a)
  | (_, Error(b), _, _) => Error(b)
  | (_, _, Error(c), _) => Error(c)
  | (_, _, _, Error(d)) => Error(d)
  }
}

let all5 = ((a, b, c, d, e)) => {
  switch (a, b, c, d, e) {
  | (Ok(a), Ok(b), Ok(c), Ok(d), Ok(e)) => Ok((a, b, c, d, e))
  | (Error(a), _, _, _, _) => Error(a)
  | (_, Error(b), _, _, _) => Error(b)
  | (_, _, Error(c), _, _) => Error(c)
  | (_, _, _, Error(d), _) => Error(d)
  | (_, _, _, _, Error(e)) => Error(e)
  }
}

let all6 = ((a, b, c, d, e, f)) => {
  switch (a, b, c, d, e, f) {
  | (Ok(a), Ok(b), Ok(c), Ok(d), Ok(e), Ok(f)) => Ok((a, b, c, d, e, f))
  | (Error(a), _, _, _, _, _) => Error(a)
  | (_, Error(b), _, _, _, _) => Error(b)
  | (_, _, Error(c), _, _, _) => Error(c)
  | (_, _, _, Error(d), _, _) => Error(d)
  | (_, _, _, _, Error(e), _) => Error(e)
  | (_, _, _, _, _, Error(f)) => Error(f)
  }
}

external ignore: result<'res, 'err> => unit = "%ignore"
