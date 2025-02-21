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
type t<'a> = option<'a> = None | Some('a)

let filter = (opt, p) =>
  switch opt {
  | Some(x) as option if p(x) => option
  | _ => None
  }

let forEach = (opt, f) =>
  switch opt {
  | Some(x) => f(x)
  | None => ()
  }

let getExn = (x, ~message=?) =>
  switch x {
  | Some(x) => x
  | None =>
    Stdlib_Error.panic(
      switch message {
      | None => "Option.getExn called for None value"
      | Some(message) => message
      },
    )
  }

external getUnsafe: option<'a> => 'a = "%identity"

let mapOr = (opt, default, f) =>
  switch opt {
  | Some(x) => f(x)
  | None => default
  }

let mapWithDefault = mapOr

let map = (opt, f) =>
  switch opt {
  | Some(x) => Some(f(x))
  | None => None
  }

let flatMap = (opt, f) =>
  switch opt {
  | Some(x) => f(x)
  | None => None
  }

let getOr = (opt, default) =>
  switch opt {
  | Some(x) => x
  | None => default
  }

let getWithDefault = getOr

let orElse = (opt, other) =>
  switch opt {
  | Some(_) as some => some
  | None => other
  }

let isSome = x =>
  switch x {
  | Some(_) => true
  | None => false
  }

let isNone = x => x == None

let equal = (a, b, eq) =>
  switch (a, b) {
  | (Some(a), Some(b)) => eq(a, b)
  | (None, None) => true
  | (None, Some(_)) | (Some(_), None) => false
  }

let compare = (a, b, cmp) =>
  switch (a, b) {
  | (Some(a), Some(b)) => cmp(a, b)
  | (None, Some(_)) => Stdlib_Ordering.less
  | (Some(_), None) => Stdlib_Ordering.greater
  | (None, None) => Stdlib_Ordering.equal
  }

let all = options => {
  let acc = []
  let hasNone = ref(false)
  let index = ref(0)
  while hasNone.contents == false && index.contents < options->Stdlib_Array.length {
    switch options->Stdlib_Array.getUnsafe(index.contents) {
    | None => hasNone.contents = true
    | Some(value) =>
      acc->Stdlib_Array.push(value)
      index.contents = index.contents + 1
    }
  }
  hasNone.contents ? None : Some(acc)
}

let all2 = ((a, b)) => {
  switch (a, b) {
  | (Some(a), Some(b)) => Some((a, b))
  | _ => None
  }
}

let all3 = ((a, b, c)) => {
  switch (a, b, c) {
  | (Some(a), Some(b), Some(c)) => Some((a, b, c))
  | _ => None
  }
}

let all4 = ((a, b, c, d)) => {
  switch (a, b, c, d) {
  | (Some(a), Some(b), Some(c), Some(d)) => Some((a, b, c, d))
  | _ => None
  }
}

let all5 = ((a, b, c, d, e)) => {
  switch (a, b, c, d, e) {
  | (Some(a), Some(b), Some(c), Some(d), Some(e)) => Some((a, b, c, d, e))
  | _ => None
  }
}

let all6 = ((a, b, c, d, e, f)) => {
  switch (a, b, c, d, e, f) {
  | (Some(a), Some(b), Some(c), Some(d), Some(e), Some(f)) => Some((a, b, c, d, e, f))
  | _ => None
  }
}
