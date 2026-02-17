/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */
type t<'res, 'err> = result<'res, 'err> = Ok('res) | Error('err)

let getOrThrow = (x, ~message=?) =>
  switch x {
  | Ok(x) => x
  | Error(_) =>
    Stdlib_JsError.panic(
      switch message {
      | None => "Result.getOrThrow called for Error value"
      | Some(message) => message
      },
    )
  }

let getExn = getOrThrow

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

let equal = (a, b, eqOk, eqError) =>
  switch (a, b) {
  | (Ok(a), Ok(b)) => eqOk(a, b)
  | (Error(_), Ok(_))
  | (Ok(_), Error(_)) => false
  | (Error(a), Error(b)) => eqError(a, b)
  }

let compare = (a, b, cmpOk, cmpError) =>
  switch (a, b) {
  | (Ok(a), Ok(b)) => cmpOk(a, b)
  | (Error(_), Ok(_)) => Stdlib_Ordering.less
  | (Ok(_), Error(_)) => Stdlib_Ordering.greater
  | (Error(a), Error(b)) => cmpError(a, b)
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

let mapOkAsync = async (res, f) =>
  switch await res {
  | Ok(value) => Ok(f(value))
  | Error(err) => Error(err)
  }

let mapErrorAsync = async (res, f) =>
  switch await res {
  | Ok(value) => Ok(value)
  | Error(err) => Error(f(err))
  }

let flatMapOkAsync = async (res, f) =>
  switch await res {
  | Ok(value) => await f(value)
  | Error(err) => Error(err)
  }

let flatMapErrorAsync = async (res, f) =>
  switch await res {
  | Ok(value) => Ok(value)
  | Error(err) => await f(err)
  }
