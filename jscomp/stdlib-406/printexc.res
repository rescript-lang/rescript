/* ************************************************************************ */
/*  */
/* OCaml */
/*  */
/* Xavier Leroy, projet Cristal, INRIA Rocquencourt */
/*  */
/* Copyright 1996 Institut National de Recherche en Informatique et */
/* en Automatique. */
/*  */
/* All rights reserved.  This file is distributed under the terms of */
/* the GNU Lesser General Public License version 2.1, with the */
/* special exception on linking described in the file LICENSE. */
/*  */
/* ************************************************************************ */
@@warning("-32")

let printers = ref(list{})

let locfmt = (s, linum: int, start: int, finish: int, msg) =>
  "File \"" ++
  s ++
  "\", line " ++
  __unsafe_cast(linum) ++
  ", characters " ++
  __unsafe_cast(start) ++
  "-" ++
  __unsafe_cast(finish) ++
  ": " ++
  msg

let fields: exn => string = %raw(`function(x){
  var s = "" 
  var index = 1
  while ("_"+index in x){
    s += x ["_" + index];
    ++ index
  }
  if(index === 1){
    return s 
  }
  return "(" + s + ")"
}
`)

external exn_slot_name: exn => string = "?exn_slot_name"

let to_string = x => {
  let rec conv = param =>
    switch param {
    | list{hd, ...tl} =>
      switch try hd(x) catch {
      | _ => None
      } {
      | Some(s) => s
      | None => conv(tl)
      }
    | list{} =>
      switch x {
      | Match_failure(file, line, char) =>
        locfmt(file, line, char, char + 5, "Pattern matching failed")
      | Assert_failure(file, line, char) => locfmt(file, line, char, char + 6, "Assertion failed")
      | Undefined_recursive_module(file, line, char) =>
        locfmt(file, line, char, char + 6, "Undefined recursive module")
      | _ =>
        let constructor = exn_slot_name(x)
        constructor ++ fields(x)
      }
    }

  conv(printers.contents)
}

let print = (fct, arg) =>
  try fct(arg) catch {
  | x =>
    Js.log("Uncaught exception: " ++ to_string(x))
    raise(x)
  }

let catch = (fct, arg) =>
  try fct(arg) catch {
  | x =>
    Js.log("Uncaught exception: " ++ to_string(x))
    exit(2)
  }

let register_printer = fn => printers := list{fn, ...printers.contents}
