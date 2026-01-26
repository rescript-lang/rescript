/* Copyright (C) 2015 -  Hongbo Zhang, Authors of ReScript
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

module Array = Primitive_array_extern
module Obj = Primitive_object_extern
module Js = Primitive_js_extern

@@uncurried

external function_arity: 'a => int = "%function_arity"

@send external apply_args: ('a => 'b, Js.null<_>, array<_>) => 'b = "apply"
let apply_args = (f, args) => apply_args(f, Js.null, args)

/* Public */
let rec app = (f, args) => {
  let init_arity = function_arity(f)
  let arity = if init_arity == 0 {
    1
  } else {
    init_arity
  } /* arity fixing */
  let len = Array.length(args)
  let d = arity - len
  if d == 0 {
    apply_args(f, args) /* f.apply (null,args) */
  } else if d < 0 {
    /* TODO: could avoid copy by tracking the index */
    app(Obj.magic(apply_args(f, Array.slice(args, 0, arity))), Array.slice(args, arity, len))
  } else {
    Obj.magic(x => app(f, Array.concat(args, [x])))
  }
}

external apply1: ('a0 => 'a1, 'a0) => 'a1 = "%curry_apply1"
external apply2: (('a0, 'a1) => 'a2, 'a0, 'a1) => 'a2 = "%curry_apply2"
external apply3: (('a0, 'a1, 'a2) => 'a3, 'a0, 'a1, 'a2) => 'a3 = "%curry_apply3"
external apply4: (('a0, 'a1, 'a2, 'a3) => 'a4, 'a0, 'a1, 'a2, 'a3) => 'a4 = "%curry_apply4"
external apply5: (('a0, 'a1, 'a2, 'a3, 'a4) => 'a5, 'a0, 'a1, 'a2, 'a3, 'a4) => 'a5 =
  "%curry_apply5"
external apply6: (('a0, 'a1, 'a2, 'a3, 'a4, 'a5) => 'a6, 'a0, 'a1, 'a2, 'a3, 'a4, 'a5) => 'a6 =
  "%curry_apply6"
external apply7: (
  ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6) => 'a7,
  'a0,
  'a1,
  'a2,
  'a3,
  'a4,
  'a5,
  'a6,
) => 'a7 = "%curry_apply7"
external apply8: (
  ('a0, 'a1, 'a2, 'a3, 'a4, 'a5, 'a6, 'a7) => 'a8,
  'a0,
  'a1,
  'a2,
  'a3,
  'a4,
  'a5,
  'a6,
  'a7,
) => 'a8 = "%curry_apply8"

let curry_1 = (o, a0, arity) =>
  switch arity {
  | 1 => apply1(Obj.magic(o), a0)
  | 2 => param => apply2(Obj.magic(o), a0, param)
  | 3 => Obj.magic((param, \"param$1") => apply3(Obj.magic(o), a0, param, \"param$1"))
  | 4 =>
    Obj.magic((param, \"param$1", \"param$2") =>
      apply4(Obj.magic(o), a0, param, \"param$1", \"param$2")
    )
  | 5 =>
    Obj.magic((param, \"param$1", \"param$2", \"param$3") =>
      apply5(Obj.magic(o), a0, param, \"param$1", \"param$2", \"param$3")
    )
  | 6 =>
    Obj.magic((param, \"param$1", \"param$2", \"param$3", \"param$4") =>
      apply6(Obj.magic(o), a0, param, \"param$1", \"param$2", \"param$3", \"param$4")
    )
  | 7 =>
    Obj.magic((param, \"param$1", \"param$2", \"param$3", \"param$4", \"param$5") =>
      apply7(Obj.magic(o), a0, param, \"param$1", \"param$2", \"param$3", \"param$4", \"param$5")
    )
  | _ => Obj.magic(app(o, [a0]))
  }

let _1 = (o, a0) => {
  let arity = function_arity(o)
  if arity == 1 {
    apply1(o, a0)
  } else {
    curry_1(o, a0, arity)
  }
}

let __1 = o => {
  let arity = function_arity(o)
  if arity == 1 {
    o
  } else {
    a0 => _1(o, a0)
  }
}

let curry_2 = (o, a0, a1, arity) =>
  switch arity {
  | 1 => app(apply1(Obj.magic(o), a0), [a1])
  | 2 => apply2(Obj.magic(o), a0, a1)
  | 3 => param => apply3(Obj.magic(o), a0, a1, param)
  | 4 => Obj.magic((param, \"param$1") => apply4(Obj.magic(o), a0, a1, param, \"param$1"))
  | 5 =>
    Obj.magic((param, \"param$1", \"param$2") =>
      apply5(Obj.magic(o), a0, a1, param, \"param$1", \"param$2")
    )
  | 6 =>
    Obj.magic((param, \"param$1", \"param$2", \"param$3") =>
      apply6(Obj.magic(o), a0, a1, param, \"param$1", \"param$2", \"param$3")
    )
  | 7 =>
    Obj.magic((param, \"param$1", \"param$2", \"param$3", \"param$4") =>
      apply7(Obj.magic(o), a0, a1, param, \"param$1", \"param$2", \"param$3", \"param$4")
    )
  | _ => Obj.magic(app(o, [a0, a1]))
  }

let _2 = (o, a0, a1) => {
  let arity = function_arity(o)
  if arity == 2 {
    apply2(o, a0, a1)
  } else {
    curry_2(Obj.magic(o), a0, a1, arity)
  }
}

let __2 = o => {
  let arity = function_arity(o)
  if arity == 2 {
    o
  } else {
    (a0, a1) => _2(o, a0, a1)
  }
}

let curry_3 = (o, a0, a1, a2, arity) =>
  switch arity {
  | 1 => app(apply1(Obj.magic(o), a0), [a1, a2])
  | 2 => app(apply2(Obj.magic(o), a0, a1), [a2])
  | 3 => apply3(Obj.magic(o), a0, a1, a2)
  | 4 => param => apply4(Obj.magic(o), a0, a1, a2, param)
  | 5 => Obj.magic((param, \"param$1") => apply5(Obj.magic(o), a0, a1, a2, param, \"param$1"))
  | 6 =>
    Obj.magic((param, \"param$1", \"param$2") =>
      apply6(Obj.magic(o), a0, a1, a2, param, \"param$1", \"param$2")
    )
  | 7 =>
    Obj.magic((param, \"param$1", \"param$2", \"param$3") =>
      apply7(Obj.magic(o), a0, a1, a2, param, \"param$1", \"param$2", \"param$3")
    )
  | _ => Obj.magic(app(o, [a0, a1, a2]))
  }

let _3 = (o, a0, a1, a2) => {
  let arity = function_arity(o)
  if arity == 3 {
    apply3(o, a0, a1, a2)
  } else {
    curry_3(Obj.magic(o), a0, a1, a2, arity)
  }
}

let __3 = o => {
  let arity = function_arity(o)
  if arity == 3 {
    o
  } else {
    (a0, a1, a2) => _3(o, a0, a1, a2)
  }
}

let curry_4 = (o, a0, a1, a2, a3, arity) =>
  switch arity {
  | 1 => app(apply1(Obj.magic(o), a0), [a1, a2, a3])
  | 2 => app(apply2(Obj.magic(o), a0, a1), [a2, a3])
  | 3 => app(apply3(Obj.magic(o), a0, a1, a2), [a3])
  | 4 => apply4(Obj.magic(o), a0, a1, a2, a3)
  | 5 => param => apply5(Obj.magic(o), a0, a1, a2, a3, param)
  | 6 => Obj.magic((param, \"param$1") => apply6(Obj.magic(o), a0, a1, a2, a3, param, \"param$1"))
  | 7 =>
    Obj.magic((param, \"param$1", \"param$2") =>
      apply7(Obj.magic(o), a0, a1, a2, a3, param, \"param$1", \"param$2")
    )
  | _ => Obj.magic(app(o, [a0, a1, a2, a3]))
  }

let _4 = (o, a0, a1, a2, a3) => {
  let arity = function_arity(o)
  if arity == 4 {
    apply4(o, a0, a1, a2, a3)
  } else {
    curry_4(Obj.magic(o), a0, a1, a2, a3, arity)
  }
}

let __4 = o => {
  let arity = function_arity(o)
  if arity == 4 {
    o
  } else {
    (a0, a1, a2, a3) => _4(o, a0, a1, a2, a3)
  }
}

let curry_5 = (o, a0, a1, a2, a3, a4, arity) =>
  switch arity {
  | 1 => app(apply1(Obj.magic(o), a0), [a1, a2, a3, a4])
  | 2 => app(apply2(Obj.magic(o), a0, a1), [a2, a3, a4])
  | 3 => app(apply3(Obj.magic(o), a0, a1, a2), [a3, a4])
  | 4 => app(apply4(Obj.magic(o), a0, a1, a2, a3), [a4])
  | 5 => apply5(Obj.magic(o), a0, a1, a2, a3, a4)
  | 6 => param => apply6(Obj.magic(o), a0, a1, a2, a3, a4, param)
  | 7 =>
    Obj.magic((param, \"param$1") => apply7(Obj.magic(o), a0, a1, a2, a3, a4, param, \"param$1"))
  | _ => Obj.magic(app(o, [a0, a1, a2, a3, a4]))
  }

let _5 = (o, a0, a1, a2, a3, a4) => {
  let arity = function_arity(o)
  if arity == 5 {
    apply5(o, a0, a1, a2, a3, a4)
  } else {
    curry_5(Obj.magic(o), a0, a1, a2, a3, a4, arity)
  }
}

let __5 = o => {
  let arity = function_arity(o)
  if arity == 5 {
    o
  } else {
    (a0, a1, a2, a3, a4) => _5(o, a0, a1, a2, a3, a4)
  }
}

let curry_6 = (o, a0, a1, a2, a3, a4, a5, arity) =>
  switch arity {
  | 1 => app(apply1(Obj.magic(o), a0), [a1, a2, a3, a4, a5])
  | 2 => app(apply2(Obj.magic(o), a0, a1), [a2, a3, a4, a5])
  | 3 => app(apply3(Obj.magic(o), a0, a1, a2), [a3, a4, a5])
  | 4 => app(apply4(Obj.magic(o), a0, a1, a2, a3), [a4, a5])
  | 5 => app(apply5(Obj.magic(o), a0, a1, a2, a3, a4), [a5])
  | 6 => apply6(Obj.magic(o), a0, a1, a2, a3, a4, a5)
  | 7 => param => apply7(Obj.magic(o), a0, a1, a2, a3, a4, a5, param)
  | _ => Obj.magic(app(o, [a0, a1, a2, a3, a4, a5]))
  }

let _6 = (o, a0, a1, a2, a3, a4, a5) => {
  let arity = function_arity(o)
  if arity == 6 {
    apply6(o, a0, a1, a2, a3, a4, a5)
  } else {
    curry_6(Obj.magic(o), a0, a1, a2, a3, a4, a5, arity)
  }
}

let __6 = o => {
  let arity = function_arity(o)
  if arity == 6 {
    o
  } else {
    (a0, a1, a2, a3, a4, a5) => _6(o, a0, a1, a2, a3, a4, a5)
  }
}

let curry_7 = (o, a0, a1, a2, a3, a4, a5, a6, arity) =>
  switch arity {
  | 1 => app(apply1(Obj.magic(o), a0), [a1, a2, a3, a4, a5, a6])
  | 2 => app(apply2(Obj.magic(o), a0, a1), [a2, a3, a4, a5, a6])
  | 3 => app(apply3(Obj.magic(o), a0, a1, a2), [a3, a4, a5, a6])
  | 4 => app(apply4(Obj.magic(o), a0, a1, a2, a3), [a4, a5, a6])
  | 5 => app(apply5(Obj.magic(o), a0, a1, a2, a3, a4), [a5, a6])
  | 6 => app(apply6(Obj.magic(o), a0, a1, a2, a3, a4, a5), [a6])
  | 7 => apply7(Obj.magic(o), a0, a1, a2, a3, a4, a5, a6)
  | _ => Obj.magic(app(o, [a0, a1, a2, a3, a4, a5, a6]))
  }

let _7 = (o, a0, a1, a2, a3, a4, a5, a6) => {
  let arity = function_arity(o)
  if arity == 7 {
    apply7(o, a0, a1, a2, a3, a4, a5, a6)
  } else {
    curry_7(Obj.magic(o), a0, a1, a2, a3, a4, a5, a6, arity)
  }
}

let __7 = o => {
  let arity = function_arity(o)
  if arity == 7 {
    o
  } else {
    (a0, a1, a2, a3, a4, a5, a6) => _7(o, a0, a1, a2, a3, a4, a5, a6)
  }
}

let curry_8 = (o, a0, a1, a2, a3, a4, a5, a6, a7, arity) =>
  switch arity {
  | 1 => app(apply1(Obj.magic(o), a0), [a1, a2, a3, a4, a5, a6, a7])
  | 2 => app(apply2(Obj.magic(o), a0, a1), [a2, a3, a4, a5, a6, a7])
  | 3 => app(apply3(Obj.magic(o), a0, a1, a2), [a3, a4, a5, a6, a7])
  | 4 => app(apply4(Obj.magic(o), a0, a1, a2, a3), [a4, a5, a6, a7])
  | 5 => app(apply5(Obj.magic(o), a0, a1, a2, a3, a4), [a5, a6, a7])
  | 6 => app(apply6(Obj.magic(o), a0, a1, a2, a3, a4, a5), [a6, a7])
  | 7 => app(apply7(Obj.magic(o), a0, a1, a2, a3, a4, a5, a6), [a7])
  | _ => Obj.magic(app(o, [a0, a1, a2, a3, a4, a5, a6, a7]))
  }

let _8 = (o, a0, a1, a2, a3, a4, a5, a6, a7) => {
  let arity = function_arity(o)
  if arity == 8 {
    apply8(o, a0, a1, a2, a3, a4, a5, a6, a7)
  } else {
    curry_8(Obj.magic(o), a0, a1, a2, a3, a4, a5, a6, a7, arity)
  }
}

let __8 = o => {
  let arity = function_arity(o)
  if arity == 8 {
    o
  } else {
    (a0, a1, a2, a3, a4, a5, a6, a7) => _8(o, a0, a1, a2, a3, a4, a5, a6, a7)
  }
}
