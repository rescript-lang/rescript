/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

module Float = Primitive_float_extern
module Int = Stdlib_Int
module Obj = Primitive_object_extern
module String = Primitive_string_extern

// Note: this only works as intended as long as the runtime is compiled
// with -bs-cross-module-opt.
let typeof = Primitive_js_extern.typeof

@send external charCodeAt: (string, int) => int = "charCodeAt"

// Multiply int32 with C-style overflow behavior
external imul: (int, int) => int = "Math.imul"

type rec cell<'a> = {
  content: 'a,
  mutable next: option<cell<'a>>,
}
and t<'a> = {
  mutable length: int,
  mutable first: option<cell<'a>>,
  mutable last: option<cell<'a>>,
}

let create_queue = () => {
  length: 0,
  first: None,
  last: None,
}

/* Added to tail */
let push_back = (q: t<'a>, v: 'a) => {
  let cell = Some({content: v, next: None})

  switch q.last {
  | None =>
    q.length = 1
    q.first = cell
    q.last = cell
  | Some(last) =>
    q.length = q.length + 1
    last.next = cell
    q.last = cell
  }
}

let is_empty_queue = q => q.length == 0

/* pop from front */

let unsafe_pop = (q: t<'a>) =>
  switch q.first {
  | None => assert(false)
  | Some(cell) =>
    let next = cell.next
    if next == None {
      q.length = 0
      q.first = None
      q.last = None
    } else {
      q.length = q.length - 1
      q.first = next
    }
    cell.content
  }

let rotl32 = (x: int, n) => Int.bitwiseOr(Int.shiftLeft(x, n), Int.shiftRightUnsigned(x, 32 - n))

let hash_mix_int = (h, d) => {
  let d = ref(d)
  d.contents = imul(d.contents, 0xcc9e2d51)
  d.contents = rotl32(d.contents, 15)
  d.contents = imul(d.contents, 0x1b873593)
  let h = ref(Int.bitwiseXor(h, d.contents))
  h.contents = rotl32(h.contents, 13)
  h.contents + Int.shiftLeft(h.contents, 2) + 0xe6546b64
}

let hash_final_mix = h => {
  let h = ref(Int.bitwiseXor(h, Int.shiftRightUnsigned(h, 16)))
  h.contents = imul(h.contents, 0x85ebca6b)
  h.contents = Int.bitwiseXor(h.contents, Int.shiftRightUnsigned(h.contents, 13))
  h.contents = imul(h.contents, 0xc2b2ae35)
  Int.bitwiseXor(h.contents, Int.shiftRightUnsigned(h.contents, 16))
}

let hash_mix_string = (h, s) => {
  let len = String.length(s)
  let block = len / 4 - 1
  let hash = ref(h)
  for i in 0 to block {
    let j = 4 * i
    let w = Int.bitwiseOr(
      Int.bitwiseOr(
        Int.bitwiseOr(s->charCodeAt(j), Int.shiftLeft(s->charCodeAt(j + 1), 8)),
        Int.shiftLeft(s->charCodeAt(j + 2), 16),
      ),
      Int.shiftLeft(s->charCodeAt(j + 3), 24),
    )

    hash.contents = hash_mix_int(hash.contents, w)
  }
  let modulo = Int.bitwiseAnd(len, 0b11)
  if modulo != 0 {
    let w = if modulo == 3 {
      Int.bitwiseOr(
        Int.bitwiseOr(
          Int.shiftLeft(s->charCodeAt(len - 1), 16),
          Int.shiftLeft(s->charCodeAt(len - 2), 8),
        ),
        s->charCodeAt(len - 3),
      )
    } else if modulo == 2 {
      Int.bitwiseOr(Int.shiftLeft(s->charCodeAt(len - 1), 8), s->charCodeAt(len - 2))
    } else {
      s->charCodeAt(len - 1)
    }

    hash.contents = hash_mix_int(hash.contents, w)
  }
  hash.contents = Int.bitwiseXor(hash.contents, len)
  hash.contents
}

let hash = (count: int, _limit, seed: int, obj: Obj.t): int => {
  let s = ref(seed)
  if typeof(obj) == "number" {
    let u = Float.toInt(Obj.magic(obj))
    s.contents = hash_mix_int(s.contents, u + u + 1)
    hash_final_mix(s.contents)
  } else if typeof(obj) == "string" {
    s.contents = hash_mix_string(s.contents, (Obj.magic(obj): string))
    hash_final_mix(s.contents)
  } else {
    /* TODO: hash [null] [undefined] as well */

    let queue = create_queue()
    let num = ref(count)
    let () = {
      push_back(queue, obj)
      num.contents = num.contents - 1
    }

    while !is_empty_queue(queue) && num.contents > 0 {
      let obj = unsafe_pop(queue)
      if typeof(obj) == "number" {
        let u = Float.toInt(Obj.magic(obj))
        s.contents = hash_mix_int(s.contents, u + u + 1)
        num.contents = num.contents - 1
      } else if typeof(obj) == "string" {
        s.contents = hash_mix_string(s.contents, (Obj.magic(obj): string))
        num.contents = num.contents - 1
      } else if typeof(obj) == "boolean" {
        ()
      } else if typeof(obj) == "undefined" {
        ()
      } else if typeof(obj) == "symbol" {
        ()
      } else if typeof(obj) == "function" {
        ()
      } else {
        let size = Obj.size(obj)
        if size != 0 {
          let obj_tag = Obj.tag(obj)
          let tag = Int.bitwiseOr(Int.shiftLeft(size, 10), obj_tag)
          s.contents = hash_mix_int(s.contents, tag)
          let block = {
            let v = size - 1
            if v < num.contents {
              v
            } else {
              num.contents
            }
          }
          for i in 0 to block {
            push_back(queue, Obj.getField(obj, i))
          }
        } else {
          let size: int = %raw(`function(obj,cb){
            var size = 0  
            for(var k in obj){
              cb(obj[k])
              ++ size
            }
            return size
          }`)(obj, v => push_back(queue, v))
          s.contents = hash_mix_int(s.contents, Int.shiftLeft(size, 10)) /* tag */
        }
      }
    }
    hash_final_mix(s.contents)
  }
}
