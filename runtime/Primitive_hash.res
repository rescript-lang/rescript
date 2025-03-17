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

module Float = Primitive_float_extern
module Obj = Primitive_object_extern
module Js = Primitive_js_extern
module String = Primitive_string_extern

@send external charCodeAt: (string, int) => int = "charCodeAt"

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

let rotl32 = (x: int, n) => lor(lsl(x, n), lsr(x, 32 - n))

let hash_mix_int = (h, d) => {
  let d = ref(d)
  d.contents = d.contents * 0xcc9e2d51
  d.contents = rotl32(d.contents, 15)
  d.contents = d.contents * 0x1b873593
  let h = ref(lxor(h, d.contents))
  h.contents = rotl32(h.contents, 13)
  h.contents + lsl(h.contents, 2) + 0xe6546b64
}

let hash_final_mix = h => {
  let h = ref(lxor(h, lsr(h, 16)))
  h.contents = h.contents * 0x85ebca6b
  h.contents = lxor(h.contents, lsr(h.contents, 13))
  h.contents = h.contents * 0xc2b2ae35
  lxor(h.contents, lsr(h.contents, 16))
}

let hash_mix_string = (h, s) => {
  let len = String.length(s)
  let block = len / 4 - 1
  let hash = ref(h)
  for i in 0 to block {
    let j = 4 * i
    let w = lor(
      lor(lor(s->charCodeAt(j), lsl(s->charCodeAt(j + 1), 8)), lsl(s->charCodeAt(j + 2), 16)),
      lsl(s->charCodeAt(j + 3), 24),
    )

    hash.contents = hash_mix_int(hash.contents, w)
  }
  let modulo = land(len, 0b11)
  if modulo != 0 {
    let w = if modulo == 3 {
      lor(
        lor(lsl(s->charCodeAt(len - 1), 16), lsl(s->charCodeAt(len - 2), 8)),
        s->charCodeAt(len - 3),
      )
    } else if modulo == 2 {
      lor(lsl(s->charCodeAt(len - 1), 8), s->charCodeAt(len - 2))
    } else {
      s->charCodeAt(len - 1)
    }

    hash.contents = hash_mix_int(hash.contents, w)
  }
  hash.contents = lxor(hash.contents, len)
  hash.contents
}

let hash = (count: int, _limit, seed: int, obj: Obj.t): int => {
  let s = ref(seed)
  if Js.typeof(obj) == "number" {
    let u = Float.toInt(Obj.magic(obj))
    s.contents = hash_mix_int(s.contents, u + u + 1)
    hash_final_mix(s.contents)
  } else if Js.typeof(obj) == "string" {
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
      if Js.typeof(obj) == "number" {
        let u = Float.toInt(Obj.magic(obj))
        s.contents = hash_mix_int(s.contents, u + u + 1)
        num.contents = num.contents - 1
      } else if Js.typeof(obj) == "string" {
        s.contents = hash_mix_string(s.contents, (Obj.magic(obj): string))
        num.contents = num.contents - 1
      } else if Js.typeof(obj) == "boolean" {
        ()
      } else if Js.typeof(obj) == "undefined" {
        ()
      } else if Js.typeof(obj) == "symbol" {
        ()
      } else if Js.typeof(obj) == "function" {
        ()
      } else {
        let size = Obj.size(obj)
        if size != 0 {
          let obj_tag = Obj.tag(obj)
          let tag = lor(lsl(size, 10), obj_tag)
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
          s.contents = hash_mix_int(s.contents, lor(lsl(size, 10), 0)) /* tag */
        }
      }
    }
    hash_final_mix(s.contents)
  }
}
