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

@deprecated({
  reason: "Use `option` directly instead.",
  migrate: %replace.type(: option),
})
type t<'a> = option<'a>

@deprecated({
  reason: "Use `Some()` directly instead.",
  migrate: Some(),
})
let some: 'a => option<'a>

@deprecated({
  reason: "Use `Option.isSome` instead.",
  migrate: Option.isSome(),
})
let isSome: option<'a> => bool

// Skipping automatic migration because this is a rather weird function that is probably not that much used.
@deprecated("Use `Option.equal` instead.")
let isSomeValue: (('a, 'a) => bool, 'a, option<'a>) => bool

@deprecated({
  reason: "Use `Option.isNone` instead.",
  migrate: Option.isNone(),
})
let isNone: option<'a> => bool

@deprecated({
  reason: "Use `Option.getOrThrow` instead.",
  migrate: Option.getOrThrow(),
})
let getExn: option<'a> => 'a

@deprecated({
  reason: "Use `Option.equal` instead.",
  migrate: Option.equal(
    %insert.unlabelledArgument(1),
    %insert.unlabelledArgument(2),
    %insert.unlabelledArgument(0),
  ),
})
let equal: (('a, 'b) => bool, option<'a>, option<'b>) => bool

@deprecated({
  reason: "Use `Option.flatMap` instead.",
  migrate: Option.flatMap(%insert.unlabelledArgument(1), %insert.unlabelledArgument(0)),
  migrateInPipeChain: Option.flatMap(%insert.unlabelledArgument(0)),
})
let andThen: ('a => option<'b>, option<'a>) => option<'b>

@deprecated({
  reason: "Use `Option.map` instead.",
  migrate: Option.map(%insert.unlabelledArgument(1), %insert.unlabelledArgument(0)),
  migrateInPipeChain: Option.map(%insert.unlabelledArgument(0)),
})
let map: ('a => 'b, option<'a>) => option<'b>

@deprecated({
  reason: "Use `Option.getOr` instead.",
  migrate: Option.getOr(%insert.unlabelledArgument(1), %insert.unlabelledArgument(0)),
  migrateInPipeChain: Option.getOr(%insert.unlabelledArgument(0)),
})
let getWithDefault: ('a, option<'a>) => 'a

@deprecated({
  reason: "Use `Option.getOr` instead. Note: `default` has special meaning in ES modules.",
  migrate: Option.getOr(%insert.unlabelledArgument(1), %insert.unlabelledArgument(0)),
  migrateInPipeChain: Option.getOr(%insert.unlabelledArgument(0)),
})
let default: ('a, option<'a>) => 'a

@deprecated({
  reason: "Use `Option.filter` instead.",
  migrate: Option.filter(%insert.unlabelledArgument(1), %insert.unlabelledArgument(0)),
  migrateInPipeChain: Option.filter(%insert.unlabelledArgument(0)),
})
let filter: ('a => bool, option<'a>) => option<'a>

@deprecated({
  reason: "Use `Option.orElse` instead.",
  migrate: Option.orElse(),
  migrateInPipeChain: Option.orElse(%insert.unlabelledArgument(0)),
})
let firstSome: (option<'a>, option<'a>) => option<'a>
