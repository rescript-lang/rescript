/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

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
