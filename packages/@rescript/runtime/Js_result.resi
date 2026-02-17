/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

@deprecated({
  reason: "Use `result` directly instead.",
  migrate: %replace.type(: result),
})
type t<+'good, +'bad> =
  | Ok('good)
  | Error('bad)
