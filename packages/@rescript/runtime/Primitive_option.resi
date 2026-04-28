/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

type nested = {@as("BS_PRIVATE_NESTED_SOME_NONE") depth: int}

let fromNullable: Primitive_js_extern.null_undefined<'a> => option<'a>

let fromUndefined: Primitive_js_extern.undefined<'a> => option<'a>

let fromNull: Primitive_js_extern.null<'a> => option<'a>

let valFromOption: Primitive_object_extern.t => Primitive_object_extern.t

let some: Primitive_object_extern.t => Primitive_object_extern.t

let isNested: Primitive_object_extern.t => bool

type poly

/** When it is None, return None
    When it is Some (#a(3)) return 3
*/
let unwrapPolyVar: option<poly> => Primitive_object_extern.t
