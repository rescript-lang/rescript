/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

type t = Primitive_object_extern.t

let updateDummy: (t, t) => unit

let compare: (t, t) => int

type eq = (t, t) => bool
let equal: eq
let notequal: eq
let greaterequal: eq
let greaterthan: eq
let lessthan: eq
let lessequal: eq

let min: (t, t) => t
let max: (t, t) => t
