/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

let hash_mix_int: (int, int) => int
let hash_mix_string: (int, string) => int
let hash_final_mix: int => int
let hash: (int, 'a, int, Primitive_object.t) => int
