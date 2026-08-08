/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

/***
This is a specialized module for [`Belt_SortArray`](), the docs in that module also
applies here, except the comparator is fixed and inlined
*/

type element = int

/**
The same as [`Belt_SortArray.strictlySortedLength`]() except the comparator is fixed

**return** `+n` means increasing order  `-n` means negative order
*/
let strictlySortedLength: array<element> => int

/** `sorted(xs)` return true if `xs` is in non strict increasing order */
let isSorted: array<element> => bool

/**
The same as [`Belt_SortArray.stableSortInPlaceBy`]() except the comparator is fixed
*/
let stableSortInPlace: array<element> => unit

/**
The same as [`Belt_SortArray.stableSortBy`]() except the comparator is fixed
*/
let stableSort: array<element> => array<element>

/**
If value is not found and value is less than one or more elements in array,
the negative number returned is the bitwise complement of the index of the first element
that is larger than value.

If value is not found and value is greater than all elements in array,
the negative number returned is the bitwise complement of
(the index of the last element plus 1)

for example, if `key` is smaller than all elements return `-1` since `lnot (-1) = 0`
if `key` is larger than all elements return `- (len + 1)` since `lnot (-(len+1)) = len`
*/
let binarySearch: (array<element>, element) => int

/**
`union(src, src1ofs, src1len, src2, src2ofs, src2len, dst, dstofs, cmp)` assume
`src` and `src2` is strictly sorted. for equivalent elements, it is picked from
`src` also assume that `dst` is large enough to store all elements
*/
let union: (array<element>, int, int, array<element>, int, int, array<element>, int) => int

let intersect: (array<element>, int, int, array<element>, int, int, array<element>, int) => int

let diff: (array<element>, int, int, array<element>, int, int, array<element>, int) => int
