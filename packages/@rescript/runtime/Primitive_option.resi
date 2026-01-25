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

type nested = {@as("BS_PRIVATE_NESTED_SOME_NONE") depth: int}

let fromNullable: Primitive_js_extern.null_undefined<'a> => option<'a>

let fromUndefined: Primitive_js_extern.undefined<'a> => option<'a>

let fromNull: Primitive_js_extern.null<'a> => option<'a>

let valFromOption: Primitive_object_extern.t => Primitive_object_extern.t

let some: Primitive_object_extern.t => Primitive_object_extern.t

let isNested: Primitive_object_extern.t => bool

let toUndefined: option<Primitive_object_extern.t> => Primitive_js_extern.undefined<
  Primitive_object_extern.t,
>

type poly

/** When it is None, return None
    When it is Some (#a(3)) return 3
*/
let unwrapPolyVar: option<poly> => Primitive_object_extern.t
