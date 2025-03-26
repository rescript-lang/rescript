(* Copyright (C) 2015-2016 Bloomberg Finance L.P.
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA. *)

type t = int

let compare (x : t) (y : t) = Stdlib.compare x y

let equal (x : t) (y : t) = x = y

let move = 0x1_0000_0000

(* works only on 64 bit platform *)
let int32_unsigned_to_int (n : int32) : int =
  let i = Int32.to_int n in
  if i < 0 then i + move else i

let int32_pow (x : int32) (y : int32) =
  let x_float = Int32.to_float x in
  let y_float = Int32.to_float y in
  let result = x_float ** y_float in
  let truncated =
    if result > 2147483647.0 || result < -2147483648.0 then
      let i = int_of_float result in
      i land 0xFFFFFFFF
    else int_of_float result
  in
  Int32.of_int truncated
