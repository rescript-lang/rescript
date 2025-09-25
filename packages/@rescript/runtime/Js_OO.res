/* Copyright (C) 2020- Hongbo Zhang, Authors of ReScript
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

@@config({flags: ["-unboxed-types"]})

@deprecated("This has been deprecated and will be removed in v13.")
external unsafe_to_method: 'a => 'a = "%unsafe_to_method"

module Callback = {
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity1<'a> = {@internal i1: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity2<'a> = {@internal i2: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity3<'a> = {@internal i3: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity4<'a> = {@internal i4: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity5<'a> = {@internal i5: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity6<'a> = {@internal i6: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity7<'a> = {@internal i7: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity8<'a> = {@internal i8: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity9<'a> = {@internal i9: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity10<'a> = {@internal i10: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity11<'a> = {@internal i11: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity12<'a> = {@internal i12: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity13<'a> = {@internal i13: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity14<'a> = {@internal i14: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity15<'a> = {@internal i15: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity16<'a> = {@internal i16: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity17<'a> = {@internal i17: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity18<'a> = {@internal i18: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity19<'a> = {@internal i19: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity20<'a> = {@internal i20: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity21<'a> = {@internal i21: 'a}
  @deprecated("This has been deprecated and will be removed in v13.")
  type arity22<'a> = {@internal i22: 'a}
}
