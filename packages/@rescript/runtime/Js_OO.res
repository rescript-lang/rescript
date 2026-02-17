/* Copyright (C) 2015-2016 Bloomberg Finance L.P.
 * Copyright (C) 2017- Hongbo Zhang, Authors of ReScript
 *
 * SPDX-License-Identifier: MIT
 */

@@config({flags: ["-unboxed-types"]})

external unsafe_to_method: 'a => 'a = "%unsafe_to_method"

module Callback = {
  type arity1<'a> = {@internal i1: 'a}
  type arity2<'a> = {@internal i2: 'a}
  type arity3<'a> = {@internal i3: 'a}
  type arity4<'a> = {@internal i4: 'a}
  type arity5<'a> = {@internal i5: 'a}
  type arity6<'a> = {@internal i6: 'a}
  type arity7<'a> = {@internal i7: 'a}
  type arity8<'a> = {@internal i8: 'a}
  type arity9<'a> = {@internal i9: 'a}
  type arity10<'a> = {@internal i10: 'a}
  type arity11<'a> = {@internal i11: 'a}
  type arity12<'a> = {@internal i12: 'a}
  type arity13<'a> = {@internal i13: 'a}
  type arity14<'a> = {@internal i14: 'a}
  type arity15<'a> = {@internal i15: 'a}
  type arity16<'a> = {@internal i16: 'a}
  type arity17<'a> = {@internal i17: 'a}
  type arity18<'a> = {@internal i18: 'a}
  type arity19<'a> = {@internal i19: 'a}
  type arity20<'a> = {@internal i20: 'a}
  type arity21<'a> = {@internal i21: 'a}
  type arity22<'a> = {@internal i22: 'a}
}
