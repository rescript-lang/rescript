@@config({flags: ["-unboxed-types"]})

@unboxed
type null<+'a> = Value('a) | @as(null) Null

type undefined<+'a>

@unboxed type nullable<+'a> = Value('a) | @as(null) Null | @as(undefined) Undefined

type null_undefined<+'a> = nullable<'a>

external null: null<'a> = "%null"

external undefined: undefined<'a> = "%undefined"

external isNullable: nullable<'a> => bool = "%is_nullable"

external testAny: 'a => bool = "%is_nullable"

external typeof: 'a => string = "%typeof"

external eqNull: ('a, null<'a>) => bool = "%equal_null"

external eqUndefined: ('a, undefined<'a>) => bool = "%equal_undefined"

external eqNullable: ('a, nullable<'a>) => bool = "%equal_nullable"

external lt: ('a, 'a) => bool = "%unsafe_lt"

external le: ('a, 'a) => bool = "%unsafe_le"

external gt: ('a, 'a) => bool = "%unsafe_gt"

external ge: ('a, 'a) => bool = "%unsafe_ge"

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
