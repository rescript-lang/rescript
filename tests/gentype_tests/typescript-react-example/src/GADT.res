// Warning here: GADT exported as opaque
@gentype
type rec t = Any('a): t | Anytwo('b, 'c): t

// No warning here
@gentype.opaque
type rec tt = Any('a): tt | Anytwo('b, 'c): tt
