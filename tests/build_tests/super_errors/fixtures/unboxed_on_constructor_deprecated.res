type mixed = | @unboxed Text(string) | Count(int)

@unboxed
type whole = | @unboxed Name(string) | Age(int)

type spreadMixed = | ...mixed | Flag(bool)

@unboxed
type plain = Plain(string)

@unboxed
type spreadPlain = | ...plain | Num(int)
