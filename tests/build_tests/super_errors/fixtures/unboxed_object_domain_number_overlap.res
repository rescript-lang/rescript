type integer = {kind: int, value: int}
type decimal = {kind: float, value: int}
@unboxed type t = Integer(integer) | Decimal(decimal)
