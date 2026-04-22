// Legacy uncurried `(. ...)` type syntax — accepted for backwards
// compatibility but emits a deprecation warning.

type t1 = (. int) => int
type t2 = (. int, int) => int
type t3 = (. unit) => unit
