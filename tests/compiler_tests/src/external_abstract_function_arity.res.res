// https://github.com/rescript-lang/rescript/issues/8038

module type A = {
  type t
  @module external dep: t = "dep"
}

module B: A = {
  type t = string => string
  @module external dep: t = "dep"
}
