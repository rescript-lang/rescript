/* A constructor's runtime tag is its decoded value, not the source spelling.
 The signature and implementation therefore describe the same variant. */
module Renamed: {
  @unboxed type t = | @as("\u0041") Renamed

  let value: t
} = {
  @unboxed type t = | @as("A") Renamed

  let value = Renamed
}
