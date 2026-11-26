/* A transparent manifest is an equation: unlike a private row
   (object_private_row_grants_set.res) or a coercion, it cannot forget a
   field's @set - the flags must be equal in both directions. */
module M: {
  type t = {"x": int}
} = {
  type t = {@set "x": int}
}
