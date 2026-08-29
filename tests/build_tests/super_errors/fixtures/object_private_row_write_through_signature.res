/* Dual of object_private_row_grants_set.res: a signature that does not
 grant @set cannot be written through from outside the module. */
module M: {
  type t = private {.."x": int}
} = {
  type t = private {.."x": int}
}
let mutate = (o: M.t) => o["x"] = 1
