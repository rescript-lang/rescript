/* Writing through the ORIGINAL parameter strengthens its requirement, so a
   read-only caller is rejected. Writing through an annotated alias imposes
   the same requirement (mutability classes are merged by unification) —
   see the paired fixture object_write_alias.res. */
@val external readonly: {"x": int} = "readonly"

let writeOriginal = (o: {.."x": int}) => {
  let _alias: {.."x": int} = o
  o["x"] = 1
}

let rejected = writeOriginal(readonly)
