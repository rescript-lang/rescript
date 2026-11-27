/* Writing through an ANNOTATED ALIAS strengthens the original parameter
   exactly as writing through the parameter does (mutability classes are
   merged by unification), so a read-only caller is rejected. The pair of
   this fixture is object_write_original_after_alias.res; both forms impose
   the same requirement. */
@val external readonly: {"x": int} = "readonly"

let writeAlias = (o: {.."x": int}) => {
  let alias: {.."x": int} = o
  alias["x"] = 1
}

let rejected = writeAlias(readonly)
