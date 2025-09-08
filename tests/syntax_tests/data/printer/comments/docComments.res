/*** This is a module comment */

/*** This is another module comment */

/** This is a doc ✅ comment */
let z = 34

@@res.doc("And this is a res.doc module annotation")

@res.doc("And this is a res.doc ✅ annotation")
let q = 11

/** This
  * is a multi-line
  multiline doc comment
  */
type h = int

/* comment and attribute */
@foo let x = 10

/** doc comment and attribute */
@foo let x = 10

/** doc comment and 3 attributes */
@foo @bar @baz let x = 10

/** doc comment and 0 attributes */
let x = 10

type pathItem = {}
/** Issue 6844: doc comment before "and" */
and operation =  {}
