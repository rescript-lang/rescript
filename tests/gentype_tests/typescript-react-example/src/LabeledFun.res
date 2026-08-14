@genType
let labelled = (a, ~b=3, ~c, d, ~e, ~f) => a + b + c + d + e + f

/* Pins @genType.as renaming of a function parameter: the annotation lives on
 the parameter (Typedtree.arg attrs), not on the arrow node or the type. */
@genType
type renamedParamCb = (@genType.as("renamed") ~first: int, ~second: int) => int
