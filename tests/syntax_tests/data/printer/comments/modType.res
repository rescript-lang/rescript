// Pmty_ident
/* c0 */ module type /* c1 */ T /* c2 */ = /* c3 */ Tree /* c4 */

// Pmty_signature
/* c0 */ module type /* c1 */ T /* c2 */ = /* c3 */ {
  let a: int
} /* c4 */

// Pmty_extension
/* c0 */ module type /* c1 */ T /* c2 */ = /* c3 */ %ext(/* c4 */"test" /* c5 */) /* c6 */

// Pmty_typeof 
/* c0 */ module type /* c1 */ A /* c2 */ =
  /* c3 */ module type of /* c4 */ {
    let a /* inside */ = 1
    } /* c5 */

// Pmty_with
/* c0 */ module type /* c1 */ A /* c2 */ = /* c3 */ Foo /* c4 */ with type t = string // end

// Pmty_functor
/* c0 */ module type /* c1 */ Functor /* c2 */ = /* c3 */ SetLike /* c4 */ => /* c5 */ Set /* c6 */
module type Functor = /* before */ (
  /* before S */ S /* after S */: /* before */ SetLike /* after */,
  /* before B */ B /* after B */: /* before */ BtreeLike /* after */
) => /* before NeoTree */ NeoTree // after NeoTree
module type Functor = (
 /* c0 */ _ /* c1*/: /* c2 */ SetLike /* c3 */,
 /* c4 */ _ /* c5 */: /* c6 */ BtreeLike /* c7 */
) => NeoTree

module type Functor = /* before */ (
  /* c0 */ SetLike /* c1 */,
  /* c2 */ BtreeLike /* c3 */,
) => NeoTree
