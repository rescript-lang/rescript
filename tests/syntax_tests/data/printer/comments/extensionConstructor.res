/* c0 */ exception /* c1 */ Exit /* c2 */ = /* c3 */ Terminate /* c4 */ 

/* c0 */ exception /* c1 */ ExitEarly /* c2 */ (
  /* c4 */ int /* c5 */,
  /* c6 */ int /* c7 */
) // after

/* c0 */ exception /* c1 */ ExitEarly /* c2 */ (
  /* c4 */ int /* c5 */,
  /* c6 */ int /* c7 */
): /* c8 */ gadt // after

/* c0 */ type /* c1 */ Foo.Bar.t /* c2 */ +=
  | /* before Foo */ Foo // after Foo
  | /* before Bar */ Bar // after Bar

/* c0 */ type /* c1 */ Foo.Bar.t /* c2 */ </* c1.1 */ 'x /* c2.2 */> +=
  | /* before Foo */ Foo // after Foo
  | /* before Bar */ Bar // after Bar

/* c0 */ type /* c1 */ Foo.Bar.t /* c2 */ += /* c3 */ Bar /* c4 */

/* c0 */ type /* c1 */ t /* c2 */ +=
  | /* c3 */ Foo /* c4 */ =  /* c5 */ Bar /* c6 */
