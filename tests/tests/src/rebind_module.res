type a = ..
type b = ..

type a += A
/* A is shadowed so it will available any more,
   it seems record disamibigution does not 
   work with extensible type
*/

type b += AA /* name must be unique since 4.06 */

/* TODO: more tests for Local module A */
