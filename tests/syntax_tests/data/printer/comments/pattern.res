// Ppat_any
let /* before */ _ /* after */ = 1

// Ppat_var
let /* before */ x /* after */ = 1

// Ppat_constant
let /* before */ 123 /* after */ = 123

// Ppat_interval not supported
// Ppat_interval of constant * constant

// Ppat_alias
let /* before pat */ p /* after pat */ as /* before alias */ x /* after alias */ = ()
let /*c0 */ (/* c1 */ p /* c2 */ as /* c3 */x /* c4*/) as /* c5 */ y /* c6 */ = ()

// Ppat_array
let /* c0 */ [/* c1 */ a /* c2 */, /* c3 */b /* c4 */, /* c5 */ c /* c6 */] /* c7 */ = [1, 2, 3]

// Ppat_construct
let /* here */ Black /* there */ = color

let /* here */ Black(/* inside */)/* there */ = color
let Black(/* inside */ /* inside2 */) = color
let Black(/* inside */ /* inside2 */ /* inside 3 */) = color
let Black(
// singleLineComment
) = color

let module(/* c0 */ X /* c1 */ : /* c2 */ X_int /* c3 */) /* c4 */ = x 

let /* before */ Rgb /* after constr */ (/* red */ r /* red2 */, /* green */ g/* green2 */, /* blue */ b /* blue2 */) /* after */ = color

switch a {
| Constr /* after constr */ 
| Constr /* after constr */ (a, _) => a
}

let () = ()
let [/* inside */] = []
let list{/* inside */} = list{}

let /* before */ list{/* a1 */ a /* a2 */, /* b1 */ b /* b2 */} /* after */ = list{1, 2}
let /* before */ list{/* a1 */ a /* a2 */, /* b1 */ ...b /* b2 */} /* after */ = list{1, 2}

// Ppat_record
let /* before */ {/* c0 */ name /* c1 */, /* c2 */ age /* c3 */} /* after */ = {
  name: "steve",
  age: 31,
}

let /* before */ {
  /* c0 */ name /* c1 */: /* c2 */ firstName /* c3 */,
  /* c3 */ age /* c4 */: /* c5 */ ageInYears /* c6 */,
} /* after */ = {name: "steve", age: 31}

// Ppat_or
let /* b1 */ Blue /* b2 */ | /* c1 */ Red /* c2 */ = color
let /* b1 */ Blue /* b2 */ | /* c1 */ Red /* c2 */ | /* d1 */ Green /* d2 */ = color

// #462: Comments in nested Ppat_or must not disappear.
// (comment positions not optimal yet because `|` has no AST location)
let _ = switch "anything" {
// above 1
| "1"
// below 1
| "2"
// below 2
| "3"
// below 3
| _ => "4"
// below 4
}

// Ppat_constraint
let /* c0 */ number /* c1 */: /* c2 */ int /* c3 */ = 123

// Ppat_unpack 
let /* before */ module(/* h1 */ Hashtbl /* h2 */) /* after */ = Hashtbl
let /* before */ module(/* h1 */ Hashtbl /* h2 */: /* h3 */ MutableTable /* h4 */) /* after */ = Hashtbl

// Ppat_exception
let /* before */ exception /* c0 */ Exit /* c1 */ /* after */ = exc

// Ppat_extension
let /* before */ %raw("eval(gc())") /* after */ = stuff

// Ppat_interval
let /* c0 */ 'a' .. 'z' /* c1 */ = x
