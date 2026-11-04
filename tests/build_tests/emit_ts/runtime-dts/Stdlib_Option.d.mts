// External contract for the two functions of Stdlib_Option the examples use.
// Stated for an element type that does not itself include undefined: the
// runtime boxes a nested None (Primitive_option.some), which `A | undefined`
// does not describe.
export declare function map<A, B>(
  opt: A | undefined,
  f: (x: A) => B,
): B | undefined;
export declare function getOr<A>(opt: A | undefined, d: A): A;
