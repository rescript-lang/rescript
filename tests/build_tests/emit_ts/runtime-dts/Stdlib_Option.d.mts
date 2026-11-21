// External contract for the two functions of Stdlib_Option
// (packages/@rescript/runtime/Stdlib_Option.res) the examples use, in terms
// of Primitive_option.some and valFromOption (Primitive_option.d.mts):
// - map(opt, f) is undefined for undefined, else some(f(valFromOption(opt))),
//   calling f once with one argument; stated at primitive payloads, where
//   some and valFromOption return their argument;
// - getOr(opt, d) is d for undefined, else valFromOption(opt); stated at a
//   primitive A, an option of one, and an option of an option of one.
type M0 = { readonly BS_PRIVATE_NESTED_SOME_NONE: 0 };
type M1 = { readonly BS_PRIVATE_NESTED_SOME_NONE: 1 };
export declare function map<
  A extends string | number | boolean | bigint,
  B extends string | number | boolean | bigint,
>(opt: undefined | A, f: (x: A) => B): undefined | B;
export declare function getOr<A extends string | number | boolean | bigint>(
  opt: undefined | A,
  d: A,
): A;
export declare function getOr<A extends string | number | boolean | bigint>(
  opt: undefined | A | M0,
  d: undefined | A,
): undefined | A;
export declare function getOr<A extends string | number | boolean | bigint>(
  opt: undefined | A | M0 | M1,
  d: undefined | A | M0,
): undefined | A | M0;
