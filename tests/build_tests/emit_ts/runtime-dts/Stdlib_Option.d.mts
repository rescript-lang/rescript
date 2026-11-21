// External contract for the two functions of Stdlib_Option
// (packages/@rescript/runtime/Stdlib_Option.res) the examples use, at
// primitive payloads, where Primitive_option.some and valFromOption return
// their argument (Primitive_option.d.mts):
// - map(opt, f) is undefined for undefined, else some(f(valFromOption(opt))),
//   calling f once with one argument;
// - getOr(opt, d) is d for undefined, else valFromOption(opt).
export declare function map<
  A extends string | number | boolean | bigint,
  B extends string | number | boolean | bigint,
>(opt: undefined | A, f: (x: A) => B): undefined | B;
export declare function getOr<A extends string | number | boolean | bigint>(
  opt: undefined | A,
  d: A,
): A;
