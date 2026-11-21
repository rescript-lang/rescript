// External contract for Primitive_option.some and valFromOption
// (packages/@rescript/runtime/Primitive_option.res), the boxing of a payload
// into Some and its unboxing, one signature per nesting of the payload: a
// primitive A, an option of one, and an option of an option of one.
// - some(x) is the frozen marker {BS_PRIVATE_NESTED_SOME_NONE: 0} when x is
//   undefined, the marker of depth d + 1 when x is not null and its key
//   BS_PRIVATE_NESTED_SOME_NONE reads a defined d, and x otherwise.
// - valFromOption(x) is undefined when x is not null and its key reads 0, the
//   marker of depth d - 1 when it reads another defined d, and x otherwise.
// A string, number, boolean or bigint is not undefined and owns no such key,
// and its standard prototype chain, untampered, holds none, so x is returned;
// a value at a marker type reads its depth at the key.
type M0 = { readonly BS_PRIVATE_NESTED_SOME_NONE: 0 };
type M1 = { readonly BS_PRIVATE_NESTED_SOME_NONE: 1 };
export declare function some<A extends string | number | boolean | bigint>(
  x: A,
): A;
export declare function some<A extends string | number | boolean | bigint>(
  x: undefined | A,
): A | M0;
export declare function some<A extends string | number | boolean | bigint>(
  x: undefined | A | M0,
): A | M0 | M1;
export declare function valFromOption<
  A extends string | number | boolean | bigint,
>(x: A): A;
export declare function valFromOption<
  A extends string | number | boolean | bigint,
>(x: A | M0): undefined | A;
export declare function valFromOption<
  A extends string | number | boolean | bigint,
>(x: A | M0 | M1): undefined | A | M0;
