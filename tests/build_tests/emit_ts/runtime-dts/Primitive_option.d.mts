// External contract for Primitive_option.some and valFromOption
// (packages/@rescript/runtime/Primitive_option.res), the boxing of a payload
// into Some and its unboxing, at a primitive payload. Each returns its
// argument x unless x is undefined, or is not null and owns or inherits a
// defined BS_PRIVATE_NESTED_SOME_NONE (a nested-option marker). A string,
// number, boolean or bigint is not undefined and owns no such key, and its
// standard prototype chain, untampered, holds none, so x is returned.
export declare function some<A extends string | number | boolean | bigint>(
  x: A,
): A;
export declare function valFromOption<
  A extends string | number | boolean | bigint,
>(x: A): A;
