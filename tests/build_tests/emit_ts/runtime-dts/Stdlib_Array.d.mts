// External contract for the one function of Stdlib_Array the examples use
// (packages/@rescript/runtime/Stdlib_Array.res): reduce(arr, init, f) is
// arr.reduce(f, init), the standard Array.prototype.reduce, untampered. It
// calls f with the accumulator, an element, its index and the array, for
// each index present, and returns init for an empty array.
export declare function reduce<A, B>(
  arr: readonly A[],
  init: B,
  f: (acc: B, x: A, index: number, array: readonly A[]) => B,
): B;
