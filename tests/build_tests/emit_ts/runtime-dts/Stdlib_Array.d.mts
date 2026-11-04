// External contract for the one function of Stdlib_Array the examples use:
// reduce(arr, init, f) is arr.reduce(f, init).
export declare function reduce<A, B>(
  arr: readonly A[],
  init: B,
  f: (acc: B, x: A) => B,
): B;
