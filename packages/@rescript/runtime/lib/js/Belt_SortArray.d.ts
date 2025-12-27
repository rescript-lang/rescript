export function strictlySortedLength<A>(
  xs: A[],
  lt: (arg0: A, arg1: A) => boolean,
): number;

export function isSorted<A>(a: A[], cmp: (arg0: A, arg1: A) => number): boolean;

export function union<A>(
  src: A[],
  src1ofs: number,
  src1len: number,
  src2: A[],
  src2ofs: number,
  src2len: number,
  dst: A[],
  dstofs: number,
  cmp: (arg0: A, arg1: A) => number,
): number;

export function intersect<A>(
  src: A[],
  src1ofs: number,
  src1len: number,
  src2: A[],
  src2ofs: number,
  src2len: number,
  dst: A[],
  dstofs: number,
  cmp: (arg0: A, arg1: A) => number,
): number;

export function diff<A>(
  src: A[],
  src1ofs: number,
  src1len: number,
  src2: A[],
  src2ofs: number,
  src2len: number,
  dst: A[],
  dstofs: number,
  cmp: (arg0: A, arg1: A) => number,
): number;

export function stableSortInPlaceBy<A>(a: A[], cmp: (arg0: A, arg1: A) => number): void;

export function stableSortBy<A>(a: A[], cmp: (arg0: A, arg1: A) => number): A[];

export function binarySearchBy<A>(
  sorted: A[],
  key: A,
  cmp: (arg0: A, arg1: A) => number,
): number;

export function binarySearchByU<A>(
  arg0: A[],
  arg1: A,
  arg2: (arg0: A, arg1: A) => number,
): number;

export function diffU<A>(
  arg0: A[],
  arg1: number,
  arg2: number,
  arg3: A[],
  arg4: number,
  arg5: number,
  arg6: A[],
  arg7: number,
  arg8: (arg0: A, arg1: A) => number,
): number;

export function intersectU<A>(
  arg0: A[],
  arg1: number,
  arg2: number,
  arg3: A[],
  arg4: number,
  arg5: number,
  arg6: A[],
  arg7: number,
  arg8: (arg0: A, arg1: A) => number,
): number;

export function isSortedU<A>(arg0: A[], arg1: (arg0: A, arg1: A) => number): boolean;

export function stableSortByU<A>(arg0: A[], arg1: (arg0: A, arg1: A) => number): A[];

export function stableSortInPlaceByU<A>(
  arg0: A[],
  arg1: (arg0: A, arg1: A) => number,
): void;

export function strictlySortedLengthU<A>(
  arg0: A[],
  arg1: (arg0: A, arg1: A) => boolean,
): number;

export function unionU<A>(
  arg0: A[],
  arg1: number,
  arg2: number,
  arg3: A[],
  arg4: number,
  arg5: number,
  arg6: A[],
  arg7: number,
  arg8: (arg0: A, arg1: A) => number,
): number;
