export type element = number;

export function strictlySortedLength(xs: element[]): number;

export function isSorted(a: element[]): boolean;

export function union(
  src: element[],
  src1ofs: number,
  src1len: number,
  src2: element[],
  src2ofs: number,
  src2len: number,
  dst: element[],
  dstofs: number,
): number;

export function intersect(
  src: element[],
  src1ofs: number,
  src1len: number,
  src2: element[],
  src2ofs: number,
  src2len: number,
  dst: element[],
  dstofs: number,
): number;

export function diff(
  src: element[],
  src1ofs: number,
  src1len: number,
  src2: element[],
  src2ofs: number,
  src2len: number,
  dst: element[],
  dstofs: number,
): number;

export function stableSortInPlace(a: element[]): void;

export function stableSort(a: element[]): element[];

export function binarySearch(sorted: element[], key: element): number;
