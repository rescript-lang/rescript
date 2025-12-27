import type * as Primitive_object from "./Primitive_object.js";

export function hash_mix_int(h: number, d: number): number;

export function hash_final_mix(h: number): number;

export function hash_mix_string(h: number, s: string): number;

export function hash<A>(
  count: number,
  _limit: A,
  seed: number,
  obj: Primitive_object.t,
): number;
