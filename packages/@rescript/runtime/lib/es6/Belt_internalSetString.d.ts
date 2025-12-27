import type * as rescript from "@rescript/runtime/types";
import type * as Belt_internalAVLset from "./Belt_internalAVLset.js";
import type * as Js from "./Js.js";

export type value = string;

export type t = Belt_internalAVLset.t<value>;

export function has(t: t, x: value): boolean;

export function compareAux(
  e1: rescript.list<Belt_internalAVLset.node<value>>,
  e2: rescript.list<Belt_internalAVLset.node<value>>,
): number;

export function cmp(
  s1: Belt_internalAVLset.t<value>,
  s2: Belt_internalAVLset.t<value>,
): number;

export function eq(s1: t, s2: Belt_internalAVLset.t<value>): boolean;

export function subset(s1: t, s2: t): boolean;

export function get(n: t, x: value): rescript.option<value>;

export function getUndefined(n: t, x: value): Js.undefined_<value>;

export function getOrThrow(n: t, x: value): value;

export function addMutate(
  t: Belt_internalAVLset.t<value>,
  x: value,
): Belt_internalAVLset.t<value>;

export function fromArray(xs: value[]): Belt_internalAVLset.t<value>;
