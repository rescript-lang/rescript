import type * as rescript from "@rescript/runtime/types";
import type * as Js_re from "./Js_re.js";

export type t = string;

export function charAt(arg1: number, obj: t): t;

export function charCodeAt(arg1: number, obj: t): number;

export function codePointAt(arg1: number, obj: t): rescript.option<number>;

export function concat(arg1: t, obj: t): t;

export function concatMany(arg1: t[], obj: t): t;

export function endsWith(arg1: t, obj: t): boolean;

export function endsWithFrom(arg1: t, arg2: number, obj: t): boolean;

export function includes(arg1: t, obj: t): boolean;

export function includesFrom(arg1: t, arg2: number, obj: t): boolean;

export function indexOf(arg1: t, obj: t): number;

export function indexOfFrom(arg1: t, arg2: number, obj: t): number;

export function lastIndexOf(arg1: t, obj: t): number;

export function lastIndexOfFrom(arg1: t, arg2: number, obj: t): number;

export function localeCompare(arg1: t, obj: t): number;

export function match_(arg1: Js_re.t, obj: t): rescript.option<rescript.option<t>[]>;

export function normalizeByForm(arg1: t, obj: t): t;

export function repeat(arg1: number, obj: t): t;

export function replace(arg1: t, arg2: t, obj: t): t;

export function replaceByRe(arg1: Js_re.t, arg2: t, obj: t): t;

export function unsafeReplaceBy0(
  arg1: Js_re.t,
  arg2: (arg0: t, arg1: number, arg2: t) => t,
  obj: t,
): t;

export function unsafeReplaceBy1(
  arg1: Js_re.t,
  arg2: (arg0: t, arg1: t, arg2: number, arg3: t) => t,
  obj: t,
): t;

export function unsafeReplaceBy2(
  arg1: Js_re.t,
  arg2: (arg0: t, arg1: t, arg2: t, arg3: number, arg4: t) => t,
  obj: t,
): t;

export function unsafeReplaceBy3(
  arg1: Js_re.t,
  arg2: (arg0: t, arg1: t, arg2: t, arg3: t, arg4: number, arg5: t) => t,
  obj: t,
): t;

export function search(arg1: Js_re.t, obj: t): number;

export function slice(from_: number, to_: number, obj: t): t;

export function sliceToEnd(from_: number, obj: t): t;

export function split(arg1: t, obj: t): t[];

export function splitAtMost(arg1: t, limit: number, obj: t): t[];

export function splitByRe(arg1: Js_re.t, obj: t): rescript.option<t>[];

export function splitByReAtMost(
  arg1: Js_re.t,
  limit: number,
  obj: t,
): rescript.option<t>[];

export function startsWith(arg1: t, obj: t): boolean;

export function startsWithFrom(arg1: t, arg2: number, obj: t): boolean;

export function substr(from_: number, obj: t): t;

export function substrAtMost(from_: number, length: number, obj: t): t;

export function substring(from_: number, to_: number, obj: t): t;

export function substringToEnd(from_: number, obj: t): t;

export function anchor(arg1: t, obj: t): t;

export function link(arg1: t, obj: t): t;
