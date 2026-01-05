import type * as rescript from "@rescript/runtime/types";
import type * as Stdlib_RegExp from "./Stdlib_RegExp.js";

export type t = string;

export function charCodeAt(s: string, i: number): rescript.option<number>;

export function indexOfOpt(s: string, search: string): rescript.option<number>;

export function lastIndexOfOpt(s: string, search: string): rescript.option<number>;

export type normalizeForm =
  | "NFC"
  | "NFD"
  | "NFKC"
  | "NFKD";

export function searchOpt(s: string, re: Stdlib_RegExp.t): rescript.option<number>;

export function isEmpty(s: string): boolean;

export function capitalize(s: string): string;
