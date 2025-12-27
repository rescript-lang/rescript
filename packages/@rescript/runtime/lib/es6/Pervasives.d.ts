import type * as rescript from "@rescript/runtime/types";

export type fpclass =
  | "FP_normal"
  | "FP_subnormal"
  | "FP_zero"
  | "FP_infinite"
  | "FP_nan";

export interface ref<A> {
  contents: A;
}

export type int32 = number;

export function failwith<A>(s: string): A;

export function invalid_arg<A>(s: string): A;

export function abs(x: number): number;

export const max_int: number;

export const min_int: number;

export const infinity: number;

export const neg_infinity: number;

export const max_float: number;

export const min_float: number;

export const epsilon_float: number;

export function classify_float(x: number): fpclass;

export function char_of_int(n: number): number;

export function string_of_bool(b: boolean): string;

export function bool_of_string(param: string): boolean;

export function bool_of_string_opt(param: string): rescript.option<boolean>;

export function int_of_string_opt(s: string): rescript.option<number>;

export function $at<A>(l1: rescript.list<A>, l2: rescript.list<A>): rescript.list<A>;
