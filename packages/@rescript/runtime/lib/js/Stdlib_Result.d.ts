import type * as rescript from "@rescript/runtime/types";
import type * as Stdlib_Ordering from "./Stdlib_Ordering.js";

export type t<Res, Err> = rescript.result<Res, Err>;

export function getOrThrow<A, B>(x: rescript.result<A, B>, message?: string): A;

export function getExn<A, B>(arg0: rescript.result<A, B>, message?: string): A;

export function mapOr<A, C, B>(opt: rescript.result<A, C>, default_: B, f: (arg0: A) => B): B;

export function mapWithDefault<A, C, B>(
  arg0: rescript.result<A, C>,
  arg1: B,
  arg2: (arg0: A) => B,
): B;

export function map<A, C, B>(
  opt: rescript.result<A, C>,
  f: (arg0: A) => B,
): rescript.result<B, C>;

export function flatMap<A, C, B>(
  opt: rescript.result<A, C>,
  f: (arg0: A) => rescript.result<B, C>,
): rescript.result<B, C>;

export function getOr<A, B>(opt: rescript.result<A, B>, default_: A): A;

export function getWithDefault<A, B>(arg0: rescript.result<A, B>, arg1: A): A;

export function isOk<A, B>(x: rescript.result<A, B>): boolean;

export function isError<A, B>(x: rescript.result<A, B>): boolean;

export function equal<A, C, B, D>(
  a: rescript.result<A, C>,
  b: rescript.result<B, D>,
  eqOk: (arg0: A, arg1: B) => boolean,
  eqError: (arg0: C, arg1: D) => boolean,
): boolean;

export function compare<A, C, B, D>(
  a: rescript.result<A, C>,
  b: rescript.result<B, D>,
  cmpOk: (arg0: A, arg1: B) => Stdlib_Ordering.t,
  cmpError: (arg0: C, arg1: D) => Stdlib_Ordering.t,
): Stdlib_Ordering.t;

export function forEach<A, B>(r: rescript.result<A, B>, f: (arg0: A) => void): void;

export function mapError<A, B, C>(
  r: rescript.result<A, B>,
  f: (arg0: B) => C,
): rescript.result<A, C>;

export function all<A, B>(results: rescript.result<A, B>[]): rescript.result<A[], B>;

export function all2<R1, E, R2>(
  param: [rescript.result<R1, E>, rescript.result<R2, E>],
): rescript.result<[R1, R2], E>;

export function all3<R1, E, R2, R3>(
  param: [rescript.result<R1, E>, rescript.result<R2, E>, rescript.result<R3, E>],
): rescript.result<[R1, R2, R3], E>;

export function all4<R1, E, R2, R3, R4>(
  param: [rescript.result<R1, E>, rescript.result<R2, E>, rescript.result<R3, E>, rescript.result<R4, E>],
): rescript.result<[R1, R2, R3, R4], E>;

export function all5<R1, E, R2, R3, R4, R5>(
  param: [rescript.result<R1, E>, rescript.result<R2, E>, rescript.result<R3, E>, rescript.result<R4, E>, rescript.result<R5, E>],
): rescript.result<[R1, R2, R3, R4, R5], E>;

export function all6<R1, E, R2, R3, R4, R5, R6>(
  param: [rescript.result<R1, E>, rescript.result<R2, E>, rescript.result<R3, E>, rescript.result<R4, E>, rescript.result<R5, E>, rescript.result<R6, E>],
): rescript.result<[R1, R2, R3, R4, R5, R6], E>;

export function mapOkAsync<Ok, Error, MappedOk>(
  res: Promise<rescript.result<Ok, Error>>,
  f: (arg0: Ok) => MappedOk,
): Promise<rescript.result<MappedOk, Error>>;

export function mapErrorAsync<Ok, Error, MappedError>(
  res: Promise<rescript.result<Ok, Error>>,
  f: (arg0: Error) => MappedError,
): Promise<rescript.result<Ok, MappedError>>;

export function flatMapOkAsync<Ok, Error, MappedOk>(
  res: Promise<rescript.result<Ok, Error>>,
  f: (arg0: Ok) => Promise<rescript.result<MappedOk, Error>>,
): Promise<rescript.result<MappedOk, Error>>;

export function flatMapErrorAsync<Ok, Error, MappedError>(
  res: Promise<rescript.result<Ok, Error>>,
  f: (arg0: Error) => Promise<rescript.result<Ok, MappedError>>,
): Promise<rescript.result<Ok, MappedError>>;
