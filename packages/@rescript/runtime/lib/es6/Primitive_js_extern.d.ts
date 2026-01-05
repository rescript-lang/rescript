export type null_<A> =
  | A
  | null;

export type undefined_<A> =
  | A
  | undefined;

export type nullable<A> =
  | A
  | null
  | undefined;

export type null_undefined<A> = nullable<A>;
