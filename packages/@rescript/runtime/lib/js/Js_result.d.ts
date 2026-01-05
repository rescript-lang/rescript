export type t<Good, Bad> =
  | {
    readonly TAG: "Ok";
    readonly _0: Good;
  }
  | {
    readonly TAG: "Error";
    readonly _0: Bad;
  };
