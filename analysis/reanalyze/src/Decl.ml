(** Declaration types for dead code analysis. *)

module Kind = struct
  type t =
    | Exception
    | RecordLabel
    | VariantCase
    | Value of {
        isToplevel: bool;
        mutable optionalArgs: OptionalArgs.t;
        sideEffects: bool;
      }

  let isType dk =
    match dk with
    | RecordLabel | VariantCase -> true
    | Exception | Value _ -> false

  let toString dk =
    match dk with
    | Exception -> "Exception"
    | RecordLabel -> "RecordLabel"
    | VariantCase -> "VariantCase"
    | Value _ -> "Value"
end

type posAdjustment = FirstVariant | OtherVariant | Nothing

type t = {
  declKind: Kind.t;
  moduleLoc: Location.t;
  posAdjustment: posAdjustment;
  path: DcePath.t;
  pos: Lexing.position;
  posEnd: Lexing.position;
  posStart: Lexing.position;
  mutable resolvedDead: bool option;
  mutable report: bool;
}

let isValue decl =
  match decl.declKind with
  | Value _ (* | Exception *) -> true
  | _ -> false

let compareUsingDependencies ~orderedFiles
    {
      declKind = kind1;
      path = _path1;
      pos =
        {pos_fname = fname1; pos_lnum = lnum1; pos_bol = bol1; pos_cnum = cnum1};
    }
    {
      declKind = kind2;
      path = _path2;
      pos =
        {pos_fname = fname2; pos_lnum = lnum2; pos_bol = bol2; pos_cnum = cnum2};
    } =
  let findPosition fn = Hashtbl.find orderedFiles fn [@@raises Not_found] in
  (* From the root of the file dependency DAG to the leaves.
       From the bottom of the file to the top. *)
  let position1, position2 =
    try (fname1 |> findPosition, fname2 |> findPosition)
    with Not_found -> (0, 0)
  in
  compare
    (position1, lnum2, bol2, cnum2, kind1)
    (position2, lnum1, bol1, cnum1, kind2)

let compareForReporting
    {
      declKind = kind1;
      pos =
        {pos_fname = fname1; pos_lnum = lnum1; pos_bol = bol1; pos_cnum = cnum1};
    }
    {
      declKind = kind2;
      pos =
        {pos_fname = fname2; pos_lnum = lnum2; pos_bol = bol2; pos_cnum = cnum2};
    } =
  compare (fname1, lnum1, bol1, cnum1, kind1) (fname2, lnum2, bol2, cnum2, kind2)
