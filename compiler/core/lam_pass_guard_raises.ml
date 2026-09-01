let rec guard_raises (lam : Lambda.lambda) : Lambda.lambda =
  match lam with
  | Lifthenelse (a, (Lprim {primitive = Praise} as b), c) -> (
    match c with
    (* A constant alternative is already as flat as it gets. *)
    | Lconst _ -> Lambda.shallow_map_sharing guard_raises lam
    | _ ->
      Lambda.seq
        (Lambda.if_ (guard_raises a) b Lambda.lambda_unit)
        (guard_raises c))
  | _ -> Lambda.shallow_map_sharing guard_raises lam
