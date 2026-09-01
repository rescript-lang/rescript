let rec guard_raises (lam : Lam.t) : Lam.t =
  match lam with
  | Lifthenelse (a, (Lprim {primitive = Praise} as b), c) -> (
    match c with
    (* A constant alternative is already as flat as it gets. *)
    | Lconst _ -> Lam.shallow_map_sharing guard_raises lam
    | _ -> Lam.seq (Lam.if_ (guard_raises a) b Lam.unit) (guard_raises c))
  | _ -> Lam.shallow_map_sharing guard_raises lam
