module Same = (Na: N, Nb: N): ((S with type number1 = Na.number) with type number2 = Nb.number) => {
  type number1 = Na.number
  type number2 = Nb.number
  let rec sim = ((n, m)) =>
    if Na.is_zero(n) {
      Nb.is_zero(m)
    } else {
      sim((Na.pred(n), Nb.pred(m)))
    }
  let similar = ((n, m)) =>
    try sim((n, m)) catch {
    | Na.Too_small => false
    | Nb.Too_small => false
    }
}
