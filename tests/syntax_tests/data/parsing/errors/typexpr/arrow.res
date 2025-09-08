// notice usage of -> instead of =>
external add_nat: nat -> int = "add_nat_bytecode"

module Error2 = {
  type observation ={
    observed: int,
    onStep: (~currentValue   ) => unit
  }
}

module Error3 = {
  type observation ={
    observed: int,
    onStep: ~currentValue  => unit
  }
}
