let value: Gadt.wrap<unit> = Gadt.Pack(1)

let Gadt.Pack(_x) = value
