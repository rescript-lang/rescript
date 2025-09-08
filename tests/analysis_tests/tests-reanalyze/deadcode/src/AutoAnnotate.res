type variant = R(int)

@genType
type record = {variant: variant}

type r2 = {r2: int}

type r3 = {r3: int}

type r4 = {r4: int}

@genType
type annotatedVariant =
  | R2(r2, r3)
  | R4(r4)

