let x: bool

@inline("hello") let f: string

@inline(`a`) let f1: string

@inline(`中文`) let f2: string

module N: {
  @inline(`中文`) let f3: string
}

module N1: () =>
{
  @inline(`中文`) let f4: string
  @inline(3e-6) let xx: float
}

let h: string
let hh: string

@inline(true) let f5: bool

@inline(1) let f6: int
