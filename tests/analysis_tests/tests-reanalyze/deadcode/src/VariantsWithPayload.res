type payload = {
  x: int,
  y: option<string>,
}

type withPayload = [
  | #a
  | @genType.as("bRenamed") #b
  | @genType.as(true) #True
  | @genType.as(20) #Twenty
  | @genType.as(0.5) #Half
  | #c(payload)
]

@genType
let testWithPayload = (x: withPayload) => x

@genType
let printVariantWithPayload = (x: withPayload) =>
  switch x {
  | #a => Console.log("printVariantWithPayload: a")
  | #b => Console.log("printVariantWithPayload: b")
  | #True => Console.log("printVariantWithPayload: True")
  | #Twenty => Console.log("printVariantWithPayload: Twenty")
  | #Half => Console.log("printVariantWithPayload: Half")
  | #c(payload) => Console.log4("printVariantWithPayload x:", payload.x, "y:", payload.y)
  }

@genType
type manyPayloads = [
  | @genType.as("oneRenamed") #one(int)
  | @genType.as(2) #two(string, string)
  | #three(payload)
]

@genType
let testManyPayloads = (x: manyPayloads) => x

@genType
let printManyPayloads = (x: manyPayloads) =>
  switch x {
  | #one(n) => Console.log2("printManyPayloads one:", n)
  | #two(s1, s2) => Console.log3("printManyPayloads two:", s1, s2)
  | #three(payload) => Console.log4("printManyPayloads x:", payload.x, "y:", payload.y)
  }

@genType
type simpleVariant =
  | A
  | B
  | C

@genType
let testSimpleVariant = (x: simpleVariant) => x

@genType
type variantWithPayloads =
  | @genType.as("ARenamed") A
  | B(int)
  | C(int, int)
  | D((int, int))
  | E(int, string, int)

@genType
let testVariantWithPayloads = (x: variantWithPayloads) => x

@genType
let printVariantWithPayloads = x =>
  switch x {
  | A => Console.log2("printVariantWithPayloads", "A")
  | B(x) => Console.log2("printVariantWithPayloads", "B(" ++ (string_of_int(x) ++ ")"))
  | C(x, y) =>
    Console.log2(
      "printVariantWithPayloads",
      "C(" ++ (string_of_int(x) ++ (", " ++ (string_of_int(y) ++ ")"))),
    )
  | D((x, y)) =>
    Console.log2(
      "printVariantWithPayloads",
      "D((" ++ (string_of_int(x) ++ (", " ++ (string_of_int(y) ++ "))"))),
    )
  | E(x, s, y) =>
    Console.log2(
      "printVariantWithPayloads",
      "E(" ++ (string_of_int(x) ++ (", " ++ (s ++ (", " ++ (string_of_int(y) ++ ")"))))),
    )
  }

@genType
type variant1Int = R(int)

@genType
let testVariant1Int = (x: variant1Int) => x

@genType
type variant1Object = R(payload)

@genType
let testVariant1Object = (x: variant1Object) => x

