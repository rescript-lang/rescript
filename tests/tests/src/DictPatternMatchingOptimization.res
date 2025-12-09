// Test for dict pattern matching compilation performance
// This used to cause exponential blowup in exhaustiveness checking (issue #8042)

type inbound = A | B | C | D | E | F

let decode = (~data: string): array<inbound> =>
  switch JSON.parseOrThrow(data) {
  | JSON.Object(dict{"type": JSON.String("a")}) => [A]
  | JSON.Object(dict{"type": JSON.String("b")}) => [B]
  | JSON.Object(dict{"type": JSON.String("c")}) => [C]
  | JSON.Object(dict{"type": JSON.String("d")}) => [D]
  | JSON.Object(dict{"type": JSON.String("e")}) => [E]
  | JSON.Object(dict{"type": JSON.String("f")}) => [F]
  | _ => []
  }
