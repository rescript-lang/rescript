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

let decodePayload = data =>
  switch JSON.parseOrThrow(data) {
  | JSON.Object(dict{
      "operationName": JSON.String(_),
      "query": JSON.String(_),
      "documentId": JSON.String(_),
      "variables": JSON.Object(_),
    }) => 4
  | JSON.Object(dict{
      "operationName": JSON.String(_),
      "query": JSON.String(_),
      "documentId": JSON.String(_),
    }) => 3
  | JSON.Object(dict{
      "operationName": JSON.String(_),
      "query": JSON.String(_),
      "variables": JSON.Object(_),
    }) => 3
  | JSON.Object(dict{
      "operationName": JSON.String(_),
      "documentId": JSON.String(_),
      "variables": JSON.Object(_),
    }) => 3
  | JSON.Object(dict{
      "query": JSON.String(_),
      "documentId": JSON.String(_),
      "variables": JSON.Object(_),
    }) => 3
  | JSON.Object(dict{"operationName": JSON.String(_), "query": JSON.String(_)}) => 2
  | JSON.Object(dict{"operationName": JSON.String(_)}) => 1
  | _ => 0
  }
