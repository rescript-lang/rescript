@tag("kind")
type t =
  | @as(string) A({@as("kind") kind: string})
  | @as(string) B({@as("kind") kind: string})
