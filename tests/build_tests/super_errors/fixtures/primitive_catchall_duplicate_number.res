@tag("kind")
type t =
  | @as(int) A({@as("kind") kind: int})
  | @as(float) B({@as("kind") kind: float})
