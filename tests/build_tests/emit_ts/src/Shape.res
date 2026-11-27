type shape =
  | Circle(float)
  | Rect(float, float)
  | Empty

let area = (s: shape) =>
  switch s {
  | Circle(r) => r *. r *. 3.14
  | Rect(w, h) => w *. h
  | Empty => 0.0
  }

let describe = (s: shape) =>
  switch s {
  | Circle(_) => "circle"
  | Rect(_, _) => "rect"
  | Empty => "empty"
  }
