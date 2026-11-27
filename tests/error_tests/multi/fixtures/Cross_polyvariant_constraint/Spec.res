type allowed = [#Red | #Green | #Blue]

let pick: allowed => string = c =>
  switch c {
  | #Red => "r"
  | #Green => "g"
  | #Blue => "b"
  }
