let foo = x =>
  switch x {
  | "\"" => "\""
  | _ => ""
  }

let s = "😀"

let bar = x =>
  switch x {
  | "\\" => "\\"
  | "😀" => "😀"
  | _ => ""
  }
