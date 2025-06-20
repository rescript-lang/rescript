type t = option<string>

let break = (~indent) =>
  switch indent {
  | None => ""
  | Some(s) => "\n" ++ s
  }

let more = indent =>
  switch indent {
  | None => None
  | Some(s) => Some("  " ++ s)
  }

let heuristicFields = (~indent, fields) => {
  let threshold = 2
  fields->List.length > threshold && indent == None ? Some("") : indent
}

let heuristicVariants = (~indent, rendered) => {
  let threshold = 40
  let break = rendered->String.concat(" ")->String.length > threshold
  break && indent == None ? Some("  ") : indent
}
