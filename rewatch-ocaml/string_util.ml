let contains value substring =
  let value_length = String.length value in
  let substring_length = String.length substring in
  let rec loop index =
    if index + substring_length > value_length then false
    else if String.sub value index substring_length = substring then true
    else loop (index + 1)
  in
  substring_length = 0 || loop 0
