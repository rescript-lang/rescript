let run = action =>
  switch action() {
  | Some(exception Not_found) => "nested exception pattern is invalid"
  | _ => "ok"
  }
