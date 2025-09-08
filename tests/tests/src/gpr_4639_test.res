let x = url =>
  switch url {
  | list{} => #start
  | list{"login"} => #login
  | list{"start"} => #start
  | _ => #start
  }
