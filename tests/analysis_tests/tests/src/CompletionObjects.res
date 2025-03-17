let x = Some(true)

let _ff = {
  "one": switch x {
  | Some(true) => "hello"
  // |
  //   ^com
  | _ => ""
  },
}
