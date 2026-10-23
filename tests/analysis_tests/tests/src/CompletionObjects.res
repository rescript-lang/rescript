let x = Some(true)

let _ff = {
  "one": switch x {
  | Some(true) => "hello"
  // |
  //   ^com
  | _ => ""
  },
}

@val external settable: {@set "one": int, @set "two": int} = "settable"

// settable["o"] = 1
//            ^com
