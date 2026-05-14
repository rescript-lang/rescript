let breakInWhile = () => {
  while true {
    break
    Console.log("after break")
  }
}

let continueInFor = () => {
  for i in 0 to 10 {
    continue
    Console.log(i)
  }
}

let terminalBreakInWhile = () => {
  while true {
    break
  }
}

let terminalContinueInFor = () => {
  for i in 0 to 10 {
    if i > 5 {
      continue
    }
  }
}
