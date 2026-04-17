let whileLoop = () => {
  let i = ref(0)
  let acc = ref(0)

  while i.contents < 6 {
    i := i.contents + 1

    if i.contents == 2 {
      continue
    }

    if i.contents == 5 {
      break
    }

    acc := acc.contents + i.contents
  }

  acc.contents
}

let whileSwitchLoop = () => {
  let i = ref(0)
  let acc = ref(0)

  while i.contents < 6 {
    i := i.contents + 1

    let state = switch i.contents {
    | 2 => "skip"
    | 5 => "stop"
    | _ => "keep"
    }

    switch state {
    | "skip" => continue
    | "stop" => break
    | _ => acc := acc.contents + i.contents
    }
  }

  acc.contents
}

let forLoop = () => {
  let acc = ref(0)

  for i in 0 to 5 {
    if i == 1 {
      continue
    }

    if i == 4 {
      break
    }

    acc := acc.contents + i
  }

  acc.contents
}

let forSwitchLoop = () => {
  let acc = ref(0)

  for i in 0 to 5 {
    let state = switch i {
    | 1 => "skip"
    | 4 => "stop"
    | _ => "keep"
    }

    switch state {
    | "skip" => continue
    | "stop" => break
    | _ => acc := acc.contents + i
    }
  }

  acc.contents
}
