while true {
  if done {
    break
  }

  if skip {
    continue
  }

  switch state {
  | Skip => continue
  | Stop => break
  | KeepGoing => work()
  }
}

for i in 0 to 10 {
  switch i {
  | 3 => continue
  | 8 => break
  | _ => work()
  }
}

for i of [0, 1, 2, 3, 4, 5] {
  switch i {
  | 1 => continue
  | 4 => break
  | _ => work()
  }
}
