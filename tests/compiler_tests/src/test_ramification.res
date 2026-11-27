let ff = x => {
  let a = switch x {
  | "0"
  | "1"
  | "2" => 3
  | "3" => 4
  | "4" => 6
  | "7" => 7
  | _ => 8
  }
  a + 3
}

let f2 = x =>
  switch x {
  | "0"
  | "1"
  | "2" => 3
  | "3" => 4
  | "4" => 6
  | "7" => 7
  | _ => 8
  } + 3

type u = A(int, int) | B(int)

let f = (x: u) => {
  let y = switch x {
  | A(_) => 3
  | B(_) => 4
  }
  y + 32
}

let f2 = (x: u) => {
  let v = ref(0)
  let y = switch x {
  | A(_) =>
    v := 1
    let z = 1
    let z = z + 32
    z + 3
  | B(_) =>
    v := 1
    let z = 1
    let z = z + 32
    z + 4
  }

  y + 32
}

let f3 = (x: u) => {
  let v = ref(0)
  let y = switch x {
  | A(_) =>
    v := 1
    3
  | B(_) =>
    v := 1
    4
  }

  y + 32
}
