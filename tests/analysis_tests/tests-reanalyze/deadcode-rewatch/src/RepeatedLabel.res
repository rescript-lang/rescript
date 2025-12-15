type userData = {
  a: bool,
  b: int,
}

type tabState = {
  a: bool,
  b: int,
  f: string,
}

let userData = ({a, b}): userData => {a: a, b: b}

Js.log(userData)

