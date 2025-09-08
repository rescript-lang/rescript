type proto = {
  syntax: option<string>,
  imports: int,
  file_options: int,
  package: int,
  messages: int,
  enums: int,
  extends: int,
}

let v = {
  syntax: None,
  imports: 0,
  file_options: 0,
  package: 0,
  messages: 0,
  enums: 0,
  extends: 0,
}

let uv = {
  syntax: None,
  imports: 1,
  file_options: 0,
  package: 0,
  messages: 0,
  enums: 0,
  extends: 0,
}
let u_v = {...v, imports: 0}

let f = (g, h) => {...g(h), imports: 0}

let suites = {
  open Mt
  list{("eq_with", _ => Eq(v, u_v))}
}

Mt.from_pair_suites(__MODULE__, suites)
