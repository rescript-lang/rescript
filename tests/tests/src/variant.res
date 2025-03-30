type pair = (int, int)

type v = A1 | A2 | B(int) | C(int, int) | D(pair)

let a1 = A1
let a2 = A2

let b = B(34)

let c = C(4, 2)

let d = D(4, 2)

let foo = x =>
  switch x {
  | A1 => 1
  | A2 => 2
  | B(n) => n
  | C(n, m) => n + m
  | D(n, m) => n + m
  }

let fooA1 = x =>
  switch x {
  | A1 => 1
  | _ => 42
  }

let fooC = x =>
  switch x {
  | C(n, m) => n + m
  | _ => 42
  }

let switchNum = x =>
  switch x {
  | 0 => "0"
  | 1 => "1"
  | 2 => "2"
  | _ => "_"
  }

module Path = {
  type rec t =
    | Pident(string)
    | Pdot(t, string, int)
    | Papply(t, t)
  let same = \"=="
  let compare = compare
}

module Make = (
  M: {
    type t = Path.t
  },
) => {
  type t = M.t
  let find = (x: t) => ()
}

module M = Make(Path)

let rollback_path = (subst, p) => {
  let _ = M.find(p)
  try "try" catch {
  | Not_found =>
    switch p {
    | Pident(_) | Papply(_) => "Pident | Papply"
    | Pdot(_) => "Pdot"
    }
  }
}

exception EA1
exception EA2
exception EB(int)
exception EC(int, int)
exception ED(pair)

let fooExn = f =>
  try f() catch {
  | EA1 => 1
  | EA2 => 2
  | EB(n) => n
  | EC(n, m) => n + m
  | ED(n, m) => n + m
  }
