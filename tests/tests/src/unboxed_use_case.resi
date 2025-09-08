type para<_>

@unboxed type rec t = H(para<_>): t

@unboxed type rec any = Any('a): any

let hi: array<any>

/* https://inbox.ocaml.org/caml-list/CAAxsn=HhhmAAYfSCLzWgMW0Q-duTZNQBLQYDx8yETwWTjm16tw@mail.gmail.com/t/#u */
type rec t3<'a, 'b> = [
  | #A(t3_aux<int, string>)
  | #B
]
@unboxed and t3_aux<'a, 'b> = {field: t3<'a, 'b>}

let v0: t3<'a, 'b>
let v1: t3<'a, 'b>
