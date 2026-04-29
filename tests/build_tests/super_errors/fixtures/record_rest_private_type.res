module M: {
  type t = private {b: string}
} = {
  type t = {b: string}
}

type source = {a: int, b: string}

let {a, ...M.t as rest} = ({a: 1, b: "x"}: source)
