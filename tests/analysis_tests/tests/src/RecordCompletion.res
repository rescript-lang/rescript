type t = {n: array<string>}

let t = {n: []}

type t2 = {n2: t}

let t2 = {n2: t}

// t.n->m
//       ^com

// t2.n2.n->m
//           ^com

module R = {
  type t = {name: string}
}

let n = {R.name: ""}
// n.R.
//     ^com

// n.R. xx
//     ^com
