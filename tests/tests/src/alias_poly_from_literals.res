type abc = [#a | #b | #c]

let useABC = (x: abc) =>
  switch x {
  | #a => 1
  | #b => 2
  | #c => 3
  }

let fromString = s =>
  switch s {
  | ("a" | "b" | "c") as #...f => useABC(f)
  | _ => 0
  }

type nums = [#1 | #2]

let useNums = (x: nums) =>
  switch x {
  | #1 => 10
  | #2 => 20
  }

let fromInt = i =>
  switch i {
  | (1 | 2) as #...n => useNums(n)
  | _ => 0
  }

let useSingle = (x: [#hello]) => {
  `${(x :> string)} world!`
}

let fromString2 = s =>
  switch s {
  | "hello" as #...g => useSingle(g)
  | _ => "unknown"
  }
