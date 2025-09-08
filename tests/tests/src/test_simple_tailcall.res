let rec length = (acc, x) =>
  switch x {
  | list{} => acc
  | list{_, ...tl} => length(acc + 1, tl)
  }
let rec tailcall = x => tailcall(x)

let rec non_length = x =>
  switch x {
  | list{} => 0
  | list{_, ...tl} => 1 + non_length(tl)
  }

let rec length = (acc, x) =>
  switch x {
  | list{} => acc
  | list{x, y, ...tl} => 1 + length(acc + 1, tl)
  | list{_, ...tl} => length(acc + 1, tl)
  }
