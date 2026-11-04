type box<'a> = {v: option<'a>}

let keep = (o: option<option<int>>) => o

let skip = (o: option<unit>) =>
  switch o {
  | Some(_) => 1
  | None => 0
  }

let rewrap = (o: option<'a>) =>
  switch o {
  | Some(x) => Some(x)
  | None => None
  }

let boxed = (x: int) => {v: Some(x)}
