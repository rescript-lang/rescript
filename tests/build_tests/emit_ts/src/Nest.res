type box<'a> = {v: option<'a>}

type opt<'a> = option<'a>

type nested = option<option<int>>

type callback = (~x: option<int>=?) => int

let keep = (o: option<option<int>>) => o

let keepAlias = (o: opt<'a>) => o

let keepNested = (o: nested) => o

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

let call = (f: callback) => f()
