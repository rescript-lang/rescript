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

// A record owning the marker key at run time: Some(Some(r)) is the marker of
// depth r.depth + 1, so its options print a marker of any depth. No
// declaration of Primitive_option.some states such a payload, so wrapping
// one fails tsc.
type keyed = {
  @as("BS_PRIVATE_NESTED_SOME_NONE") depth: int,
  tag: string,
}

let keepKeyed = (x: option<option<keyed>>) => x
