type x = {x: int}

type y = {
  y: int,
  ...x,
}

let getY = (v: y) => v.y
let getX = (v: y) => v.x

let v: y = {y: 3, x: 3}

type f<'a> = {
  a: string,
  b: 'a,
  c: option<'a>,
  d: option<result<'a, 'a>>,
}

type d<'a> = {
  ...f<'a>,
}

let d: d<int> = {
  a: "",
  b: 1,
  c: None,
  d: Some(Ok(1)),
}

type rn<'aaa> = {c: option<'aaa>}

type withRenamedTypeVariable<'bbb> = {
  ...rn<'bbb>,
}

let x: withRenamedTypeVariable<bool> = {
  c: Some(true),
}

type rnAsString = {
  ...rn<string>,
}

let x: rnAsString = {
  c: Some("hello"),
}

module DeepSub = {
  type t<'a, 'b> = {
    x: result<'a, 'b>,
    z: [#One | #Two('a) | #Three('b)],
  }
  type d = {
    ...t<int, int>,
  }
  let d: d = {
    x: Ok(1),
    z: #Two(1),
  }
}
