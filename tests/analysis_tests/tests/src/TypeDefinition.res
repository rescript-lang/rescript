type variant = Foo | Bar

type record = {item: string}
//       ^typ

let x = Foo
//  ^typ

let y = {item: "foo"}
//  ^typ

type obj = {"foo": string}

let obj: obj = {"foo": "bar"}
//  ^typ

let f = r => r.item
//           ^typ

let g = v =>
  switch v {
  //     ^typ
  | Foo => "Foo"
  | Bar => "Bar"
  }
