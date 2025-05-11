type element
type dom
@send @return({null_to_opt: null_to_opt})
external getElementById: (dom, string) => option<element> = "getElementById"

let test = dom => {
  let elem = dom->getElementById("haha")
  switch elem {
  | None => 1
  | Some(ui) =>
    Js.log(ui)
    2
  }
}

/* external getElementById2 : dom -> string -> element option = ""
 [@@return null_to_opt] */

@get_index external get_undefined: (array<int>, int) => option<int> = ""

let f_undefined = (xs, i) =>
  switch get_undefined(xs, i) {
  | None => assert(false)
  | Some(k) => k
  }

let f_escaped_not = (xs, i) => {
  let x = get_undefined(xs, i)
  Js.log("hei")
  switch x {
  | Some(k) => k
  | None => 1
  }
}

let f_escaped_1 = (xs, i) => {
  let x = get_undefined(xs, i)
  () =>
    switch x {
    | Some(k) => k
    | None => 1
    }
} /* still okay */

let f_escaped_2 = (xs, i) => Js.log(get_undefined(xs, i))

@get_index @return(null_to_opt) external get_null: (array<int>, int) => option<int> = ""

let f_null = (xs, i) =>
  switch get_null(xs, i) {
  | None => assert(false)
  | Some(k) => k
  }

@get_index @return(nullable) external get_null_undefined: (array<int>, int) => option<int> = ""

let f_null_undefined = (xs, i) =>
  switch get_null_undefined(xs, i) {
  | None => assert(false)
  | Some(k) => k
  }
