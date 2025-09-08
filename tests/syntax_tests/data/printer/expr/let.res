let z = {
  let a = 1
  a
}

let x = {
  let x = 1 and y = 2 and z = 3

  let x = 1
  and y = 2
  and z = 3


  let x = 1

  and y = 2

  and z = 3

  x + y + z
}

let x = {let a = true; let b = false; a || b}

// don't add whitespace here
let highlight_dumb = (ppf, lb, loc) => {
  let line_start = ref(0)
  and line_end = ref(0)
  foo
}

// should contain a newline before foo
let highlight_dumb = (ppf, lb, loc) => {
  let line_start = ref(0)
  and line_end = ref(0)

  foo
}

let f = x => {
  let a =
    x
    ->Js.Dict.get("wm-received")
    ->Option.flatMap(Js.Json.decodeString)
    ->Option.map(Js.Date.fromString)

  let b =
    x
    ->Js.Dict.get("wm-property")
    ->Option.flatMap(Js.Json.decodeString)
    ->Option.flatMap(x =>
      switch x {
      | "like-of" => Some(#like)
      | "repost-of" => Some(#repost)
      | _ => None
      }
    )
  (a, b)
}
