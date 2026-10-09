let afterLet = s => {
  let t = s
  (/a/->RegExp.test(t))
}

let afterExpression = s => {
  ignore(s)
  (/a/->RegExp.test(s))
}

let intermediate = () => {
  let seen = ref(false)
  (/a/->RegExp.test("a")->(result => seen := result))
  seen.contents
}

let bare = () => {
  ignore()
  (/a/)
}

let dotAndWhitespace = s => {
  ignore(s)
  (/./s->RegExp.test(s)->ignore)
  (/ a/->RegExp.test(s))
}

let ternaryAndBinary = s => {
  let t = s
  (/a/->RegExp.test(t) && true ? 1 : 0)
}

module Array = {
  @get_index external get: (RegExp.t, string) => int = ""
  @set_index external set: (RegExp.t, string, int) => unit = ""
}

let arrayAccess = key => {
  ignore(key)
  (/a/g[key])
}

let arrayMutation = (key, value) => {
  let written = ref(0)
  module Array = {
    let set = (regexp, key, value) => {
      regexp[key] = value
      written := regexp[key]
    }
  }
  ignore(key)
  (/a/g[key] = value)
  written.contents
}

type field = {mutable value: int}
type predicate = {test: unit => bool}

let fieldAccess = key => {
  module Array = {
    let get = (regexp, key) => {value: regexp->RegExp.test(key) ? 1 : 2}
  }
  ignore(key)
  (/a/[key].value)
}

let fieldMutation = (key, value) => {
  let field = {value: 0}
  module Array = {
    let get = (regexp, key) => regexp->RegExp.test(key) ? field : {value: -1}
  }
  ignore(key)
  (/a/[key].value = value)
  field.value
}

let fieldCall = key => {
  module Array = {
    let get = (regexp, key) => {test: () => regexp->RegExp.test(key)}
  }
  ignore(key)
  (/a/[key].test())
}

let division = a => {
  let b = a / 2 / 3
  b
}

let floatDivision = a => {
  let b = a /. 2. /. 3.
  b
}

let s = "a"
(/a/->RegExp.test(s)->ignore)
