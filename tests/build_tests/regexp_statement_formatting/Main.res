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
