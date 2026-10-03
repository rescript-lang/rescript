let afterLet = s => {
  let t = s
  (/a/)->RegExp.test(t)
}

let afterExpression = s => {
  Console.log(s);
  /a/->RegExp.test(s)
}

let intermediate = () => {
  Console.log("before")
  (/a/)->ignore
  Console.log("after")
}

let bare = () => {
  Console.log("before")
  (/a/g)
}

let dotAndWhitespace = s => {
  Console.log(s)
  (/./s)->RegExp.test(s)->ignore
  (/ a/)->RegExp.test(s)
}

let ternaryAndBinary = s => {
  let t = s
  (/a/)->RegExp.test(t) && true ? Some(t) : None
}

let placeholder = () => {
  Console.log("before")
  (/a/)->use(_, "a")
}

let objectAccess = () => {
  Console.log("before")
  (/a/g)["lastIndex"]
}

let comments = s => {
  let t = s // binding
  // before regexp
  (/* inside */ /a/ /* after regexp */)->RegExp.test(t) // after statement
}

let first = s => {
  /a/->RegExp.test(s)
}

let division = a => {
  let b = a
    /2
    / 3
  b
}

let floatDivision = a => {
  let b = a
    /.2.
    /. 3.
  b
}

let s = "a";
/a/->RegExp.test(s)->ignore

module Nested = {
  let s = "a"
  (/a/)->RegExp.test(s)->ignore
}
