let raises = () => throw(Not_found)

let catches1 = try () catch {
| Not_found => ()
}

let catches2 = switch () {
| _ => ()
| exception Not_found => ()
}

let throwAndCatch = try throw(Not_found) catch {
| _ => ()
}

@throws(Not_found)
let throwsWithAnnotation = () => throw(Not_found)

let callsThrowWithAnnotation = throwsWithAnnotation()

@throws(A)
let callsThrowWithAnnotationAndIsAnnotated = throwsWithAnnotation()

let incompleteMatch = l =>
  switch l {
  | list{} => ()
  }

exception A
exception B

let twoThrows = (x, y) => {
  if x {
    throw(A)
  }
  if y {
    throw(B)
  }
}

let sequencing = () => {
  throw(A)
  try throw(B) catch {
  | _ => ()
  }
}

let wrongCatch = () =>
  try throw(B) catch {
  | A => ()
  }

exception C
let wrongCatch2 = b =>
  switch b ? throw(B) : throw(C) {
  | exception A => ()
  | exception B => ()
  | list{} => ()
  }

@throws([A, B, C])
let throw2Annotate3 = (x, y) => {
  if x {
    throw(A)
  }
  if y {
    throw(B)
  }
}

exception Error(string, string, int)

let parse_json_from_file = s => {
  switch 34 {
  | exception Error(p1, p2, e) => throw(Error(p1, p2, e))
  | v => v
  }
}

let reThrow = () =>
  switch throw(A) {
  | exception A => throw(B)
  | _ => 11
  }

let switchWithCatchAll = switch throw(A) {
| exception _ => 1
| _ => 2
}

let throwInInternalLet = b => {
  let a = b ? throw(A) : 22
  a + 34
}

let indirectCall = () => throwsWithAnnotation()

let array = a => a[2]

let id = x => x

let tryChar = v => {
  try ignore(id(Char.chr(v))) catch {
  | _ => ()
  }
  42
}

@throws(Not_found)
let throwAtAt = () => \"@@"(throw, Not_found)

@throws(Not_found)
let throwPipe = throw(Not_found)

@throws(Not_found)
let throwArrow = Not_found->throw

@throws(JsExn)
let bar = () => Js.Json.parseExn("!!!")

let severalCases = cases =>
  switch cases {
  | "one" => failwith("one")
  | "two" => failwith("two")
  | "three" => failwith("three")
  | _ => ()
  }

@throws(genericException)
let genericThrowIsNotSupported = exn => throw(exn)

@throws(Invalid_argument)
let redundantAnnotation = () => ()

let _x = throw(A)

let _ = throw(A)

let () = throw(A)

throw(Not_found)

// Examples with pipe

let onFunction = () => (@doesNotThrow Belt.Array.getExn)([], 0)

let onResult = () => @doesNotThrow Belt.Array.getExn([], 0)

let onFunctionPipe = () => []->(@doesNotThrow Belt.Array.getExn)(0)

let onResultPipeWrong = () => (@doesNotThrow [])->Belt.Array.getExn(0)
