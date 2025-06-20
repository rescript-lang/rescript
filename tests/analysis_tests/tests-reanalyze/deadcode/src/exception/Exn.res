let raises = () => throw(Not_found)

let catches1 = try () catch {
| Not_found => ()
}

let catches2 = switch () {
| _ => ()
| exception Not_found => ()
}

let raiseAndCatch = try throw(Not_found) catch {
| _ => ()
}

@raises(Not_found)
let raisesWithAnnotaion = () => throw(Not_found)

let callsRaiseWithAnnotation = raisesWithAnnotaion()

@raises(A)
let callsRaiseWithAnnotationAndIsAnnotated = raisesWithAnnotaion()

let incompleteMatch = l =>
  switch l {
  | list{} => ()
  }

exception A
exception B

let twoRaises = (x, y) => {
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

@raises([A, B, C])
let raise2Annotate3 = (x, y) => {
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

let reRaise = () =>
  switch throw(A) {
  | exception A => throw(B)
  | _ => 11
  }

let switchWithCatchAll = switch throw(A) {
| exception _ => 1
| _ => 2
}

let raiseInInternalLet = b => {
  let a = b ? throw(A) : 22
  a + 34
}

let indirectCall = () => raisesWithAnnotaion()

let array = a => a[2]

let id = x => x

let tryChar = v => {
  try ignore(id(Char.chr(v))) catch {
  | _ => ()
  }
  42
}

@raises(Not_found)
let raiseAtAt = () => \"@@"(raise, Not_found)

@raises(Not_found)
let raisePipe = throw(Not_found)

@raises(Not_found)
let raiseArrow = Not_found->raise

@raises(JsExn)
let bar = () => Js.Json.parseExn("!!!")

let severalCases = cases =>
  switch cases {
  | "one" => failwith("one")
  | "two" => failwith("two")
  | "three" => failwith("three")
  | _ => ()
  }

@raises(genericException)
let genericRaiseIsNotSupported = exn => throw(exn)

@raises(Invalid_argument)
let redundantAnnotation = () => ()

let _x = throw(A)

let _ = throw(A)

let () = throw(A)

throw(Not_found)

// Examples with pipe

let onFunction = () => (@doesNotRaise Belt.Array.getExn)([], 0)

let onResult = () => @doesNotRaise Belt.Array.getExn([], 0)

let onFunctionPipe = () => []->(@doesNotRaise Belt.Array.getExn)(0)

let onResultPipeWrong = () => (@doesNotRaise [])->Belt.Array.getExn(0)
