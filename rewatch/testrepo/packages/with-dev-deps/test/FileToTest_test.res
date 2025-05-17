let res = FileToTest.add(1, 2)
let expected = 3

if res !== expected {
  failwith("Expected " ++ expected->Js.Int.toString ++ ", got " ++ res->Js.Int.toString)
}
