let normalizeByForm1 = "abcde"->Js.String2.normalizeByForm("a")
let normalizeByForm2 = Js.String2.normalizeByForm("abcde", "a")

let unsafeReplaceBy01 = "abcde"->Js.String2.unsafeReplaceBy0(/d/, (_, _, _) => "f")
let unsafeReplaceBy02 = Js.String2.unsafeReplaceBy0("abcde", /d/, (_, _, _) => "f")

let unsafeReplaceBy11 = "abcde"->Js.String2.unsafeReplaceBy1(/d/, (_, _, _, _) => "f")
let unsafeReplaceBy12 = Js.String2.unsafeReplaceBy1("abcde", /d/, (_, _, _, _) => "f")

let unsafeReplaceBy21 = "abcde"->Js.String2.unsafeReplaceBy2(/d/, (_, _, _, _, _) => "f")
let unsafeReplaceBy22 = Js.String2.unsafeReplaceBy2("abcde", /d/, (_, _, _, _, _) => "f")
