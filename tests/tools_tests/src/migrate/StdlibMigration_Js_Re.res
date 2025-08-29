let re1 = Js.Re.fromString("foo")
let re2 = Js.Re.fromStringWithFlags("foo", ~flags="gi")

let flags1 = re2->Js.Re.flags
let flags2 = Js.Re.flags(re2)

let g1 = re2->Js.Re.global
let g2 = Js.Re.global(re2)

let ic1 = re2->Js.Re.ignoreCase
let ic2 = Js.Re.ignoreCase(re2)

let m1 = re2->Js.Re.multiline
let m2 = Js.Re.multiline(re2)

let u1 = re2->Js.Re.unicode
let u2 = Js.Re.unicode(re2)

let y1 = re2->Js.Re.sticky
let y2 = Js.Re.sticky(re2)

let src1 = re2->Js.Re.source
let src2 = Js.Re.source(re2)

let li1 = re2->Js.Re.lastIndex
let () = re2->Js.Re.setLastIndex(0)

let exec1 = re2->Js.Re.exec_("Foo bar")
let exec2 = Js.Re.exec_(re2, "Foo bar")

let test1 = re2->Js.Re.test_("Foo bar")
let test2 = Js.Re.test_(re2, "Foo bar")

let result_index = switch re2->Js.Re.exec_("Foo bar") {
| None => 0
| Some(r) => Js.Re.index(r)
}

let result_input = switch re2->Js.Re.exec_("Foo bar") {
| None => ""
| Some(r) => Js.Re.input(r)
}
