open Webapi.Dom
open DomTokenList

let tlist = document->Document.createElement("div")->Element.classList

let _ = length(tlist)
let _ = item(3, tlist)

add("my-class", tlist)
addMany(["my-class", "my-other-class"], tlist)
let _ = contains("my-class", tlist)
forEach((item, _) => print_endline(item), tlist)
remove("my-class", tlist)
removeMany(["my-class", "my-other-class"], tlist)
replace("my-class", "my-other-class", tlist)
let _ = supports("my-class", tlist)
let _ = toggle("my-class", tlist)
let _ = toggleForced("my-class", tlist)
let _ = toString(tlist)
let _ = value(tlist)
let _ = setValue(tlist, "foo")
