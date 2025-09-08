open Webapi.Dom
open NodeList

let items = document->Document.querySelectorAll(".item")

forEach((item, _) => Js.log(item), items)
