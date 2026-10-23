open Webapi.Dom
open NodeList

let items = document->Document.querySelectorAll(".item")

forEach((item, _) => Console.log(item), items)
