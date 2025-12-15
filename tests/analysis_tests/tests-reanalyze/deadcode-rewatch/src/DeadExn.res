exception Etoplevel

module Inside = {
  exception Einside
}

exception DeadE
let eToplevel = Etoplevel

let eInside = Inside.Einside

Js.log(eInside)

