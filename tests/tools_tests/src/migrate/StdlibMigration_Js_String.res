// Migration tests for Js.String (old) -> String module

let make1 = 1->Js.String.make
let make2 = Js.String.make(1)

let fromCharCode1 = 65->Js.String.fromCharCode
let fromCharCode2 = Js.String.fromCharCode(65)

let fromCharCodeMany1 = [65, 66, 67]->Js.String.fromCharCodeMany
let fromCharCodeMany2 = Js.String.fromCharCodeMany([65, 66, 67])

let fromCodePoint1 = 65->Js.String.fromCodePoint
let fromCodePoint2 = Js.String.fromCodePoint(65)

let fromCodePointMany1 = [65, 66, 67]->Js.String.fromCodePointMany
let fromCodePointMany2 = Js.String.fromCodePointMany([65, 66, 67])

let length1 = "abcde"->Js.String.length
let length2 = Js.String.length("abcde")

let get1 = "abcde"->Js.String.get(2)
let get2 = Js.String.get("abcde", 2)

let normalize1 = "abcde"->Js.String.normalize
let normalize2 = Js.String.normalize("abcde")

let toLowerCase1 = "ABCDE"->Js.String.toLowerCase
let toLowerCase2 = Js.String.toLowerCase("ABCDE")

let toUpperCase1 = "abcde"->Js.String.toUpperCase
let toUpperCase2 = Js.String.toUpperCase("abcde")

let toLocaleLowerCase1 = "ABCDE"->Js.String.toLocaleLowerCase
let toLocaleLowerCase2 = Js.String.toLocaleLowerCase("ABCDE")

let toLocaleUpperCase1 = "abcde"->Js.String.toLocaleUpperCase
let toLocaleUpperCase2 = Js.String.toLocaleUpperCase("abcde")

let trim1 = "  abcde  "->Js.String.trim
let trim2 = Js.String.trim("  abcde  ")

// Type alias migration
let sT: Js.String.t = "abc"
