let make1 = 1->Js.String2.make
let make2 = Js.String2.make(1)

let fromCharCode1 = 65->Js.String2.fromCharCode
let fromCharCode2 = Js.String2.fromCharCode(65)

let fromCharCodeMany1 = [65, 66, 67]->Js.String2.fromCharCodeMany
let fromCharCodeMany2 = Js.String2.fromCharCodeMany([65, 66, 67])

let fromCodePoint1 = 65->Js.String2.fromCodePoint
let fromCodePoint2 = Js.String2.fromCodePoint(65)

let fromCodePointMany1 = [65, 66, 67]->Js.String2.fromCodePointMany
let fromCodePointMany2 = Js.String2.fromCodePointMany([65, 66, 67])

let length1 = "abcde"->Js.String2.length
let length2 = Js.String2.length("abcde")

let get1 = "abcde"->Js.String2.get(2)
let get2 = Js.String2.get("abcde", 2)

let charAt1 = "abcde"->Js.String2.charAt(2)
let charAt2 = Js.String2.charAt("abcde", 2)

let charCodeAt1 = "abcde"->Js.String2.charCodeAt(2)
let charCodeAt2 = Js.String2.charCodeAt("abcde", 2)

let codePointAt1 = "abcde"->Js.String2.codePointAt(2)
let codePointAt2 = Js.String2.codePointAt("abcde", 2)

let concat1 = "abcde"->Js.String2.concat("fghij")
let concat2 = Js.String2.concat("abcde", "fghij")

let concatMany1 = "abcde"->Js.String2.concatMany(["fghij", "klmno"])
let concatMany2 = Js.String2.concatMany("abcde", ["fghij", "klmno"])

let endsWith1 = "abcde"->Js.String2.endsWith("de")
let endsWith2 = Js.String2.endsWith("abcde", "de")

let endsWithFrom1 = "abcde"->Js.String2.endsWithFrom("d", 2)
let endsWithFrom2 = Js.String2.endsWithFrom("abcde", "d", 2)

let includes1 = "abcde"->Js.String2.includes("de")
let includes2 = Js.String2.includes("abcde", "de")

let includesFrom1 = "abcde"->Js.String2.includesFrom("d", 2)
let includesFrom2 = Js.String2.includesFrom("abcde", "d", 2)

let indexOf1 = "abcde"->Js.String2.indexOf("de")
let indexOf2 = Js.String2.indexOf("abcde", "de")

let indexOfFrom1 = "abcde"->Js.String2.indexOfFrom("d", 2)
let indexOfFrom2 = Js.String2.indexOfFrom("abcde", "d", 2)

let lastIndexOf1 = "abcde"->Js.String2.lastIndexOf("de")
let lastIndexOf2 = Js.String2.lastIndexOf("abcde", "de")

let lastIndexOfFrom1 = "abcde"->Js.String2.lastIndexOfFrom("d", 2)
let lastIndexOfFrom2 = Js.String2.lastIndexOfFrom("abcde", "d", 2)

let localeCompare1 = "abcde"->Js.String2.localeCompare("fghij")
let localeCompare2 = Js.String2.localeCompare("abcde", "fghij")

let match1 = "abcde"->Js.String2.match_(/d/)
let match2 = Js.String2.match_("abcde", /d/)

let normalize1 = "abcde"->Js.String2.normalize
let normalize2 = Js.String2.normalize("abcde")

let repeat1 = "abcde"->Js.String2.repeat(2)
let repeat2 = Js.String2.repeat("abcde", 2)

let replace1 = "abcde"->Js.String2.replace("d", "f")
let replace2 = Js.String2.replace("abcde", "d", "f")

let replaceByRe1 = "abcde"->Js.String2.replaceByRe(/d/, "f")
let replaceByRe2 = Js.String2.replaceByRe("abcde", /d/, "f")

let search1 = "abcde"->Js.String2.search(/d/)
let search2 = Js.String2.search("abcde", /d/)

let slice1 = "abcde"->Js.String2.slice(~from=1, ~to_=3)
let slice2 = Js.String2.slice("abcde", ~from=1, ~to_=3)

let sliceToEnd1 = "abcde"->Js.String2.sliceToEnd(~from=1)
let sliceToEnd2 = Js.String2.sliceToEnd("abcde", ~from=1)

let split1 = "abcde"->Js.String2.split("d")
let split2 = Js.String2.split("abcde", "d")

let splitAtMost1 = "abcde"->Js.String2.splitAtMost("d", ~limit=2)
let splitAtMost2 = Js.String2.splitAtMost("abcde", "d", ~limit=2)

let splitByRe1 = "abcde"->Js.String2.splitByRe(/d/)
let splitByRe2 = Js.String2.splitByRe("abcde", /d/)

let splitByReAtMost1 = "abcde"->Js.String2.splitByReAtMost(/d/, ~limit=2)
let splitByReAtMost2 = Js.String2.splitByReAtMost("abcde", /d/, ~limit=2)

let startsWith1 = "abcde"->Js.String2.startsWith("ab")
let startsWith2 = Js.String2.startsWith("abcde", "ab")

let startsWithFrom1 = "abcde"->Js.String2.startsWithFrom("b", 1)
let startsWithFrom2 = Js.String2.startsWithFrom("abcde", "b", 1)

let substring1 = "abcde"->Js.String2.substring(~from=1, ~to_=3)
let substring2 = Js.String2.substring("abcde", ~from=1, ~to_=3)

let substringToEnd1 = "abcde"->Js.String2.substringToEnd(~from=1)
let substringToEnd2 = Js.String2.substringToEnd("abcde", ~from=1)

let toLowerCase1 = "abcde"->Js.String2.toLowerCase
let toLowerCase2 = Js.String2.toLowerCase("abcde")

let toLocaleLowerCase1 = "abcde"->Js.String2.toLocaleLowerCase
let toLocaleLowerCase2 = Js.String2.toLocaleLowerCase("abcde")

let toUpperCase1 = "abcde"->Js.String2.toUpperCase
let toUpperCase2 = Js.String2.toUpperCase("abcde")

let toLocaleUpperCase1 = "abcde"->Js.String2.toLocaleUpperCase
let toLocaleUpperCase2 = Js.String2.toLocaleUpperCase("abcde")

let trim1 = "abcde"->Js.String2.trim
let trim2 = Js.String2.trim("abcde")
