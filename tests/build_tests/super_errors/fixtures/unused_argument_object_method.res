@get external clipboardData: string => {..} = "clipboardData"

// A method of an open JS object type receives its arguments: no warning 20.
let text = clipboardData("event")["getData"]("text/plain")

// Arguments that read object fields do not change that.
let closed: {"x": string} = {"x": "x"}
let closedArg = clipboardData("e")["getData"](closed["x"])
let openArg = clipboardData("e")["getData"](clipboardData("o")["x"])
let closedLast = clipboardData("e")["m"]("a", closed["x"])
let closedFirst = clipboardData("e")["m"](closed["x"], "a")
let openLast = clipboardData("e")["m"]("a", clipboardData("o")["x"])
let openFirst = clipboardData("e")["m"](clipboardData("o")["x"], "a")
let nested = clipboardData("e")["m"](clipboardData("f")["n"]("y"))

// A function that never returns ignores its arguments: warning 20.
let ignored = () => failwith("unreachable")(1)

// A field read that cannot return ignores its arguments: warning 20.
let fromLiteral = () => ({"m": failwith("boom")})["m"](Console.log("a"))
let fromFailwith = () => failwith("boom")["m"](Console.log("b"))
