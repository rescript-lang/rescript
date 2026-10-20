@get external clipboardData: string => {..} = "clipboardData"

// A method of an open JS object type receives its arguments: no warning 20.
let text = clipboardData("event")["getData"]("text/plain")

// A function that never returns ignores its arguments: warning 20.
let ignored = () => failwith("unreachable")(1)

// A field read that cannot return ignores its arguments: warning 20.
let fromLiteral = () => ({"m": failwith("boom")})["m"](Console.log("a"))
let fromFailwith = () => failwith("boom")["m"](Console.log("b"))
