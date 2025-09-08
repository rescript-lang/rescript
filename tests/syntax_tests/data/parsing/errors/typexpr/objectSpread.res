type u = {...a, u: int}

type u = private {...a, u: int}

type x = Type({...a, u: int})

type u = {...a, "u": int, v: int}

let f = (x: {a: int, b: int}) => ()
