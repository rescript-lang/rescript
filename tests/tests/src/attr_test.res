let u = (x, y) => x + y

let h = u(1, 2)

type u = {"v": int, "y": int}
type xx<'a, 'b> = {.."case": int => int => 'a} as 'b
type xx_uncurry<'a, 'b> = {.."case": (int, int) => 'a} as 'b

type yy_uncurry = {"x": int}
type yy = {"x": int}
type number = float

let max2: (float, float) => float = (x, y) => x +. y

let hh = max2(1., 2.)

@val external des: (string, unit => unit) => unit = "des"

let f = x => des(x, () => Console.log("hei"))
