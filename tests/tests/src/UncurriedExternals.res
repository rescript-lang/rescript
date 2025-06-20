module StandardNotation = {
  external raise: exn => 'a = "%raise"
  let dd = () => throw(Not_found)

  @val external sum: (float, float) => float = "sum"
  let h = sum(1.0, 2.0)

  module M: {
    let sum: (float, float) => float
  } = {
    external sum: (float, float) => float = "sum"
  }
  let hh = M.sum(1.0, 2.0)

  @get_index external get: (array<string>, int) => option<'a> = ""
  let tg = arr => arr->get(0)

  @val external copy: (@as(json`{}`) _, string) => string = "Object.assign"
  let tc = copy("abc")

  external toException: exn => exn = "%identity"
  let te = toException(Not_found)

  @obj external ccreate: unit => string = ""
  let tcr = ccreate()

  type counter
  @set external setIncrementC: (counter, @this (counter, int) => unit) => unit = "increment"
  let tsiC = c => setIncrementC(c, @this (me, amount) => Js.log(me))
  @set external setIncrementU: (counter, @this (counter, int) => unit) => unit = "increment"
  let tsiU = c => setIncrementU(c, @this (me, amount) => Js.log(me))

  @module("react")
  external useState: (unit => 'state) => ('state, ('state => 'state) => unit) = "useState"
  let (get, set) = useState(() => 3)
}

let methodWithAsync = @this this => async arg => this + arg
