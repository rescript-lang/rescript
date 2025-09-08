@genType type promise<'a> = Js.Promise.t<'a>

@genType
type fromPayload = {
  x: int,
  s: string,
}

@genType type toPayload = {result: string}

@genType let convert = p => Js.Promise.then_(({s}) => Js.Promise.resolve({result: s}), p)

@genType let barx = (~x=Js.Promise.resolve(Some("a")), ()) => x == x
