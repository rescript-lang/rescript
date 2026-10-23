@genType type promise<'a> = Promise.t<'a>

@genType
type fromPayload = {
  x: int,
  s: string,
}

@genType type toPayload = {result: string}

@genType let convert = p => Promise.then(p, ({s}) => Promise.resolve({result: s}))

@genType let barx = (~x=Promise.resolve(Some("a")), ()) => x == x
