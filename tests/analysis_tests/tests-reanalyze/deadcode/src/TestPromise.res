@genType
type promise<'a> = Promise.t<'a>

@genType
type fromPayload = {
  x: int,
  s: string,
}

@genType
type toPayload = {result: string}

@genType
let convert = promise => promise->Promise.then(({s}) => Promise.resolve({result: s}))
