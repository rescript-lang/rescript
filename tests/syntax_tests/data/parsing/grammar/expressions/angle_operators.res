type nested<'a>= array<option<array<'a>>>
let value: nested<int>= [Some([1])]
let value: array<array<array<int>>>= [[[1]]]
let make = (): option<array<array<int>>>=> Some([[1]])
let optional = (~value: option<array<int>>=?) => value

let shifts = (a, b, c) => (a>>b, a>>>b, a<<b, a>=b, a<=b)
let precedence = (a, b, c) => a + b >> c + 1 >= a << b && a >>> b <= c
let call = f((a >> b, a >= b), (x): option<array<int>> => Some([x]))
let comment = a /* left */ >>> /* right */ b
let template = `prefix ${a >> 2 >= b} suffix`
let regex = /[<>]+>>=/g
let jsx = <div title={a >= b} data-value={a >> 1}>
  <span>{a << 1}</span>
  <>{`value ${a >>> 1}`}</>
</div>

let expressionAfterType = (value: array<int>) >= other
let diamond = "💎"; let located = a>>>b
