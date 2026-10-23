type nested = pair<option(<node<int>>), string>

type fields = {
  [first: int, second: string],
  last: bool,
}

type parameters<{'a, 'b}, 'c> = ('a, 'b, 'c)

let {(first, second), last} = value

type missingCloser = {
  (field: int
}

type unexpectedCloser = {] field: int}

let after = 1
