type encoded
type coder<'a> = {
  encode: 'a => option<encoded>,
  decode: encoded => option<'a>,
}

@module("serialize-query-params") @val
external float: coder<float> = "NumberParam"
@module("serialize-query-params") @val
external int: coder<int> = "NumberParam"
@module("serialize-query-params") @val
external string: coder<string> = "StringParam"
@module("serialize-query-params") @val
external stringArray: coder<array<string>> = "ArrayParam"
@module("serialize-query-params") @val
external date: coder<Js.Date.t> = "DateParam"

type locationFragments = {
  protocol: string,
  host: string,
  pathname: string,
  search: string,
  state: Window.locationState,
}

@module("serialize-query-params")
external updateInLocation: (dict<option<encoded>>, Window.location) => locationFragments =
  "updateInLocation"

@module("serialize-query-params")
external parse: string => dict<encoded> = "parse"
