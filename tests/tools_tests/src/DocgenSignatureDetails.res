let labeledOptional: (~required: 'a, ~optional: array<'a>=?) => result<'a, string> = (
  ~required,
  ~optional=?,
) => {
  ignore(optional)
  Ok(required)
}

let takesCallback: ('a => string) => bool = _callback => true

let returnsTuple: int => (string, int) = value => ("value", value)

let returnsFunction: int => string => bool = _value => _text => true

let takesVariant: [#enabled | #count(int)] => unit = _variant => ()

let constant = 42
