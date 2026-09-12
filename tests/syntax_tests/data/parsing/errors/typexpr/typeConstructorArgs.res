type node<'a> = {
  _value: Nullable.t('a)
}

type t<'a> = Belt.Map.t('a)
type t<'a> = private Belt.Map.t('a)

type t = option<<node<int>>
type t = option(<node<int>>)

type t = option(int)
type t = pair<option(<node<int>>), string>
type t = pair(option(<node<int>>), string)
type t = pair(int, string)
type t = option(<int)
type t = option<<int>
let after = 1

type a = option(<int>)
type b = option(<string>)
let after = 1
