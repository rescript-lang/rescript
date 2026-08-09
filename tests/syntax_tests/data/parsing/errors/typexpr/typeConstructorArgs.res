type node<'a> = {
  _value: Nullable.t('a)
}

type t<'a> = Belt.Map.t('a)
type t<'a> = private Belt.Map.t('a)

type t = option<<node<int>>
type t = option(<node<int>>)
