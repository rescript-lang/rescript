// An immutable record still generalizes.
type options<'a> = {value?: 'a}
let defaults: options<_> = {}
let ints: options<int> = defaults
let strings: options<string> = defaults

type cell<'a> = {tag?: int, mutable value?: 'a}
let cell: cell<_> = {}
cell.value = Some(1)
let s: option<string> = cell.value
