// A spread of an immutable record still generalizes.
type options<'a> = {tag: int, value: option<'a>}
@val external defaults: options<'a> = "defaults"
let options = {...defaults, tag: 1}
let ints: options<int> = options
let strings: options<string> = options

type cell<'a> = {tag: int, mutable value: option<'a>}
@val external base: cell<'a> = "base"
let copy = {...base, tag: 1}
copy.value = Some(1)
let s: option<string> = copy.value
