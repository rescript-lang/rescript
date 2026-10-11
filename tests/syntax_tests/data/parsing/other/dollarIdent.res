let count$ = signal(0)
let $count = count$ + 1
let value$$ = {count$, $count}
let update = {...value$$, count$: $count}
let fn = (~count$, ~on$count) => count$ + on$count
let fromRecord = value$$.count$ + value$$.$count
let _$ = 1
let __$x = _$ + 1
let underscore = (~_$) => _$
