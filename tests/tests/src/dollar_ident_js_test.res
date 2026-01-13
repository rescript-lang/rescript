let count$ = 1
let $count = count$ + 2

type t$ = {count$: int, $count: int}

let record$: t$ = {count$, $count}
let sum$ = record$.count$ + record$.$count

let update$ = {...record$, count$: 5}

let fn$ = (~count$, ~inc$) => count$ + inc$

let $ = count$ + $count
let $$ = value$ => {
  Console.log(value$)
  value$ + $count
}
let call$$ = $$($)
