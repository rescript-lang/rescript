let count$ = signal(0)
let $count = count$ + 1
let record$ = {count$, $count}
let access = record$.count$ + record$.$count
