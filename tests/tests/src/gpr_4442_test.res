let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

let u = %raw(`function fib(n){
  if(n===0||n==1){
    return 1
  }
  return fib(n-1) + fib(n-2)
}`)

eq(__LOC__, u(2), 2)
eq(__LOC__, u(3), 3)

Mt.from_pair_suites(__FILE__, suites.contents)
