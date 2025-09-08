let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~test_id, ~suites, loc, x, y)

@@warning("-a")

/* let [|a|] = [|1|] */

let x = 1

/* let () = 
  eq __LOC__ a 1 


let () = Mt.from_pair_suites __FILE__ !suites */
