include Belt.Array

include {
  assert(1 + 2 == 3)
  let a = 3
}

module N = {
  /* ;; prerr_endline "hello " */
  assert(1 + 2 == 3)
  let a = 3
  let v = ref(32)
  v := 0
}

module NN = {
  let a = 3
  let v = ref(32)
}

include N
