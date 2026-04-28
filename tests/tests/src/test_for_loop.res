let for_ = x =>
  for i in 0 to {
    Console.log("hi")
    ignore(3)
    Belt.Array.length(x)
  } {
    Console.log(x[i])
  }

let for_2 = x =>
  for i in 0 to Belt.Array.length(x) {
    Console.log(x[i])
  }

let for_3 = x => {
  let v = ref(0)
  let arr = x->Belt.Array.map(_ => _ => ())
  for i in 0 to Belt.Array.length(x) {
    let j = i * 2
    arr[i] = _ => v := v.contents + j
  }
  arr->Belt.Array.forEach(x => x())
  v.contents
}

let for_4 = x => {
  let v = ref(0)
  let arr = x->Belt.Array.map(_ => _ => ())
  for i in 0 to Belt.Array.length(x) {
    let j = i * 2
    let k = 2 * j
    arr[i] = _ => v := v.contents + k
  }
  arr->Belt.Array.forEach(x => x())
  v.contents
}

let for_5 = (x, u) => {
  let v = ref(0)
  let arr = x->Belt.Array.map(_ => _ => ())
  for i in 0 to Belt.Array.length(x) {
    let _j = i * 2
    let k = 2 * u * u
    arr[i] = _ => v := v.contents + k
  }
  arr->Belt.Array.forEach(x => x())
  v.contents
}

let for_6 = (x, u) => {
  let v = ref(0)
  let arr = x->Belt.Array.map(_ => _ => ())
  let v4 = ref(0)
  let v5 = ref(0)
  incr(v4)
  for j in 0 to 1 {
    incr(v5)
    let v2 = ref(0)
    let v3 = u
    for i in 0 to Belt.Array.length(x) {
      let _j = i * 2
      let k = 2 * u * u
      let h = 2 * v5.contents
      incr(v2)
      arr[i] = _ => v := v.contents + k + v2.contents + v3 + v4.contents + v5.contents + h
    }
  }
  arr->Belt.Array.forEach(x => x())
  v.contents
}
