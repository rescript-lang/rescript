let for_ = x =>
  for i in 0 to {
    Console.log("hi")
    ignore(3)
    Array.length(x)
  } {
    Console.log(x[i])
  }

let for_2 = x =>
  for i in 0 to Array.length(x) {
    Console.log(x[i])
  }

let for_3 = x => {
  let v = ref(0)
  let arr = x->Array.map(_ => _ => ())
  for i in 0 to Array.length(x) {
    let j = i * 2
    arr[i] = _ => v := v.contents + j
  }
  arr->Array.forEach(x => x())
  v.contents
}

let for_4 = x => {
  let v = ref(0)
  let arr = x->Array.map(_ => _ => ())
  for i in 0 to Array.length(x) {
    let j = i * 2
    let k = 2 * j
    arr[i] = _ => v := v.contents + k
  }
  arr->Array.forEach(x => x())
  v.contents
}

let for_5 = (x, u) => {
  let v = ref(0)
  let arr = x->Array.map(_ => _ => ())
  for i in 0 to Array.length(x) {
    let _j = i * 2
    let k = 2 * u * u
    arr[i] = _ => v := v.contents + k
  }
  arr->Array.forEach(x => x())
  v.contents
}

let for_6 = (x, u) => {
  let v = ref(0)
  let arr = x->Array.map(_ => _ => ())
  let v4 = ref(0)
  let v5 = ref(0)
  v4.contents = v4.contents + 1
  for j in 0 to 1 {
    v5.contents = v5.contents + 1
    let v2 = ref(0)
    let v3 = u
    for i in 0 to Array.length(x) {
      let _j = i * 2
      let k = 2 * u * u
      let h = 2 * v5.contents
      v2.contents = v2.contents + 1
      arr[i] = _ => v := v.contents + k + v2.contents + v3 + v4.contents + v5.contents + h
    }
  }
  arr->Array.forEach(x => x())
  v.contents
}
