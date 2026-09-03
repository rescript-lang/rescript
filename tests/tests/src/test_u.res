let f = x => {
  let v = ref(x)
  let sum = ref(0)
  while v.contents > 0 {
    sum := sum.contents + v.contents
    v.contents = v.contents - 1
  }
  sum.contents
}
