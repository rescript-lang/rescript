let () = {
  let r = ref(0)
  for k in 1 to 10 {
    for i in 1 to 10 {
      let (x, y) = if mod(i, 2) == 0 {
        (1, i * 2)
      } else {
        (2, i * 3)
      }

      r := r.contents * x + y
    }
  }
}
