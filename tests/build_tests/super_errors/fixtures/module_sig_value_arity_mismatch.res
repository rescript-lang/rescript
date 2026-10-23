module M: {
  let f: (int, int) => int
} = {
  let f = (x: int) => (y: int) => x + y
}
