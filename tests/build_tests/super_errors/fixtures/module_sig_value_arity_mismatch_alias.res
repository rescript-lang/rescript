type curried = int => int => int

module M: {
  let f: (int, int) => int
} = {
  let f: curried = (x: int) => (_y: int) => x
}
