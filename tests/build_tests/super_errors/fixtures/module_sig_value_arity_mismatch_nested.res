module M: {
  let f: int => (int, int) => int
} = {
  let f = (_x: int) => (y: int) => (_z: int) => y
}
