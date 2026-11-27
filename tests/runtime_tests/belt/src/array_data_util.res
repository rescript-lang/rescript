module A = Belt.Array

/* [] */
let range = (i, j) => A.makeBy(j - i + 1, k => k + i)

let randomRange = (i, j) => {
  let v = A.makeBy(j - i + 1, k => k + i)
  A.shuffleInPlace(v)
  v
}
