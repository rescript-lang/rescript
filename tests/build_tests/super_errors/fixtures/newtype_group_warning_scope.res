let suppressed = (@warning("-26") type a, x: a) => {
  let unused = 1
  x
}
let unsuppressed = (type a, x: a) => {
  let unused = 1
  x
}
