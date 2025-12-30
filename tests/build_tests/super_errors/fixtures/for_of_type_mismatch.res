// Test for...of with incompatible array element type
let numbers: array<int> = [1, 2, 3]
for x of numbers {
  let y: string = x
  Js.log(y)
}
