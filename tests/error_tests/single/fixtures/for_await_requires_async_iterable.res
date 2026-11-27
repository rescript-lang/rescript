// Test for await...of with a sync iterable (should fail type checking)
let numbers = [1, 2, 3]

let consume = async () => {
  for await x of numbers->Array.asIterable {
    Console.log(x)
  }
}
