// Test for..of AST mapping

let testForOf = () => {
  let arr = [1, 2, 3]

  // Basic for..of
  for x of arr {
    Console.log(x)
  }

  // For..of with complex expression
  for item of Array.map(arr, x => x + 1) {
    Console.log(item)
  }

  // Nested for..of loops
  for x of arr {
    for y of arr {
      Console.log((x, y))
    }
  }

  // Mixed for..of and regular for loops
  for x of arr {
    for i in 1 to 3 {
      Console.log((x, i))
    }
  }
}
