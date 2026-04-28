// Test basic for...of syntax
let arr = [1, 2, 3]

for x of arr {
  ignore(x + 1)
}

// Test for...of with piped iterable expression
for item of arr->Array.map(x => x + 1) {
  ignore(item)
}
