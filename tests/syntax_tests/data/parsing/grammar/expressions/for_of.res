// Test basic for...of syntax
let arr = [1, 2, 3]

for x of arr {
  ignore(x + 1)
}

// Test for...of with different pattern
for (item, index) of arr {
  ignore((item, index))
}