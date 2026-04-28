// Test basic for...of syntax
let arr = [1, 2, 3]

for x of arr {
  Console.log(x)
}

// Test for...of with async function
let processData = (data: int): promise<int> => {
  Promise.resolve(data + 10)
}

let asyncProcess = async () => {
  let results = []
  for item of arr {
    let result = await processData(item)
    results->Array.push(result)
  }
  results
}

// Test for...of with piped iterable expression
for item of arr->Array.map(x => x + 1) {
  Console.log(item)
}
