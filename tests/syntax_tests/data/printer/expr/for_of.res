// Test basic for...of syntax
let arr = [1, 2, 3]

for x of arr {
  Js.log(x)
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