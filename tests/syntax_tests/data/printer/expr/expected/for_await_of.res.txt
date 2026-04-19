// Test basic for await...of syntax
let iterable = asyncIterable

for await x of iterable {
  Console.log(x)
}

let consume = async () => {
  for await item of iterable {
    let result = await Promise.resolve(item + 1)
    Console.log(result)
  }
}
