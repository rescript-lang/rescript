open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("basic iteration", () => {
    let arr = [1, 2, 3, 4, 5]
    let sum = ref(0)

    for x of arr {
      sum := sum.contents + x
    }

    eq(__LOC__, sum.contents, 15)
  })

  test("single element", () => {
    let arr = [42]
    let result = ref(0)

    for x of arr {
      result := x
    }

    eq(__LOC__, result.contents, 42)
  })

  test("empty array", () => {
    let arr = []
    let sum = ref(0)

    for x of arr {
      ok(__LOC__, false)
    }
  })
})

// Compilation tests

// Test for...of with async function
let processData = async (data: int) => {
  data + 10
}

let asyncProcess = async arr => {
  let results = []
  for item of arr {
    let result = await processData(item)
    results->Array.push(result)
  }
  results
}

// Test for...of with wildcard pattern
let testWildcardPattern = () => {
  let arr = [1, 2, 3]
  let count = ref(0)

  for _ of arr {
    count := count.contents + 1
  }

  count.contents === 3
}
