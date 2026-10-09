open Mocha
open Test_utils

type orderStatus = Open | Paid | Cancelled
type order = {id: int, status: orderStatus, total: int}

let firstLargeOpenOrder = (orders: array<order>) => {
  let matchId = ref(None)

  for order of orders {
    switch order.status {
    | Cancelled | Paid => continue
    | Open =>
      if order.total >= 500 {
        matchId := Some(order.id)
        break
      }
    }
  }

  matchId.contents
}

// Test for...of with an unused named loop variable
let countItems = (arr: array<int>) => {
  let count = ref(0)

  for item of arr {
    count := count.contents + 1
  }

  count.contents
}

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

  test("unused named loop variable", () => {
    eq(__LOC__, countItems([1, 2, 3]), 3)
  })

  test("body bindings can shadow the loop variable", () => {
    let arr = [1, 2, 3]
    let sum = ref(0)

    for x of arr {
      let x = x + 10
      sum := sum.contents + x
    }

    eq(__LOC__, sum.contents, 36)
  })

  test("break and continue target the for..of loop from nested switches", () => {
    let arr = [1, 2, 3, 4]
    let seen = ref([])

    for x of arr {
      switch x {
      | 1 => continue
      | 3 => break
      | _ => seen := [...seen.contents, x]
      }
    }

    eq(__LOC__, seen.contents, [2])
  })

  test("blog-style order scan uses continue and break", () => {
    let orders = [
      {id: 101, status: Cancelled, total: 900},
      {id: 102, status: Paid, total: 700},
      {id: 103, status: Open, total: 120},
      {id: 104, status: Open, total: 650},
      {id: 105, status: Open, total: 900},
    ]

    eq(__LOC__, firstLargeOpenOrder(orders), Some(104))
  })
})

// Compilation tests

// Test for...of with async function
let processData = async (data: int) => {
  data + 10
}

let asyncProcess = async (arr: array<int>) => {
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
