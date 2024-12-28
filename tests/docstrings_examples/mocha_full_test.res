open Mocha
@@warning("-32-34-60-37-109-3-44")

describe("AsyncIterator.forEach", () => {
  test("AsyncIterator.forEach", () => {
    module Test = {
      // Let's pretend we get an async iterator returning ints from somewhere.
      let asyncIterator: AsyncIterator.t<(string, string)> = %raw(`
  (() => {
    var map1 = new Map();

    map1.set('first', '1');
    map1.set('second', '2');

    var iterator1 = map1[Symbol.iterator]();
    return iterator1;
  })()
`)

      let main = async () =>
        await asyncIterator->AsyncIterator.forEach(
          v => {
            switch v {
            | Some(("second", value)) => assertEqual(value, "2")
            | _ => ()
            }
          },
        )

      main()->ignore
    }
    ()
  })
})

describe("AsyncIterator.next", () => {
  test("AsyncIterator.next", () => {
    module Test = {
      let asyncIterator: AsyncIterator.t<(string, string)> = %raw(`
  (() => {
    var map1 = new Map();

    map1.set('first', '1');
    map1.set('second', '2');

    var iterator1 = map1[Symbol.iterator]();
    return iterator1;
  })()
`)

      let processMyAsyncIterator = async () => {
        // ReScript doesn't have `for ... of` loops, but it's easy to mimic using a while loop.
        let break = ref(false)

        while !break.contents {
          // Await the next iterator value
          let {value, done} = await asyncIterator->AsyncIterator.next

          // Exit the while loop if the iterator says it's done
          break := done

          if done {
            value
            ->Option.isNone
            ->assertEqual(true)
          }
        }
      }

      processMyAsyncIterator()->ignore
    }
    ()
  })
})

describe("AsyncIterator.done", () => {
  test("AsyncIterator.done", () => {
    module Test = {
      let context = ref(0)

      let asyncIterator = AsyncIterator.make(
        async () => {
          let currentValue = context.contents
          // Increment current value
          context := currentValue + 1

          if currentValue >= 3 {
            AsyncIterator.done()
          } else {
            AsyncIterator.value(currentValue)
          }
        },
      )
    }
    ()
  })
})

describe("AsyncIterator.value", () => {
  test("AsyncIterator.value", () => {
    module Test = {
      let context = ref(0)

      let asyncIterator = AsyncIterator.make(
        async () => {
          let currentValue = context.contents
          // Increment current value
          context := currentValue + 1

          if currentValue >= 3 {
            AsyncIterator.done()
          } else {
            AsyncIterator.value(currentValue)
          }
        },
      )
    }
    ()
  })
})

describe("AsyncIterator.make", () => {
  test("AsyncIterator.make", () => {
    module Test = {
      let context = ref(0)

      let asyncIterator = AsyncIterator.make(
        async () => {
          let currentValue = context.contents
          // Increment current value
          context := currentValue + 1

          {
            AsyncIterator.value: Some(currentValue),
            done: currentValue >= 3,
          }
        },
      )

      // This will log 1, 2, 3
      let main = async () =>
        await asyncIterator->AsyncIterator.forEach(
          value =>
            switch value {
            | Some(value) => Console.log(value)
            | None => ()
            },
        )

      main()->ignore
    }
    ()
  })
})

describe("Array.last", () => {
  test("Array.last", () => {
    module Test = {
      ["Hello", "Hi", "Good bye"]
      ->Array.last
      ->assertEqual(Some("Good bye"))

      []
      ->Array.last
      ->assertEqual(None)
    }
    ()
  })
})

describe("Array.at", () => {
  test("Array.at", () => {
    module Test = {
      ["a", "b", "c"]->Array.at(0)->assertEqual(Some("a"))
      ["a", "b", "c"]->Array.at(2)->assertEqual(Some("c"))
      ["a", "b", "c"]->Array.at(3)->assertEqual(None)
      ["a", "b", "c"]->Array.at(-1)->assertEqual(Some("c"))
      ["a", "b", "c"]->Array.at(-3)->assertEqual(Some("a"))
      ["a", "b", "c"]->Array.at(-4)->assertEqual(None)
    }
    ()
  })
})

describe("Array.findMap", () => {
  test("Array.findMap", () => {
    module Test = {
      Array.findMap([1, 2, 3], n => mod(n, 2) == 0 ? Some(n - 2) : None)->assertEqual(Some(0))

      Array.findMap([1, 2, 3, 4, 5, 6], n => mod(n, 2) == 0 ? Some(n - 8) : None)->assertEqual(
        Some(-6),
      )

      Array.findMap([1, 2, 3, 4, 5, 6], _ => None)->assertEqual(None)

      Array.findMap([], n => mod(n, 2) == 0 ? Some(n * n) : None)->assertEqual(None)
    }
    ()
  })
})

describe("Array.flatMapWithIndex", () => {
  test("Array.flatMapWithIndex", () => {
    module Test = {
      type language = ReScript | TypeScript | JavaScript

      let array = [ReScript, TypeScript, JavaScript]

      array
      ->Array.flatMapWithIndex(
        (item, index) =>
          switch item {
          | ReScript => [index]
          | TypeScript => [index, index + 1]
          | JavaScript => [index, index + 1, index + 2]
          },
      )
      ->assertEqual([0, 1, 2, 2, 3, 4])
    }
    ()
  })
})

describe("Array.flatMap", () => {
  test("Array.flatMap", () => {
    module Test = {
      type language = ReScript | TypeScript | JavaScript

      let array = [ReScript, TypeScript, JavaScript]

      array
      ->Array.flatMap(
        item =>
          switch item {
          | ReScript => [1, 2, 3]
          | TypeScript => [4, 5, 6]
          | JavaScript => [7, 8, 9]
          },
      )
      ->assertEqual([1, 2, 3, 4, 5, 6, 7, 8, 9])
    }
    ()
  })
})

describe("Array.shuffle", () => {
  test("Array.shuffle", () => {
    module Test = {
      let array = ["Hello", "Hi", "Good bye"]
      array->Array.shuffle
      Console.log(array)

      let array2 = [1, 2, 3]
      array2->Array.shuffle

      array2
      ->Array.length
      ->assertEqual(3)
    }
    ()
  })
})

describe("Array.toShuffled", () => {
  test("Array.toShuffled", () => {
    module Test = {
      let array = ["Hello", "Hi", "Good bye"]
      let shuffledArray = array->Array.toShuffled
      Console.log(shuffledArray)

      Array.toShuffled([1, 2, 3])
      ->Array.length
      ->assertEqual(3)
    }
    ()
  })
})

describe("Array.keepSome", () => {
  test("Array.keepSome", () => {
    module Test = {
      Array.keepSome([Some(1), None, Some(3)])->assertEqual([1, 3])

      Array.keepSome([Some(1), Some(2), Some(3)])->assertEqual([1, 2, 3])

      Array.keepSome([None, None, None])->assertEqual([])

      Array.keepSome([])->assertEqual([])
    }
    ()
  })
})

describe("Array.filterMap", () => {
  test("Array.filterMap", () => {
    module Test = {
      ["Hello", "Hi", "Good bye"]
      ->Array.filterMap(
        item =>
          switch item {
          | "Hello" => Some(item->String.length)
          | _ => None
          },
      )
      ->assertEqual([5])

      [1, 2, 3, 4, 5, 6]
      ->Array.filterMap(n => mod(n, 2) == 0 ? Some(n * n) : None)
      ->assertEqual([4, 16, 36])

      Array.filterMap([1, 2, 3, 4, 5, 6], _ => None)->assertEqual([])

      Array.filterMap([], n => mod(n, 2) == 0 ? Some(n * n) : None)->assertEqual([])
    }
    ()
  })
})

describe("Array.findIndexOpt", () => {
  test("Array.findIndexOpt", () => {
    module Test = {
      type languages = ReScript | TypeScript | JavaScript

      let array = [ReScript, TypeScript, JavaScript]

      array
      ->Array.findIndexOpt(item => item == ReScript)
      ->assertEqual(Some(0))
    }
    ()
  })
})

describe("Array.setUnsafe", () => {
  test("Array.setUnsafe", () => {
    module Test = {
      let array = ["Hello", "Hi", "Good bye"]
      array->Array.setUnsafe(1, "Hello")

      assertEqual(array[1], Some("Hello"))
    }
    ()
  })
})

describe("Array.unsafe_get", () => {
  test("Array.unsafe_get", () => {
    module Test = {
      let array = [1, 2, 3]
      for index in 0 to array->Array.length - 1 {
        let value = array->Array.unsafe_get(index)
        Console.log(value)
      }
    }
    ()
  })
})

describe("Array.getUnsafe", () => {
  test("Array.getUnsafe", () => {
    module Test = {
      let array = [1, 2, 3]
      for index in 0 to array->Array.length - 1 {
        let value = array->Array.getUnsafe(index)
        Console.log(value)
      }
    }
    ()
  })
})

describe("Array.set", () => {
  test("Array.set", () => {
    module Test = {
      let array = ["Hello", "Hi", "Good bye"]
      array->Array.set(1, "Hello")

      array[1]->assertEqual(Some("Hello"))
    }
    ()
  })
})

describe("Array.get", () => {
  test("Array.get", () => {
    module Test = {
      let array = ["Hello", "Hi", "Good bye"]

      array
      ->Array.get(0)
      ->assertEqual(Some("Hello"))

      array
      ->Array.get(3)
      ->assertEqual(None)
    }
    ()
  })
})

describe("Array.someWithIndex", () => {
  test("Array.someWithIndex", () => {
    module Test = {
      let array = ["Hello", "Hi", "Good bye"]

      array
      ->Array.someWithIndex((greeting, index) => greeting === "Hello" && index === 0)
      ->assertEqual(true)
    }
    ()
  })
})

describe("Array.some", () => {
  test("Array.some", () => {
    module Test = {
      let array = ["Hello", "Hi", "Good bye"]

      array
      ->Array.some(greeting => greeting === "Hello")
      ->assertEqual(true)
    }
    ()
  })
})

describe("Array.reduceRightWithIndex", () => {
  test("Array.reduceRightWithIndex", () => {
    module Test = {
      Array.reduceRightWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i)->assertEqual(16)

      Array.reduceRightWithIndex(
        [],
        list{},
        (acc, v, i) => list{v + i, ...acc},
      )->assertEqual(list{})
    }
    ()
  })
})

describe("Array.reduceRight", () => {
  test("Array.reduceRight", () => {
    module Test = {
      Array.reduceRight(["a", "b", "c", "d"], "", (a, b) => a ++ b)->assertEqual("dcba")

      Array.reduceRight([1, 2, 3], list{}, List.add)->assertEqual(list{1, 2, 3})

      Array.reduceRight([], list{}, List.add)->assertEqual(list{})
    }
    ()
  })
})

describe("Array.reduceWithIndex", () => {
  test("Array.reduceWithIndex", () => {
    module Test = {
      Array.reduceWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i)->assertEqual(16)

      Array.reduceWithIndex(
        [1, 2, 3],
        list{},
        (acc, v, i) => list{v + i, ...acc},
      )->assertEqual(list{5, 3, 1})

      Array.reduceWithIndex([], list{}, (acc, v, i) => list{v + i, ...acc})->assertEqual(list{})
    }
    ()
  })
})

describe("Array.reduce", () => {
  test("Array.reduce", () => {
    module Test = {
      Array.reduce([2, 3, 4], 1, (a, b) => a + b)->assertEqual(10)

      Array.reduce(["a", "b", "c", "d"], "", (a, b) => a ++ b)->assertEqual("abcd")

      [1, 2, 3]
      ->Array.reduce(list{}, List.add)
      ->assertEqual(list{3, 2, 1})

      Array.reduce([], list{}, List.add)->assertEqual(list{})
    }
    ()
  })
})

describe("Array.mapWithIndex", () => {
  test("Array.mapWithIndex", () => {
    module Test = {
      let array = ["Hello", "Hi", "Good bye"]
      let mappedArray =
        array->Array.mapWithIndex(
          (greeting, index) => greeting ++ " at position " ++ Int.toString(index),
        )

      assertEqual(
        mappedArray,
        ["Hello at position 0", "Hi at position 1", "Good bye at position 2"],
      )
    }
    ()
  })
})

describe("Array.map", () => {
  test("Array.map", () => {
    module Test = {
      let array = ["Hello", "Hi", "Good bye"]
      let mappedArray = array->Array.map(greeting => greeting ++ " to you")

      assertEqual(mappedArray, ["Hello to you", "Hi to you", "Good bye to you"])
    }
    ()
  })
})

describe("Array.forEachWithIndex", () => {
  test("Array.forEachWithIndex", () => {
    module Test = {
      let array = ["Hello", "Hi", "Good bye"]

      array->Array.forEachWithIndex(
        (item, index) => {
          Console.log("At item " ++ Int.toString(index) ++ ": " ++ item)
        },
      )
    }
    ()
  })
})

describe("Array.forEach", () => {
  test("Array.forEach", () => {
    module Test = {
      let array = ["Hello", "Hi", "Good bye"]

      array->Array.forEach(
        item => {
          Console.log(item)
        },
      )
    }
    ()
  })
})

describe("Array.findIndexWithIndex", () => {
  test("Array.findIndexWithIndex", () => {
    module Test = {
      type languages = ReScript | TypeScript | JavaScript

      let array = [ReScript, JavaScript]

      let isReScriptFirst =
        array->Array.findIndexWithIndex((item, index) => index === 0 && item == ReScript)
      let isTypeScriptFirst =
        array->Array.findIndexWithIndex((item, index) => index === 0 && item == TypeScript)

      assertEqual(isReScriptFirst, 0)
      assertEqual(isTypeScriptFirst, -1)
    }
    ()
  })
})

describe("Array.findIndex", () => {
  test("Array.findIndex", () => {
    module Test = {
      type languages = ReScript | TypeScript | JavaScript

      let array = [ReScript, JavaScript]

      array
      ->Array.findIndex(item => item == ReScript)
      ->assertEqual(0)

      array
      ->Array.findIndex(item => item == TypeScript)
      ->assertEqual(-1)
    }
    ()
  })
})

describe("Array.findWithIndex", () => {
  test("Array.findWithIndex", () => {
    module Test = {
      type languages = ReScript | TypeScript | JavaScript

      let array = [TypeScript, JavaScript, ReScript]

      array
      ->Array.findWithIndex((item, index) => index > 1 && item == ReScript)
      ->assertEqual(Some(ReScript))
    }
    ()
  })
})

describe("Array.find", () => {
  test("Array.find", () => {
    module Test = {
      type languages = ReScript | TypeScript | JavaScript

      let array = [ReScript, TypeScript, JavaScript]

      array
      ->Array.find(item => item == ReScript)
      ->assertEqual(Some(ReScript))
    }
    ()
  })
})

describe("Array.filterWithIndex", () => {
  test("Array.filterWithIndex", () => {
    module Test = {
      [1, 2, 3, 4]
      ->Array.filterWithIndex((num, index) => index === 0 || num === 2)
      ->assertEqual([1, 2])
    }
    ()
  })
})

describe("Array.filter", () => {
  test("Array.filter", () => {
    module Test = {
      [1, 2, 3, 4]
      ->Array.filter(num => num > 2)
      ->assertEqual([3, 4])
    }
    ()
  })
})

describe("Array.everyWithIndex", () => {
  test("Array.everyWithIndex", () => {
    module Test = {
      let array = [1, 2, 3, 4]

      array
      ->Array.everyWithIndex((num, index) => index < 5 && num <= 4)
      ->assertEqual(true)

      array
      ->Array.everyWithIndex((num, index) => index < 2 && num >= 2)
      ->assertEqual(false)
    }
    ()
  })
})

describe("Array.every", () => {
  test("Array.every", () => {
    module Test = {
      let array = [1, 2, 3, 4]

      array
      ->Array.every(num => num <= 4)
      ->assertEqual(true)

      array
      ->Array.every(num => num === 1)
      ->assertEqual(false)
    }
    ()
  })
})

describe("Array.toString", () => {
  test("Array.toString", () => {
    module Test = {
      [1, 2, 3, 4]
      ->Array.toString
      ->assertEqual("1,2,3,4")
    }
    ()
  })
})

describe("Array.copy", () => {
  test("Array.copy", () => {
    module Test = {
      let myArray = [1, 2, 3]
      let copyOfMyArray = myArray->Array.copy

      copyOfMyArray->assertEqual([1, 2, 3])
      assertEqual(myArray === copyOfMyArray, false)
    }
    ()
  })
})

describe("Array.sliceToEnd", () => {
  test("Array.sliceToEnd", () => {
    module Test = {
      [1, 2, 3, 4]
      ->Array.sliceToEnd(~start=1)
      ->assertEqual([2, 3, 4])
    }
    ()
  })
})

describe("Array.slice", () => {
  test("Array.slice", () => {
    module Test = {
      [1, 2, 3, 4]
      ->Array.slice(~start=1, ~end=3)
      ->assertEqual([2, 3])
    }
    ()
  })
})

describe("Array.joinWithUnsafe", () => {
  test("Array.joinWithUnsafe", () => {
    module Test = {
      [1, 2, 3]
      ->Array.joinWithUnsafe(" -- ")
      ->assertEqual("1 -- 2 -- 3")
    }
    ()
  })
})

describe("Array.joinUnsafe", () => {
  test("Array.joinUnsafe", () => {
    module Test = {
      [1, 2, 3]
      ->Array.joinUnsafe(" -- ")
      ->assertEqual("1 -- 2 -- 3")
    }
    ()
  })
})

describe("Array.joinWith", () => {
  test("Array.joinWith", () => {
    module Test = {
      ["One", "Two", "Three"]
      ->Array.joinWith(" -- ")
      ->assertEqual("One -- Two -- Three")
    }
    ()
  })
})

describe("Array.join", () => {
  test("Array.join", () => {
    module Test = {
      ["One", "Two", "Three"]
      ->Array.join(" -- ")
      ->assertEqual("One -- Two -- Three")
    }
    ()
  })
})

describe("Array.indexOfOpt", () => {
  test("Array.indexOfOpt", () => {
    module Test = {
      [1, 2]->Array.indexOfOpt(2)->assertEqual(Some(1))
      [1, 2]->Array.indexOfOpt(3)->assertEqual(None)
      [{"language": "ReScript"}]
      ->Array.indexOfOpt({"language": "ReScript"})
      ->assertEqual(None) // None, because of strict equality
    }
    ()
  })
})

describe("Array.indexOf", () => {
  test("Array.indexOf", () => {
    module Test = {
      [1, 2]->Array.indexOf(2)->assertEqual(1)
      [1, 2]->Array.indexOf(3)->assertEqual(-1)

      [{"language": "ReScript"}]
      ->Array.indexOf({"language": "ReScript"})
      ->assertEqual(-1) // -1, because of strict equality
    }
    ()
  })
})

describe("Array.includes", () => {
  test("Array.includes", () => {
    module Test = {
      [1, 2]->Array.includes(1)->assertEqual(true)
      [1, 2]->Array.includes(3)->assertEqual(false)

      [{"language": "ReScript"}]
      ->Array.includes({"language": "ReScript"})
      ->assertEqual(false) // false, because of strict equality
    }
    ()
  })
})

describe("Array.flat", () => {
  test("Array.flat", () => {
    module Test = {
      [[1], [2], [3, 4]]
      ->Array.flat
      ->assertEqual([1, 2, 3, 4])
    }
    ()
  })
})

describe("Array.concatMany", () => {
  test("Array.concatMany", () => {
    module Test = {
      let array1 = ["hi", "hello"]
      let array2 = ["yay"]
      let array3 = ["wehoo"]

      let someArray = array1->Array.concatMany([array2, array3])

      Console.log(someArray) // ["hi", "hello", "yay", "wehoo"]
    }
    ()
  })
})

describe("Array.concat", () => {
  test("Array.concat", () => {
    module Test = {
      let array1 = ["hi", "hello"]
      let array2 = ["yay", "wehoo"]

      let someArray = array1->Array.concat(array2)

      someArray->assertEqual(["hi", "hello", "yay", "wehoo"])
    }
    ()
  })
})

describe("Array.unshiftMany", () => {
  test("Array.unshiftMany", () => {
    module Test = {
      let someArray = ["hi", "hello"]
      someArray->Array.unshiftMany(["yay", "wehoo"])
      someArray->assertEqual(["yay", "wehoo", "hi", "hello"])
    }
    ()
  })
})

describe("Array.unshift", () => {
  test("Array.unshift", () => {
    module Test = {
      let someArray = ["hi", "hello"]
      someArray->Array.unshift("yay")
      someArray->assertEqual(["yay", "hi", "hello"])
    }
    ()
  })
})

describe("Array.sort", () => {
  test("Array.sort", () => {
    module Test = {
      let array = [3, 2, 1]
      array->Array.sort((a, b) => float(a - b))
      array->assertEqual([1, 2, 3])
    }
    ()
  })
})

describe("Array.shift", () => {
  test("Array.shift", () => {
    module Test = {
      let someArray = ["hi", "hello"]

      someArray
      ->Array.shift
      ->assertEqual(Some("hi"))

      someArray->assertEqual(["hello"]) // Notice first item is gone.
    }
    ()
  })
})

describe("Array.reverse", () => {
  test("Array.reverse", () => {
    module Test = {
      let someArray = ["hi", "hello"]
      someArray->Array.reverse

      someArray->assertEqual(["hello", "hi"])
    }
    ()
  })
})

describe("Array.pushMany", () => {
  test("Array.pushMany", () => {
    module Test = {
      let someArray = ["hi", "hello"]

      someArray->Array.pushMany(["yay", "wehoo"])
      someArray->assertEqual(["hi", "hello", "yay", "wehoo"])
    }
    ()
  })
})

describe("Array.push", () => {
  test("Array.push", () => {
    module Test = {
      let someArray = ["hi", "hello"]

      someArray->Array.push("yay")

      someArray->assertEqual(["hi", "hello", "yay"])
    }
    ()
  })
})

describe("Array.pop", () => {
  test("Array.pop", () => {
    module Test = {
      let someArray = ["hi", "hello"]

      someArray
      ->Array.pop
      ->assertEqual(Some("hello"))

      someArray->assertEqual(["hi"]) // Notice last item is gone.
    }
    ()
  })
})

describe("Array.fill", () => {
  test("Array.fill", () => {
    module Test = {
      let myArray = [1, 2, 3, 4]

      myArray->Array.fill(9, ~start=1, ~end=3)

      myArray->assertEqual([1, 9, 9, 4])
    }
    ()
  })
})

describe("Array.fillToEnd", () => {
  test("Array.fillToEnd", () => {
    module Test = {
      let myArray = [1, 2, 3, 4]
      myArray->Array.fillToEnd(9, ~start=1)
      myArray->assertEqual([1, 9, 9, 9])
    }
    ()
  })
})

describe("Array.fillAll", () => {
  test("Array.fillAll", () => {
    module Test = {
      let myArray = [1, 2, 3, 4]
      myArray->Array.fillAll(9)
      myArray->assertEqual([9, 9, 9, 9])
    }
    ()
  })
})

describe("Array.length", () => {
  test("Array.length", () => {
    module Test = {
      let someArray = ["hi", "hello"]

      someArray
      ->Array.length
      ->assertEqual(2)
    }
    ()
  })
})

describe("Array.fromInitializer", () => {
  test("Array.fromInitializer", () => {
    module Test = {
      Array.fromInitializer(~length=3, i => i + 3)->assertEqual([3, 4, 5])

      Array.fromInitializer(~length=7, i => i + 3)->assertEqual([3, 4, 5, 6, 7, 8, 9])
    }
    ()
  })
})

describe("Array.make", () => {
  test("Array.make", () => {
    module Test = {
      Array.make(~length=3, #apple)->assertEqual([#apple, #apple, #apple])
      Array.make(~length=6, 7)->assertEqual([7, 7, 7, 7, 7, 7])
    }
    ()
  })
})

describe("Array.fromIterator", () => {
  test("Array.fromIterator", () => {
    module Test = {
      Map.fromArray([("foo", 1), ("bar", 2)])
      ->Map.values
      ->Array.fromIterator
      ->assertEqual([1, 2])
    }
    ()
  })
})

describe("Belt_Float./", () => {
  test("Belt_Float./", () => {
    module Test = {
      open Belt.Float
      assertEqual(4.0 / 2.0, 2.0)
    }
    ()
  })
})

describe("Belt_Float.*", () => {
  test("Belt_Float.*", () => {
    module Test = {
      open Belt.Float
      assertEqual(2.0 * 2.0, 4.0)
    }
    ()
  })
})

describe("Belt_Float.-", () => {
  test("Belt_Float.-", () => {
    module Test = {
      open Belt.Float
      assertEqual(2.0 - 1.0, 1.0)
    }
    ()
  })
})

describe("Belt_Float.+", () => {
  test("Belt_Float.+", () => {
    module Test = {
      open Belt.Float
      assertEqual(2.0 + 2.0, 4.0)
    }
    ()
  })
})

describe("Belt_Float.toString", () => {
  test("Belt_Float.toString", () => {
    module Test = {
      Js.log(Belt.Float.toString(1.0) === "1.0") /* true */
    }
    ()
  })
})

describe("Belt_Float.fromString", () => {
  test("Belt_Float.fromString", () => {
    module Test = {
      Js.log(Belt.Float.fromString("1.0") === Some(1.0)) /* true */
    }
    ()
  })
})

describe("Belt_Float.fromInt", () => {
  test("Belt_Float.fromInt", () => {
    module Test = {
      Js.log(Belt.Float.fromInt(1) === 1.0) /* true */
    }
    ()
  })
})

describe("Belt_Float.toInt", () => {
  test("Belt_Float.toInt", () => {
    module Test = {
      Js.log(Belt.Float.toInt(1.0) === 1) /* true */
    }
    ()
  })
})

describe("Belt_Array.truncateToLengthUnsafe", () => {
  test("Belt_Array.truncateToLengthUnsafe", () => {
    module Test = {
      let arr = ["ant", "bee", "cat", "dog", "elk"]

      Belt.Array.truncateToLengthUnsafe(arr, 3)

      arr == ["ant", "bee", "cat"]
    }
    ()
  })
})

describe("Belt_Array.eq", () => {
  test("Belt_Array.eq", () => {
    module Test = {
      Belt.Array.eq([1, 2, 3], [-1, -2, -3], (a, b) => abs(a) == abs(b)) == true
    }
    ()
  })
})

describe("Belt_Array.cmp", () => {
  test("Belt_Array.cmp", () => {
    module Test = {
      Belt.Array.cmp([1, 3, 5], [1, 4, 2], (a, b) => compare(a, b)) == -1

      Belt.Array.cmp([1, 3, 5], [1, 2, 3], (a, b) => compare(a, b)) == 1

      Belt.Array.cmp([1, 3, 5], [1, 3, 5], (a, b) => compare(a, b)) == 0
    }
    ()
  })
})

describe("Belt_Array.some2", () => {
  test("Belt_Array.some2", () => {
    module Test = {
      Belt.Array.some2([0, 2], [1, 0, 3], (a, b) => a > b) == true

      Belt.Array.some2([], [1], (x, y) => x > y) == false

      Belt.Array.some2([2, 3], [1, 4], (x, y) => x > y) == true
    }
    ()
  })
})

describe("Belt_Array.every2", () => {
  test("Belt_Array.every2", () => {
    module Test = {
      Belt.Array.every2([1, 2, 3], [0, 1], (a, b) => a > b) == true

      Belt.Array.every2([], [1], (x, y) => x > y) == true

      Belt.Array.every2([2, 3], [1], (x, y) => x > y) == true

      Belt.Array.every2([0, 1], [5, 0], (x, y) => x > y) == false
    }
    ()
  })
})

describe("Belt_Array.every", () => {
  test("Belt_Array.every", () => {
    module Test = {
      Belt.Array.every([1, 3, 5], x => mod(x, 2) == 1) == true

      Belt.Array.every([1, -3, 5], x => x > 0) == false
    }
    ()
  })
})

describe("Belt_Array.some", () => {
  test("Belt_Array.some", () => {
    module Test = {
      Belt.Array.some([2, 3, 4], x => mod(x, 2) == 1) == true

      Belt.Array.some([-1, -3, -5], x => x > 0) == false
    }
    ()
  })
})

describe("Belt_Array.joinWith", () => {
  test("Belt_Array.joinWith", () => {
    module Test = {
      Belt.Array.joinWith([0, 1], ", ", Js.Int.toString) == "0, 1"
      Belt.Array.joinWith([], " ", Js.Int.toString) == ""
      Belt.Array.joinWith([1], " ", Js.Int.toString) == "1"
    }
    ()
  })
})

describe("Belt_Array.reduceWithIndex", () => {
  test("Belt_Array.reduceWithIndex", () => {
    module Test = {
      Belt.Array.reduceWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i) == 16
    }
    ()
  })
})

describe("Belt_Array.reduceReverse2", () => {
  test("Belt_Array.reduceReverse2", () => {
    module Test = {
      Belt.Array.reduceReverse2([1, 2, 3], [1, 2], 0, (acc, x, y) => acc + x + y) == 6
    }
    ()
  })
})

describe("Belt_Array.reduceReverse", () => {
  test("Belt_Array.reduceReverse", () => {
    module Test = {
      Belt.Array.reduceReverse(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "dcba"
    }
    ()
  })
})

describe("Belt_Array.reduce", () => {
  test("Belt_Array.reduce", () => {
    module Test = {
      Belt.Array.reduce([2, 3, 4], 1, (a, b) => a + b) == 10

      Belt.Array.reduce(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "abcd"
    }
    ()
  })
})

describe("Belt_Array.partition", () => {
  test("Belt_Array.partition", () => {
    module Test = {
      Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) == 0) == ([2, 4], [1, 3, 5])

      Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) != 0) == ([1, 3, 5], [2, 4])
    }
    ()
  })
})

describe("Belt_Array.mapWithIndex", () => {
  test("Belt_Array.mapWithIndex", () => {
    module Test = {
      Belt.Array.mapWithIndex([1, 2, 3], (i, x) => i + x) == [0 + 1, 1 + 2, 2 + 3]
    }
    ()
  })
})

describe("Belt_Array.forEachWithIndex", () => {
  test("Belt_Array.forEachWithIndex", () => {
    module Test = {
      Belt.Array.forEachWithIndex(
        ["a", "b", "c"],
        (i, x) => Js.log("Item " ++ Belt.Int.toString(i) ++ " is " ++ x),
      )

      /*
  prints:
  Item 0 is a
  Item 1 is b
  Item 2 is cc
*/
      let total = ref(0)

      Belt.Array.forEachWithIndex([10, 11, 12, 13], (i, x) => total := total.contents + x + i)

      total.contents == 0 + 10 + 1 + 11 + 2 + 12 + 3 + 13
    }
    ()
  })
})

describe("Belt_Array.keepMap", () => {
  test("Belt_Array.keepMap", () => {
    module Test = {
      Belt.Array.keepMap(
        [1, 2, 3],
        x =>
          if mod(x, 2) == 0 {
            Some(x)
          } else {
            None
          },
      ) == [2]
    }
    ()
  })
})

describe("Belt_Array.keepWithIndex", () => {
  test("Belt_Array.keepWithIndex", () => {
    module Test = {
      Belt.Array.keepWithIndex([1, 2, 3], (_x, i) => i == 1) == [2]
    }
    ()
  })
})

describe("Belt_Array.getIndexBy", () => {
  test("Belt_Array.getIndexBy", () => {
    module Test = {
      Belt.Array.getIndexBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(1)
      Belt.Array.getIndexBy([15, 13, 11], x => mod(x, 2) == 0) == None
    }
    ()
  })
})

describe("Belt_Array.getBy", () => {
  test("Belt_Array.getBy", () => {
    module Test = {
      Belt.Array.getBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(4)
      Belt.Array.getBy([15, 13, 11], x => mod(x, 2) == 0) == None
    }
    ()
  })
})

describe("Belt_Array.flatMap", () => {
  test("Belt_Array.flatMap", () => {
    module Test = {
      Belt.Array.flatMap([1, 2], x => [x + 10, x + 20]) == [11, 21, 12, 22]
    }
    ()
  })
})

describe("Belt_Array.map", () => {
  test("Belt_Array.map", () => {
    module Test = {
      Belt.Array.map([1, 2], x => x + 1) == [3, 4]
    }
    ()
  })
})

describe("Belt_Array.forEach", () => {
  test("Belt_Array.forEach", () => {
    module Test = {
      Belt.Array.forEach(["a", "b", "c"], x => Js.log("Item: " ++ x))

      /*
  prints:
  Item: a
  Item: b
  Item: c
*/
      let total = ref(0)

      Belt.Array.forEach([1, 2, 3, 4], x => total := total.contents + x)

      total.contents == 1 + 2 + 3 + 4
    }
    ()
  })
})

describe("Belt_Array.blit", () => {
  test("Belt_Array.blit", () => {
    module Test = {
      let v1 = [10, 11, 12, 13, 14, 15, 16, 17]
      let v2 = [20, 21, 22, 23, 24, 25, 26, 27]

      Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v2, ~dstOffset=2, ~len=3)
      v2 == [20, 21, 14, 15, 16, 25, 26, 27]

      Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v1, ~dstOffset=2, ~len=3)
      v1 == [10, 11, 14, 15, 16, 15, 16, 17]
    }
    ()
  })
})

describe("Belt_Array.fill", () => {
  test("Belt_Array.fill", () => {
    module Test = {
      let arr = Belt.Array.makeBy(5, i => i)

      Belt.Array.fill(arr, ~offset=2, ~len=2, 9)

      arr == [0, 1, 9, 9, 4]

      Belt.Array.fill(arr, ~offset=7, ~len=2, 8)

      arr == [0, 1, 9, 9, 4]
    }
    ()
  })
})

describe("Belt_Array.sliceToEnd", () => {
  test("Belt_Array.sliceToEnd", () => {
    module Test = {
      Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], 2) == [12, 13, 14, 15, 16]

      Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], -4) == [13, 14, 15, 16]
    }
    ()
  })
})

describe("Belt_Array.slice", () => {
  test("Belt_Array.slice", () => {
    module Test = {
      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=2, ~len=3) == [12, 13, 14]

      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=-4, ~len=3) == [13, 14, 15]

      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=4, ~len=9) == [14, 15, 16]
    }
    ()
  })
})

describe("Belt_Array.concatMany", () => {
  test("Belt_Array.concatMany", () => {
    module Test = {
      Belt.Array.concatMany([[1, 2, 3], [4, 5, 6], [7, 8]]) == [1, 2, 3, 4, 5, 6, 7, 8]
    }
    ()
  })
})

describe("Belt_Array.concat", () => {
  test("Belt_Array.concat", () => {
    module Test = {
      Belt.Array.concat([1, 2, 3], [4, 5]) == [1, 2, 3, 4, 5]

      Belt.Array.concat([], ["a", "b", "c"]) == ["a", "b", "c"]
    }
    ()
  })
})

describe("Belt_Array.unzip", () => {
  test("Belt_Array.unzip", () => {
    module Test = {
      Belt.Array.unzip([(1, 2), (3, 4)]) == ([1, 3], [2, 4])

      Belt.Array.unzip([(1, 2), (3, 4), (5, 6), (7, 8)]) == ([1, 3, 5, 7], [2, 4, 6, 8])
    }
    ()
  })
})

describe("Belt_Array.zipBy", () => {
  test("Belt_Array.zipBy", () => {
    module Test = {
      Belt.Array.zipBy([1, 2, 3], [4, 5], (a, b) => 2 * a + b) == [6, 9]
    }
    ()
  })
})

describe("Belt_Array.zip", () => {
  test("Belt_Array.zip", () => {
    module Test = {
      Belt.Array.zip([1, 2], [3, 4, 5]) == [(1, 3), (2, 4)]
    }
    ()
  })
})

describe("Belt_Array.makeBy", () => {
  test("Belt_Array.makeBy", () => {
    module Test = {
      Belt.Array.makeBy(5, i => i) == [0, 1, 2, 3, 4]

      Belt.Array.makeBy(5, i => i * i) == [0, 1, 4, 9, 16]
    }
    ()
  })
})

describe("Belt_Array.rangeBy", () => {
  test("Belt_Array.rangeBy", () => {
    module Test = {
      Belt.Array.rangeBy(0, 10, ~step=3) == [0, 3, 6, 9]

      Belt.Array.rangeBy(0, 12, ~step=3) == [0, 3, 6, 9, 12]

      Belt.Array.rangeBy(33, 0, ~step=1) == []

      Belt.Array.rangeBy(33, 0, ~step=-1) == []

      Belt.Array.rangeBy(3, 12, ~step=-1) == []

      Belt.Array.rangeBy(3, 3, ~step=0) == []

      Belt.Array.rangeBy(3, 3, ~step=1) == [3]
    }
    ()
  })
})

describe("Belt_Array.range", () => {
  test("Belt_Array.range", () => {
    module Test = {
      Belt.Array.range(0, 3) == [0, 1, 2, 3]

      Belt.Array.range(3, 0) == []

      Belt.Array.range(3, 3) == [3]
    }
    ()
  })
})

describe("Belt_Array.makeUninitializedUnsafe", () => {
  test("Belt_Array.makeUninitializedUnsafe", () => {
    module Test = {
      let arr = Belt.Array.makeUninitializedUnsafe(5)

      Js.log(Belt.Array.getExn(arr, 0)) // undefined

      Belt.Array.setExn(arr, 0, "example")

      Js.log(Belt.Array.getExn(arr, 0) == "example")
    }
    ()
  })
})

describe("Belt_Array.makeUninitialized", () => {
  test("Belt_Array.makeUninitialized", () => {
    module Test = {
      let arr: array<Js.undefined<string>> = Belt.Array.makeUninitialized(5)

      Belt.Array.getExn(arr, 0) == Js.undefined
    }
    ()
  })
})

describe("Belt_Array.reverse", () => {
  test("Belt_Array.reverse", () => {
    module Test = {
      Belt.Array.reverse([10, 11, 12, 13, 14]) == [14, 13, 12, 11, 10]
    }
    ()
  })
})

describe("Belt_Array.reverseInPlace", () => {
  test("Belt_Array.reverseInPlace", () => {
    module Test = {
      let arr = [10, 11, 12, 13, 14]

      let () = Belt.Array.reverseInPlace(arr)

      arr == [14, 13, 12, 11, 10]
    }
    ()
  })
})

describe("Belt_Array.get", () => {
  test("Belt_Array.get", () => {
    module Test = {
      Belt.Array.get(["a", "b", "c"], 0) == Some("a")
      Belt.Array.get(["a", "b", "c"], 3) == None
      Belt.Array.get(["a", "b", "c"], -1) == None
    }
    ()
  })
})

describe("Belt_Array.length", () => {
  test("Belt_Array.length", () => {
    module Test = {
      // Returns 1
      Belt.Array.length(["test"])
    }
    ()
  })
})

describe("Belt_HashMap.logStats", () => {
  test("Belt_HashMap.logStats", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })
      let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(hMap, 1, "1")

      Belt.HashMap.logStats(hMap)
    }
    ()
  })
})

describe("Belt_HashMap.getBucketHistogram", () => {
  test("Belt_HashMap.getBucketHistogram", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })
      let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(hMap, 1, "1")

      Belt.HashMap.getBucketHistogram(hMap)
    }
    ()
  })
})

describe("Belt_HashMap.mergeMany", () => {
  test("Belt_HashMap.mergeMany", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.mergeMany(hMap, [(1, "1"), (2, "2")])
    }
    ()
  })
})

describe("Belt_HashMap.fromArray", () => {
  test("Belt_HashMap.fromArray", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.fromArray([(1, "value1"), (2, "value2")], ~id=module(IntHash))
      Belt.HashMap.toArray(s0) == [(1, "value1"), (2, "value2")]
    }
    ()
  })
})

describe("Belt_HashMap.valuesToArray", () => {
  test("Belt_HashMap.valuesToArray", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.set(s0, 2, "value2")

      Belt.HashMap.valuesToArray(s0) == ["value1", "value2"]
    }
    ()
  })
})

describe("Belt_HashMap.keysToArray", () => {
  test("Belt_HashMap.keysToArray", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.set(s0, 2, "value2")

      Belt.HashMap.keysToArray(s0) == [1, 2]
    }
    ()
  })
})

describe("Belt_HashMap.toArray", () => {
  test("Belt_HashMap.toArray", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.set(s0, 2, "value2")

      Belt.HashMap.toArray(s0) == [(1, "value1"), (2, "value2")]
    }
    ()
  })
})

describe("Belt_HashMap.size", () => {
  test("Belt_HashMap.size", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.set(s0, 2, "value2")

      Belt.HashMap.size(s0) == 2
    }
    ()
  })
})

describe("Belt_HashMap.keepMapInPlace", () => {
  test("Belt_HashMap.keepMapInPlace", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.set(s0, 2, "value2")

      Belt.HashMap.keepMapInPlace(s0, (key, value) => key == 1 ? None : Some(value))
    }
    ()
  })
})

describe("Belt_HashMap.reduce", () => {
  test("Belt_HashMap.reduce", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))

      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.set(s0, 2, "value2")

      s0
      ->Belt.HashMap.reduce("", (acc, _, value) => acc ++ (", " ++ value))
      ->assertEqual(", value1, value2")

      Console.log("lol")
    }
    ()
  })
})

describe("Belt_HashMap.forEach", () => {
  test("Belt_HashMap.forEach", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.forEach(s0, (key, value) => Js.log2(key, value))
      // prints (1, "value1")
    }
    ()
  })
})

describe("Belt_HashMap.remove", () => {
  test("Belt_HashMap.remove", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.remove(s0, 1)
      Belt.HashMap.has(s0, 1) == false
    }
    ()
  })
})

describe("Belt_HashMap.has", () => {
  test("Belt_HashMap.has", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")

      Belt.HashMap.has(s0, 1) == true
      Belt.HashMap.has(s0, 2) == false
    }
    ()
  })
})

describe("Belt_HashMap.get", () => {
  test("Belt_HashMap.get", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")

      Belt.HashMap.get(s0, 1) == Some("value1")
      Belt.HashMap.get(s0, 2) == None
    }
    ()
  })
})

describe("Belt_HashMap.copy", () => {
  test("Belt_HashMap.copy", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntHash))
      let s1 = Belt.HashMap.copy(s0)

      Belt.HashMap.set(s0, 2, "3")

      Belt.HashMap.get(s0, 2) != Belt.HashMap.get(s1, 2)
    }
    ()
  })
})

describe("Belt_HashMap.set", () => {
  test("Belt_HashMap.set", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntHash))

      Belt.HashMap.set(s0, 2, "3")

      Belt.HashMap.valuesToArray(s0) == ["1", "3", "3"]
    }
    ()
  })
})

describe("Belt_HashMap.isEmpty", () => {
  test("Belt_HashMap.isEmpty", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      Belt.HashMap.isEmpty(Belt.HashMap.fromArray([(1, "1")], ~id=module(IntHash))) == false
    }
    ()
  })
})

describe("Belt_HashMap.clear", () => {
  test("Belt_HashMap.clear", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let hMap = Belt.HashMap.fromArray([(1, "1")], ~id=module(IntHash))
      Belt.HashMap.clear(hMap)
      Belt.HashMap.isEmpty(hMap) == true
    }
    ()
  })
})

describe("Belt_HashMap.make", () => {
  test("Belt_HashMap.make", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))

      Belt.HashMap.set(hMap, 0, "a")
    }
    ()
  })
})

describe("Belt.Map.Dict.findFirstBy", () => {
  test("Belt.Map.Dict.findFirstBy", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Map.Dict.fromArray([(4, "4"), (1, "1"), (2, "2"), (3, "3")], ~cmp=IntCmp.cmp)

      Belt.Map.Dict.findFirstBy(s0, (k, _) => k == 4) == Some((4, "4"))
    }
    ()
  })
})

describe("Belt.Map.String.findFirstBy", () => {
  test("Belt.Map.String.findFirstBy", () => {
    module Test = {
      let mapString = Belt.Map.String.fromArray([("1", "one"), ("2", "two"), ("3", "three")])

      mapString
      ->Belt.Map.String.findFirstBy((k, v) => k == "1" && v == "one")
      ->assertEqual(Some("1", "one"))
    }
    ()
  })
})

describe("Belt.Map.Int.findFirstBy", () => {
  test("Belt.Map.Int.findFirstBy", () => {
    module Test = {
      let mapInt = Belt.Map.Int.fromArray([(1, "one"), (2, "two"), (3, "three")])

      mapInt
      ->Belt.Map.Int.findFirstBy((k, v) => k == 1 && v == "one")
      ->assertEqual(Some(1, "one"))
    }
    ()
  })
})

describe("Belt.Set.Dict.split", () => {
  test("Belt.Set.Dict.split", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)

      let ((smaller, larger), present) = s0->Belt.Set.Dict.split(3, ~cmp=IntCmp.cmp)

      present /* true */
      smaller->Belt.Set.Dict.toArray /* [1,2] */
      larger->Belt.Set.Dict.toArray /* [4,5] */
    }
    ()
  })
})

describe("Belt.Set.Dict.get", () => {
  test("Belt.Set.Dict.get", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.get(3, ~cmp=IntCmp.cmp) /* Some(3) */
      s0->Belt.Set.Dict.get(20, ~cmp=IntCmp.cmp) /* None */
    }
    ()
  })
})

describe("Belt.Set.Dict.maxUndefined", () => {
  test("Belt.Set.Dict.maxUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.maxUndefined /* undefined */
      s1->Belt.Set.Dict.maxUndefined /* 5 */
    }
    ()
  })
})

describe("Belt.Set.Dict.maximum", () => {
  test("Belt.Set.Dict.maximum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.maximum /* None */
      s1->Belt.Set.Dict.maximum /* Some(5) */
    }
    ()
  })
})

describe("Belt.Set.Dict.minUndefined", () => {
  test("Belt.Set.Dict.minUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.minUndefined /* undefined */
      s1->Belt.Set.Dict.minUndefined /* 1 */
    }
    ()
  })
})

describe("Belt.Set.Dict.minimum", () => {
  test("Belt.Set.Dict.minimum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.minimum /* None */
      s1->Belt.Set.Dict.minimum /* Some(1) */
    }
    ()
  })
})

describe("Belt.Set.Dict.toArray", () => {
  test("Belt.Set.Dict.toArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.toArray /* [1,2,3,5] */
    }
    ()
  })
})

describe("Belt.Set.Dict.toList", () => {
  test("Belt.Set.Dict.toList", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.toList /* [1,2,3,5] */
    }
    ()
  })
})

describe("Belt.Set.Dict.size", () => {
  test("Belt.Set.Dict.size", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.size /* 4 */
    }
    ()
  })
})

describe("Belt.Set.Dict.partition", () => {
  test("Belt.Set.Dict.partition", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)
      let (s1, s2) = s0->Belt.Set.Dict.partition(isOdd)

      s1->Belt.Set.Dict.toArray /* [1,3,5] */
      s2->Belt.Set.Dict.toArray /* [2,4] */
    }
    ()
  })
})

describe("Belt.Set.Dict.keep", () => {
  test("Belt.Set.Dict.keep", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)
      let s1 = s0->Belt.Set.Dict.keep(isEven)

      s1->Belt.Set.Dict.toArray /* [2,4] */
    }
    ()
  })
})

describe("Belt.Set.Dict.some", () => {
  test("Belt.Set.Dict.some", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.Set.Dict.fromArray([1, 2, 4, 6, 8], ~cmp=IntCmp.cmp)
      s0->Belt.Set.Dict.some(isOdd) /* true */
    }
    ()
  })
})

describe("Belt.Set.Dict.every", () => {
  test("Belt.Set.Dict.every", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.Set.Dict.fromArray([2, 4, 6, 8], ~cmp=IntCmp.cmp)
      s0->Belt.Set.Dict.every(isEven) /* true */
    }
    ()
  })
})

describe("Belt.Set.Dict.reduce", () => {
  test("Belt.Set.Dict.reduce", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      s0->Belt.Set.Dict.reduce(
        list{},
        (acc, element) => acc->Belt.List.add(element),
      ) /* [6,5,3,2] */
    }
    ()
  })
})

describe("Belt.Set.Dict.forEach", () => {
  test("Belt.Set.Dict.forEach", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let acc = ref(list{})
      s0->Belt.Set.Dict.forEach(x => acc := Belt.List.add(acc.contents, x))
      acc /* [6,5,3,2] */
    }
    ()
  })
})

describe("Belt.Set.Dict.eq", () => {
  test("Belt.Set.Dict.eq", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([3, 2, 5], ~cmp=IntCmp.cmp)

      Belt.Set.Dict.eq(s0, s1, ~cmp=IntCmp.cmp) /* true */
    }
    ()
  })
})

describe("Belt.Set.Dict.subset", () => {
  test("Belt.Set.Dict.subset", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
      let s2 = Belt.Set.Dict.intersect(s0, s1, ~cmp=IntCmp.cmp)
      Belt.Set.Dict.subset(s2, s0, ~cmp=IntCmp.cmp) /* true */
      Belt.Set.Dict.subset(s2, s1, ~cmp=IntCmp.cmp) /* true */
      Belt.Set.Dict.subset(s1, s0, ~cmp=IntCmp.cmp) /* false */
    }
    ()
  })
})

describe("Belt.Set.Dict.diff", () => {
  test("Belt.Set.Dict.diff", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)

      let diff1 = Belt.Set.Dict.diff(s0, s1, ~cmp=IntCmp.cmp)
      let diff2 = Belt.Set.Dict.diff(s1, s0, ~cmp=IntCmp.cmp)

      diff1->Belt.Set.Dict.toArray /* [6] */
      diff2->Belt.Set.Dict.toArray /* [1,4] */
    }
    ()
  })
})

describe("Belt.Set.Dict.intersect", () => {
  test("Belt.Set.Dict.intersect", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
      let intersect = Belt.Set.Dict.intersect(s0, s1, ~cmp=IntCmp.cmp)
      intersect->Belt.Set.Dict.toArray /* [2,3,5] */
    }
    ()
  })
})

describe("Belt.Set.Dict.union", () => {
  test("Belt.Set.Dict.union", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
      let union = Belt.Set.Dict.union(s0, s1, ~cmp=IntCmp.cmp)
      union->Belt.Set.Dict.toArray /* [1,2,3,4,5,6] */
    }
    ()
  })
})

describe("Belt.Set.Dict.removeMany", () => {
  test("Belt.Set.Dict.removeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.Dict.fromArray([1, 2, 3, 4], ~cmp=IntCmp.cmp)

      let newSet = set->Belt.Set.Dict.removeMany([5, 4, 3, 2, 1], ~cmp=IntCmp.cmp)
      newSet->Belt.Set.Dict.toArray /* [] */
    }
    ()
  })
})

describe("Belt.Set.Dict.remove", () => {
  test("Belt.Set.Dict.remove", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([2, 3, 1, 4, 5], ~cmp=IntCmp.cmp)
      let s1 = s0->Belt.Set.Dict.remove(1, ~cmp=IntCmp.cmp)
      let s2 = s1->Belt.Set.Dict.remove(3, ~cmp=IntCmp.cmp)
      let s3 = s2->Belt.Set.Dict.remove(3, ~cmp=IntCmp.cmp)

      s1->Belt.Set.Dict.toArray /* [2,3,4,5] */
      s2->Belt.Set.Dict.toArray /* [2,4,5] */
      s2 == s3 /* true */
    }
    ()
  })
})

describe("Belt.Set.Dict.mergeMany", () => {
  test("Belt.Set.Dict.mergeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.Dict.empty

      let newSet = set->Belt.Set.Dict.mergeMany([5, 4, 3, 2, 1], ~cmp=IntCmp.cmp)
      newSet->Belt.Set.Dict.toArray /* [1, 2, 3, 4, 5] */
    }
    ()
  })
})

describe("Belt.Set.Dict.add", () => {
  test("Belt.Set.Dict.add", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = s0->Belt.Set.Dict.add(1, ~cmp=IntCmp.cmp)
      let s2 = s1->Belt.Set.Dict.add(2, ~cmp=IntCmp.cmp)
      let s3 = s2->Belt.Set.Dict.add(2, ~cmp=IntCmp.cmp)
      s0->Belt.Set.Dict.toArray /* [] */
      s1->Belt.Set.Dict.toArray /* [1] */
      s2->Belt.Set.Dict.toArray /* [1, 2] */
      s3->Belt.Set.Dict.toArray /* [1,2 ] */
      s2 == s3 /* true */
    }
    ()
  })
})

describe("Belt.Set.Dict.has", () => {
  test("Belt.Set.Dict.has", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.Dict.fromArray([1, 4, 2, 5], ~cmp=IntCmp.cmp)

      set->Belt.Set.Dict.has(3, ~cmp=IntCmp.cmp) /* false */
      set->Belt.Set.Dict.has(1, ~cmp=IntCmp.cmp) /* true */
    }
    ()
  })
})

describe("Belt.Set.Dict.isEmpty", () => {
  test("Belt.Set.Dict.isEmpty", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let empty = Belt.Set.Dict.fromArray([], ~cmp=IntCmp.cmp)
      let notEmpty = Belt.Set.Dict.fromArray([1], ~cmp=IntCmp.cmp)

      Belt.Set.Dict.isEmpty(empty) /* true */
      Belt.Set.Dict.isEmpty(notEmpty) /* false */
    }
    ()
  })
})

describe("Belt.Set.Dict.fromArray", () => {
  test("Belt.Set.Dict.fromArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([1, 3, 2, 4], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.toArray /* [1, 2, 3, 4] */
    }
    ()
  })
})

describe("Belt.Set.Dict.empty", () => {
  test("Belt.Set.Dict.empty", () => {
    module Test = {
      let s0 = Belt.Set.Dict.empty
    }
    ()
  })
})

describe("Belt.Float./", () => {
  test("Belt.Float./", () => {
    module Test = {
      open Belt.Float
      assertEqual(4.0 / 2.0, 2.0)
    }
    ()
  })
})

describe("Belt.Float.*", () => {
  test("Belt.Float.*", () => {
    module Test = {
      open Belt.Float
      assertEqual(2.0 * 2.0, 4.0)
    }
    ()
  })
})

describe("Belt.Float.-", () => {
  test("Belt.Float.-", () => {
    module Test = {
      open Belt.Float
      assertEqual(2.0 - 1.0, 1.0)
    }
    ()
  })
})

describe("Belt.Float.+", () => {
  test("Belt.Float.+", () => {
    module Test = {
      open Belt.Float
      assertEqual(2.0 + 2.0, 4.0)
    }
    ()
  })
})

describe("Belt.Float.toString", () => {
  test("Belt.Float.toString", () => {
    module Test = {
      Js.log(Belt.Float.toString(1.0) === "1.0") /* true */
    }
    ()
  })
})

describe("Belt.Float.fromString", () => {
  test("Belt.Float.fromString", () => {
    module Test = {
      Js.log(Belt.Float.fromString("1.0") === Some(1.0)) /* true */
    }
    ()
  })
})

describe("Belt.Float.fromInt", () => {
  test("Belt.Float.fromInt", () => {
    module Test = {
      Js.log(Belt.Float.fromInt(1) === 1.0) /* true */
    }
    ()
  })
})

describe("Belt.Float.toInt", () => {
  test("Belt.Float.toInt", () => {
    module Test = {
      Js.log(Belt.Float.toInt(1.0) === 1) /* true */
    }
    ()
  })
})

describe("Belt.Int./", () => {
  test("Belt.Int./", () => {
    module Test = {
      open Belt.Int
      assertEqual(4 / 2, 2)
    }
    ()
  })
})

describe("Belt.Int.*", () => {
  test("Belt.Int.*", () => {
    module Test = {
      open Belt.Int
      assertEqual(2 * 2, 4)
    }
    ()
  })
})

describe("Belt.Int.-", () => {
  test("Belt.Int.-", () => {
    module Test = {
      open Belt.Int
      assertEqual(2 - 1, 1)
    }
    ()
  })
})

describe("Belt.Int.+", () => {
  test("Belt.Int.+", () => {
    module Test = {
      open Belt.Int
      assertEqual(2 + 2, 4)
    }
    ()
  })
})

describe("Belt.Int.toString", () => {
  test("Belt.Int.toString", () => {
    module Test = {
      Belt.Int.toString(1)->assertEqual("1")
    }
    ()
  })
})

describe("Belt.Int.fromString", () => {
  test("Belt.Int.fromString", () => {
    module Test = {
      Belt.Int.fromString("1")->assertEqual(Some(1))
    }
    ()
  })
})

describe("Belt.Int.fromFloat", () => {
  test("Belt.Int.fromFloat", () => {
    module Test = {
      Belt.Int.fromFloat(1.0)->assertEqual(1)
    }
    ()
  })
})

describe("Belt.Int.toFloat", () => {
  test("Belt.Int.toFloat", () => {
    module Test = {
      Belt.Int.toFloat(1)->assertEqual(1.0)
    }
    ()
  })
})

describe("Belt.Result.cmp", () => {
  test("Belt.Result.cmp", () => {
    module Test = {
      let good1 = Belt.Result.Ok(59)

      let good2 = Belt.Result.Ok(37)

      let bad1 = Belt.Result.Error("invalid")

      let bad2 = Belt.Result.Error("really invalid")

      let mod10cmp = (a, b) => Pervasives.compare(mod(a, 10), mod(b, 10))

      Belt.Result.cmp(Ok(39), Ok(57), mod10cmp) == 1

      Belt.Result.cmp(Ok(57), Ok(39), mod10cmp) == -1

      Belt.Result.cmp(Ok(39), Error("y"), mod10cmp) == 1

      Belt.Result.cmp(Error("x"), Ok(57), mod10cmp) == -1

      Belt.Result.cmp(Error("x"), Error("y"), mod10cmp) == 0
    }
    ()
  })
})

describe("Belt.Result.eq", () => {
  test("Belt.Result.eq", () => {
    module Test = {
      let good1 = Belt.Result.Ok(42)

      let good2 = Belt.Result.Ok(32)

      let bad1 = Belt.Result.Error("invalid")

      let bad2 = Belt.Result.Error("really invalid")

      let mod10equal = (a, b) => mod(a, 10) === mod(b, 10)

      Belt.Result.eq(good1, good2, mod10equal) == true

      Belt.Result.eq(good1, bad1, mod10equal) == false

      Belt.Result.eq(bad2, good2, mod10equal) == false

      Belt.Result.eq(bad1, bad2, mod10equal) == true
    }
    ()
  })
})

describe("Belt.Result.getWithDefault", () => {
  test("Belt.Result.getWithDefault", () => {
    module Test = {
      Belt.Result.getWithDefault(Ok(42), 0) == 42

      Belt.Result.getWithDefault(Error("Invalid Data"), 0) == 0
    }
    ()
  })
})

describe("Belt.Result.flatMap", () => {
  test("Belt.Result.flatMap", () => {
    module Test = {
      let recip = x =>
        if x !== 0.0 {
          Belt.Result.Ok(1.0 /. x)
        } else {
          Belt.Result.Error("Divide by zero")
        }

      Belt.Result.flatMap(Ok(2.0), recip) == Ok(0.5)

      Belt.Result.flatMap(Ok(0.0), recip) == Error("Divide by zero")

      Belt.Result.flatMap(Error("Already bad"), recip) == Error("Already bad")
    }
    ()
  })
})

describe("Belt.Result.map", () => {
  test("Belt.Result.map", () => {
    module Test = {
      let f = x => sqrt(Belt.Int.toFloat(x))

      Belt.Result.map(Ok(64), f) == Ok(8.0)

      Belt.Result.map(Error("Invalid data"), f) == Error("Invalid data")
    }
    ()
  })
})

describe("Belt.Result.mapWithDefault", () => {
  test("Belt.Result.mapWithDefault", () => {
    module Test = {
      let ok = Belt.Result.Ok(42)
      Belt.Result.mapWithDefault(ok, 0, x => x / 2) == 21

      let error = Belt.Result.Error("Invalid data")
      Belt.Result.mapWithDefault(error, 0, x => x / 2) == 0
    }
    ()
  })
})

describe("Belt.Result.getExn", () => {
  test("Belt.Result.getExn", () => {
    module Test = {
      Belt.Result.Ok(42)
      ->Belt.Result.getExn
      ->assertEqual(42)

      switch Belt.Result.getExn(Belt.Result.Error("Invalid data")) {
      // raise a exception
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Belt.Option.cmp", () => {
  test("Belt.Option.cmp", () => {
    module Test = {
      let clockCompare = (a, b) => compare(mod(a, 12), mod(b, 12))

      open Belt.Option

      cmp(Some(3), Some(15), clockCompare) /* 0 */

      cmp(Some(3), Some(14), clockCompare) /* 1 */

      cmp(Some(2), Some(15), clockCompare) /* (-1) */

      cmp(None, Some(15), clockCompare) /* (-1) */

      cmp(Some(14), None, clockCompare) /* 1 */

      cmp(None, None, clockCompare) /* 0 */
    }
    ()
  })
})

describe("Belt.Option.eq", () => {
  test("Belt.Option.eq", () => {
    module Test = {
      let clockEqual = (a, b) => mod(a, 12) == mod(b, 12)

      open Belt.Option

      eq(Some(3), Some(15), clockEqual) /* true */

      eq(Some(3), None, clockEqual) /* false */

      eq(None, Some(3), clockEqual) /* false */

      eq(None, None, clockEqual) /* true */
    }
    ()
  })
})

describe("Belt.Option.isNone", () => {
  test("Belt.Option.isNone", () => {
    module Test = {
      Belt.Option.isNone(None) /* true */

      Belt.Option.isNone(Some(1)) /* false */
    }
    ()
  })
})

describe("Belt.Option.isSome", () => {
  test("Belt.Option.isSome", () => {
    module Test = {
      Belt.Option.isSome(None) /* false */

      Belt.Option.isSome(Some(1)) /* true */
    }
    ()
  })
})

describe("Belt.Option.orElse", () => {
  test("Belt.Option.orElse", () => {
    module Test = {
      Belt.Option.orElse(Some(1812), Some(1066)) == Some(1812)
      Belt.Option.orElse(None, Some(1066)) == Some(1066)
      Belt.Option.orElse(None, None) == None
    }
    ()
  })
})

describe("Belt.Option.getWithDefault", () => {
  test("Belt.Option.getWithDefault", () => {
    module Test = {
      Belt.Option.getWithDefault(None, "Banana") /* Banana */

      Belt.Option.getWithDefault(Some("Apple"), "Banana") /* Apple */

      let greet = (firstName: option<string>) =>
        "Greetings " ++ firstName->Belt.Option.getWithDefault("Anonymous")

      Some("Jane")->greet /* "Greetings Jane" */

      None->greet /* "Greetings Anonymous" */
    }
    ()
  })
})

describe("Belt.Option.flatMap", () => {
  test("Belt.Option.flatMap", () => {
    module Test = {
      let addIfAboveOne = value =>
        if value > 1 {
          Some(value + 1)
        } else {
          None
        }

      Belt.Option.flatMap(Some(2), addIfAboveOne) /* Some(3) */

      Belt.Option.flatMap(Some(-4), addIfAboveOne) /* None */

      Belt.Option.flatMap(None, addIfAboveOne) /* None */
    }
    ()
  })
})

describe("Belt.Option.map", () => {
  test("Belt.Option.map", () => {
    module Test = {
      Belt.Option.map(Some(3), x => x * x) /* Some(9) */

      Belt.Option.map(None, x => x * x) /* None */
    }
    ()
  })
})

describe("Belt.Option.mapWithDefault", () => {
  test("Belt.Option.mapWithDefault", () => {
    module Test = {
      let someValue = Some(3)
      someValue->Belt.Option.mapWithDefault(0, x => x + 5) /* 8 */

      let noneValue = None
      noneValue->Belt.Option.mapWithDefault(0, x => x + 5) /* 0 */
    }
    ()
  })
})

describe("Belt.Option.getExn", () => {
  test("Belt.Option.getExn", () => {
    module Test = {
      Some(3)
      ->Belt.Option.getExn
      ->assertEqual(3)

      switch Belt.Option.getExn(None) {
      // Raises an exception
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Belt.Option.forEach", () => {
  test("Belt.Option.forEach", () => {
    module Test = {
      Belt.Option.forEach(Some("thing"), x => Js.log(x)) /* logs "thing" */
      Belt.Option.forEach(None, x => Js.log(x)) /* returns () */
    }
    ()
  })
})

describe("Belt.Option.keep", () => {
  test("Belt.Option.keep", () => {
    module Test = {
      Belt.Option.keep(Some(10), x => x > 5) /* returns `Some(10)` */
      Belt.Option.keep(Some(4), x => x > 5) /* returns `None` */
      Belt.Option.keep(None, x => x > 5) /* returns `None` */
    }
    ()
  })
})

describe("Belt.HashMap.logStats", () => {
  test("Belt.HashMap.logStats", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })
      let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(hMap, 1, "1")

      Belt.HashMap.logStats(hMap)
    }
    ()
  })
})

describe("Belt.HashMap.getBucketHistogram", () => {
  test("Belt.HashMap.getBucketHistogram", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })
      let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(hMap, 1, "1")

      Belt.HashMap.getBucketHistogram(hMap)
    }
    ()
  })
})

describe("Belt.HashMap.mergeMany", () => {
  test("Belt.HashMap.mergeMany", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.mergeMany(hMap, [(1, "1"), (2, "2")])
    }
    ()
  })
})

describe("Belt.HashMap.fromArray", () => {
  test("Belt.HashMap.fromArray", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.fromArray([(1, "value1"), (2, "value2")], ~id=module(IntHash))
      Belt.HashMap.toArray(s0) == [(1, "value1"), (2, "value2")]
    }
    ()
  })
})

describe("Belt.HashMap.valuesToArray", () => {
  test("Belt.HashMap.valuesToArray", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.set(s0, 2, "value2")

      Belt.HashMap.valuesToArray(s0) == ["value1", "value2"]
    }
    ()
  })
})

describe("Belt.HashMap.keysToArray", () => {
  test("Belt.HashMap.keysToArray", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.set(s0, 2, "value2")

      Belt.HashMap.keysToArray(s0) == [1, 2]
    }
    ()
  })
})

describe("Belt.HashMap.toArray", () => {
  test("Belt.HashMap.toArray", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.set(s0, 2, "value2")

      Belt.HashMap.toArray(s0) == [(1, "value1"), (2, "value2")]
    }
    ()
  })
})

describe("Belt.HashMap.size", () => {
  test("Belt.HashMap.size", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.set(s0, 2, "value2")

      Belt.HashMap.size(s0) == 2
    }
    ()
  })
})

describe("Belt.HashMap.keepMapInPlace", () => {
  test("Belt.HashMap.keepMapInPlace", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.set(s0, 2, "value2")

      Belt.HashMap.keepMapInPlace(s0, (key, value) => key == 1 ? None : Some(value))
    }
    ()
  })
})

describe("Belt.HashMap.reduce", () => {
  test("Belt.HashMap.reduce", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))

      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.set(s0, 2, "value2")

      s0
      ->Belt.HashMap.reduce("", (acc, _, value) => acc ++ (", " ++ value))
      ->assertEqual(", value1, value2")

      Console.log("lol")
    }
    ()
  })
})

describe("Belt.HashMap.forEach", () => {
  test("Belt.HashMap.forEach", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.forEach(s0, (key, value) => Js.log2(key, value))
      // prints (1, "value1")
    }
    ()
  })
})

describe("Belt.HashMap.remove", () => {
  test("Belt.HashMap.remove", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")
      Belt.HashMap.remove(s0, 1)
      Belt.HashMap.has(s0, 1) == false
    }
    ()
  })
})

describe("Belt.HashMap.has", () => {
  test("Belt.HashMap.has", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")

      Belt.HashMap.has(s0, 1) == true
      Belt.HashMap.has(s0, 2) == false
    }
    ()
  })
})

describe("Belt.HashMap.get", () => {
  test("Belt.HashMap.get", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))
      Belt.HashMap.set(s0, 1, "value1")

      Belt.HashMap.get(s0, 1) == Some("value1")
      Belt.HashMap.get(s0, 2) == None
    }
    ()
  })
})

describe("Belt.HashMap.copy", () => {
  test("Belt.HashMap.copy", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntHash))
      let s1 = Belt.HashMap.copy(s0)

      Belt.HashMap.set(s0, 2, "3")

      Belt.HashMap.get(s0, 2) != Belt.HashMap.get(s1, 2)
    }
    ()
  })
})

describe("Belt.HashMap.set", () => {
  test("Belt.HashMap.set", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let s0 = Belt.HashMap.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntHash))

      Belt.HashMap.set(s0, 2, "3")

      Belt.HashMap.valuesToArray(s0) == ["1", "3", "3"]
    }
    ()
  })
})

describe("Belt.HashMap.isEmpty", () => {
  test("Belt.HashMap.isEmpty", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      Belt.HashMap.isEmpty(Belt.HashMap.fromArray([(1, "1")], ~id=module(IntHash))) == false
    }
    ()
  })
})

describe("Belt.HashMap.clear", () => {
  test("Belt.HashMap.clear", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let hMap = Belt.HashMap.fromArray([(1, "1")], ~id=module(IntHash))
      Belt.HashMap.clear(hMap)
      Belt.HashMap.isEmpty(hMap) == true
    }
    ()
  })
})

describe("Belt.HashMap.make", () => {
  test("Belt.HashMap.make", () => {
    module Test = {
      module IntHash = Belt.Id.MakeHashable({
        type t = int
        let hash = a => a
        let eq = (a, b) => a == b
      })

      let hMap = Belt.HashMap.make(~hintSize=10, ~id=module(IntHash))

      Belt.HashMap.set(hMap, 0, "a")
    }
    ()
  })
})

describe("Belt.MutableSet.split", () => {
  test("Belt.MutableSet.split", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))

      let ((smaller, larger), present) = s0->Belt.MutableSet.split(3)

      present /* true */
      smaller->Belt.MutableSet.toArray /* [1,2] */
      larger->Belt.MutableSet.toArray /* [4,5] */
    }
    ()
  })
})

describe("Belt.MutableSet.get", () => {
  test("Belt.MutableSet.get", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.get(3) /* Some(3) */
      s0->Belt.MutableSet.get(20) /* None */
    }
    ()
  })
})

describe("Belt.MutableSet.maxUndefined", () => {
  test("Belt.MutableSet.maxUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.make(~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.maxUndefined /* undefined */
      s1->Belt.MutableSet.maxUndefined /* 5 */
    }
    ()
  })
})

describe("Belt.MutableSet.maximum", () => {
  test("Belt.MutableSet.maximum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.make(~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.maximum /* None */
      s1->Belt.MutableSet.maximum /* Some(5) */
    }
    ()
  })
})

describe("Belt.MutableSet.minUndefined", () => {
  test("Belt.MutableSet.minUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.make(~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.minUndefined /* undefined */
      s1->Belt.MutableSet.minUndefined /* 1 */
    }
    ()
  })
})

describe("Belt.MutableSet.minimum", () => {
  test("Belt.MutableSet.minimum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.make(~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.minimum /* None */
      s1->Belt.MutableSet.minimum /* Some(1) */
    }
    ()
  })
})

describe("Belt.MutableSet.toArray", () => {
  test("Belt.MutableSet.toArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.toArray /* [1,2,3,5] */
    }
    ()
  })
})

describe("Belt.MutableSet.toList", () => {
  test("Belt.MutableSet.toList", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.toList /* [1,2,3,5] */
    }
    ()
  })
})

describe("Belt.MutableSet.size", () => {
  test("Belt.MutableSet.size", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4], ~id=module(IntCmp))

      s0->Belt.MutableSet.size /* 4 */
    }
    ()
  })
})

describe("Belt.MutableSet.partition", () => {
  test("Belt.MutableSet.partition", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))
      let (s1, s2) = s0->Belt.MutableSet.partition(isOdd)

      s1->Belt.MutableSet.toArray /* [1,3,5] */
      s2->Belt.MutableSet.toArray /* [2,4] */
    }
    ()
  })
})

describe("Belt.MutableSet.keep", () => {
  test("Belt.MutableSet.keep", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))
      let s1 = s0->Belt.MutableSet.keep(isEven)

      s1->Belt.MutableSet.toArray /* [2, 4] */
    }
    ()
  })
})

describe("Belt.MutableSet.some", () => {
  test("Belt.MutableSet.some", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.MutableSet.fromArray([1, 2, 4, 6, 8], ~id=module(IntCmp))
      s0->Belt.MutableSet.some(isOdd) /* true */
    }
    ()
  })
})

describe("Belt.MutableSet.every", () => {
  test("Belt.MutableSet.every", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.MutableSet.fromArray([2, 4, 6, 8], ~id=module(IntCmp))
      s0->Belt.MutableSet.every(isEven) /* true */
    }
    ()
  })
})

describe("Belt.MutableSet.reduce", () => {
  test("Belt.MutableSet.reduce", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      s0->Belt.MutableSet.reduce(
        list{},
        (acc, element) => acc->Belt.List.add(element),
      ) /* [6,5,3,2] */
    }
    ()
  })
})

describe("Belt.MutableSet.forEach", () => {
  test("Belt.MutableSet.forEach", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let acc = ref(list{})
      s0->Belt.MutableSet.forEach(x => acc := Belt.List.add(acc.contents, x))
      acc /* [6,5,3,2] */
    }
    ()
  })
})

describe("Belt.MutableSet.eq", () => {
  test("Belt.MutableSet.eq", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3], ~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([3, 2, 5], ~id=module(IntCmp))

      Belt.MutableSet.eq(s0, s1) /* true */
    }
    ()
  })
})

describe("Belt.MutableSet.subset", () => {
  test("Belt.MutableSet.subset", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
      let s2 = Belt.MutableSet.intersect(s0, s1)
      Belt.MutableSet.subset(s2, s0) /* true */
      Belt.MutableSet.subset(s2, s1) /* true */
      Belt.MutableSet.subset(s1, s0) /* false */
    }
    ()
  })
})

describe("Belt.MutableSet.diff", () => {
  test("Belt.MutableSet.diff", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
      Belt.MutableSet.toArray(Belt.MutableSet.diff(s0, s1)) /* [6] */
      Belt.MutableSet.toArray(Belt.MutableSet.diff(s1, s0)) /* [1,4] */
    }
    ()
  })
})

describe("Belt.MutableSet.intersect", () => {
  test("Belt.MutableSet.intersect", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
      let intersect = Belt.MutableSet.intersect(s0, s1)
      intersect->Belt.MutableSet.toArray /* [2,3,5] */
    }
    ()
  })
})

describe("Belt.MutableSet.union", () => {
  test("Belt.MutableSet.union", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
      let union = Belt.MutableSet.union(s0, s1)
      union->Belt.MutableSet.toArray /* [1,2,3,4,5,6] */
    }
    ()
  })
})

describe("Belt.MutableSet.removeMany", () => {
  test("Belt.MutableSet.removeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.MutableSet.fromArray([1, 2, 3, 4], ~id=module(IntCmp))

      set->Belt.MutableSet.removeMany([5, 4, 3, 2, 1])
      set->Belt.MutableSet.toArray /* [] */
    }
    ()
  })
})

describe("Belt.MutableSet.remove", () => {
  test("Belt.MutableSet.remove", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([2, 3, 1, 4, 5], ~id=module(IntCmp))
      s0->Belt.MutableSet.remove(1)
      s0->Belt.MutableSet.remove(3)
      s0->Belt.MutableSet.remove(3)

      s0->Belt.MutableSet.toArray /* [2,4,5] */
    }
    ()
  })
})

describe("Belt.MutableSet.mergeMany", () => {
  test("Belt.MutableSet.mergeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.MutableSet.make(~id=module(IntCmp))

      set->Belt.MutableSet.mergeMany([5, 4, 3, 2, 1])
      set->Belt.MutableSet.toArray /* [1, 2, 3, 4, 5] */
    }
    ()
  })
})

describe("Belt.MutableSet.add", () => {
  test("Belt.MutableSet.add", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.make(~id=module(IntCmp))
      s0->Belt.MutableSet.add(1)
      s0->Belt.MutableSet.add(2)
      s0->Belt.MutableSet.add(2)

      s0->Belt.MutableSet.toArray /* [1, 2] */
    }
    ()
  })
})

describe("Belt.MutableSet.has", () => {
  test("Belt.MutableSet.has", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.MutableSet.fromArray([1, 4, 2, 5], ~id=module(IntCmp))

      set->Belt.MutableSet.has(3) /* false */
      set->Belt.MutableSet.has(1) /* true */
    }
    ()
  })
})

describe("Belt.MutableSet.isEmpty", () => {
  test("Belt.MutableSet.isEmpty", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let empty = Belt.MutableSet.fromArray([], ~id=module(IntCmp))
      let notEmpty = Belt.MutableSet.fromArray([1], ~id=module(IntCmp))

      Belt.MutableSet.isEmpty(empty) /* true */
      Belt.MutableSet.isEmpty(notEmpty) /* false */
    }
    ()
  })
})

describe("Belt.MutableSet.copy", () => {
  test("Belt.MutableSet.copy", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([1, 3, 2, 4], ~id=module(IntCmp))

      let copied = s0->Belt.MutableSet.copy
      copied->Belt.MutableSet.toArray /* [1, 2, 3, 4] */
    }
    ()
  })
})

describe("Belt.MutableSet.fromArray", () => {
  test("Belt.MutableSet.fromArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([1, 3, 2, 4], ~id=module(IntCmp))

      s0->Belt.MutableSet.toArray /* [1, 2, 3, 4] */
    }
    ()
  })
})

describe("Belt.Map.set", () => {
  test("Belt.Map.set", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      let s0 = Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))

      let s1 = Belt.Map.set(s0, 2, "3")

      Belt.Map.valuesToArray(s1) == ["1", "3", "3"]
    }
    ()
  })
})

describe("Belt.Map.remove", () => {
  test("Belt.Map.remove", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      let s0 = Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))

      let s1 = Belt.Map.remove(s0, 1)

      let s2 = Belt.Map.remove(s1, 1)

      s1 === s2

      Belt.Map.keysToArray(s1) == [2, 3]
    }
    ()
  })
})

describe("Belt.Map.get", () => {
  test("Belt.Map.get", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.get(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp)), 2) ==
        Some("2")

      Belt.Map.get(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp)), 2) ==
        None
    }
    ()
  })
})

describe("Belt.Map.valuesToArray", () => {
  test("Belt.Map.valuesToArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.valuesToArray(
        Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp)),
      ) == ["1", "2", "3"]
    }
    ()
  })
})

describe("Belt.Map.keysToArray", () => {
  test("Belt.Map.keysToArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.keysToArray(
        Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp)),
      ) == [1, 2, 3]
    }
    ()
  })
})

describe("Belt.Map.fromArray", () => {
  test("Belt.Map.fromArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.toArray(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))) == [
          (1, "1"),
          (2, "2"),
          (3, "3"),
        ]
    }
    ()
  })
})

describe("Belt.Map.toArray", () => {
  test("Belt.Map.toArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.toArray(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))) == [
          (1, "1"),
          (2, "2"),
          (3, "3"),
        ]
    }
    ()
  })
})

describe("Belt.Map.size", () => {
  test("Belt.Map.size", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.size(Belt.Map.fromArray([(2, "2"), (2, "1"), (3, "3")], ~id=module(IntCmp))) == 2
    }
    ()
  })
})

describe("Belt.Map.reduce", () => {
  test("Belt.Map.reduce", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      let s0 = Belt.Map.fromArray(~id=module(IntCmp), [(4, "4"), (1, "1"), (2, "2"), (3, "3")])

      Belt.Map.reduce(
        s0,
        list{},
        (acc, k, v) => list{(k, v), ...acc},
      ) /* [(4, "4"), (3, "3"), (2, "2"), (1, "1"), 0] */
    }
    ()
  })
})

describe("Belt.Map.forEach", () => {
  test("Belt.Map.forEach", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      let s0 = Belt.Map.fromArray(~id=module(IntCmp), [(4, "4"), (1, "1"), (2, "2"), (3, "")])

      let acc = ref(list{})

      Belt.Map.forEach(s0, (k, v) => acc := list{(k, v), ...acc.contents})

      acc.contents == list{(4, "4"), (3, "3"), (2, "2"), (1, "1")}
    }
    ()
  })
})

describe("Belt.Map.findFirstBy", () => {
  test("Belt.Map.findFirstBy", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      let s0 = Belt.Map.fromArray(~id=module(IntCmp), [(4, "4"), (1, "1"), (2, "2"), (3, "")])

      s0
      ->Belt.Map.findFirstBy((k, _) => k == 4)
      ->assertEqual(Some(4, "4"))
    }
    ()
  })
})

describe("Belt.Map.has", () => {
  test("Belt.Map.has", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.has(Belt.Map.fromArray([(1, "1")], ~id=module(IntCmp)), 1) == true
    }
    ()
  })
})

describe("Belt.Map.isEmpty", () => {
  test("Belt.Map.isEmpty", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.isEmpty(Belt.Map.fromArray([(1, "1")], ~id=module(IntCmp))) == false
    }
    ()
  })
})

describe("Belt.Map.make", () => {
  test("Belt.Map.make", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      let m = Belt.Map.make(~id=module(IntCmp))

      Belt.Map.set(m, 0, "a")
    }
    ()
  })
})

describe("Belt.Map.Int", () => {
  test("Belt.Map.Int", () => {
    module Test = {
      type t<'key, 'value, 'identity>
      type id<'key, 'id> = Belt_Id.comparable<'key, 'id>
    }
    ()
  })
})

describe("Belt.Set.split", () => {
  test("Belt.Set.split", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))

      let ((smaller, larger), present) = s0->Belt.Set.split(3)

      present->assertEqual(true)
      smaller->Belt.Set.toArray->assertEqual([1, 2])
      larger->Belt.Set.toArray->assertEqual([4, 5])
    }
    ()
  })
})

describe("Belt.Set.get", () => {
  test("Belt.Set.get", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))

      s0->Belt.Set.get(3)->assertEqual(Some(3))
      s0->Belt.Set.get(20)->assertEqual(None)
    }
    ()
  })
})

describe("Belt.Set.maxUndefined", () => {
  test("Belt.Set.maxUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.make(~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0
      ->Belt.Set.maxUndefined
      ->Js.Undefined.toOption
      ->assertEqual(None)

      s1
      ->Belt.Set.maxUndefined
      ->Js.Undefined.toOption
      ->assertEqual(Some(5))
    }
    ()
  })
})

describe("Belt.Set.maximum", () => {
  test("Belt.Set.maximum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.make(~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.Set.maximum->assertEqual(None)
      s1->Belt.Set.maximum->assertEqual(Some(5))
    }
    ()
  })
})

describe("Belt.Set.minUndefined", () => {
  test("Belt.Set.minUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.make(~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.Set.minUndefined->Js.Undefined.toOption->assertEqual(None)
      s1->Belt.Set.minUndefined->Js.Undefined.toOption->assertEqual(Some(1))
    }
    ()
  })
})

describe("Belt.Set.minimum", () => {
  test("Belt.Set.minimum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.make(~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.Set.minimum->assertEqual(None)
      s1->Belt.Set.minimum->assertEqual(Some(1))
    }
    ()
  })
})

describe("Belt.Set.toList", () => {
  test("Belt.Set.toList", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.Set.toList->assertEqual(list{1, 2, 3, 5})
    }
    ()
  })
})

describe("Belt.Set.toArray", () => {
  test("Belt.Set.toArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.Set.toArray->assertEqual([1, 2, 3, 5])
    }
    ()
  })
})

describe("Belt.Set.size", () => {
  test("Belt.Set.size", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([1, 2, 3, 4], ~id=module(IntCmp))

      s0->Belt.Set.size->assertEqual(4)
    }
    ()
  })
})

describe("Belt.Set.partition", () => {
  test("Belt.Set.partition", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.Set.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))
      let (s1, s2) = s0->Belt.Set.partition(isOdd)

      s1->Belt.Set.toArray->assertEqual([1, 3, 5])
      s2->Belt.Set.toArray->assertEqual([2, 4])
    }
    ()
  })
})

describe("Belt.Set.keep", () => {
  test("Belt.Set.keep", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.Set.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))
      let s1 = s0->Belt.Set.keep(isEven)

      s1->Belt.Set.toArray->assertEqual([2, 4])
    }
    ()
  })
})

describe("Belt.Set.some", () => {
  test("Belt.Set.some", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.Set.fromArray([1, 2, 4, 6, 8], ~id=module(IntCmp))
      s0->Belt.Set.some(isOdd)->assertEqual(true)
    }
    ()
  })
})

describe("Belt.Set.every", () => {
  test("Belt.Set.every", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.Set.fromArray([2, 4, 6, 8], ~id=module(IntCmp))
      s0->Belt.Set.every(isEven)->assertEqual(true)
    }
    ()
  })
})

describe("Belt.Set.reduce", () => {
  test("Belt.Set.reduce", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      s0
      ->Belt.Set.reduce(list{}, (acc, element) => acc->Belt.List.add(element))
      ->assertEqual(list{6, 5, 3, 2})
    }
    ()
  })
})

describe("Belt.Set.forEach", () => {
  test("Belt.Set.forEach", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))

      let acc = ref(list{})

      s0->Belt.Set.forEach(
        x => {
          acc := Belt.List.add(acc.contents, x)
        },
      )

      acc.contents->assertEqual(list{6, 5, 3, 2})
    }
    ()
  })
})

describe("Belt.Set.eq", () => {
  test("Belt.Set.eq", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3], ~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([3, 2, 5], ~id=module(IntCmp))

      Belt.Set.eq(s0, s1)->assertEqual(true)
    }
    ()
  })
})

describe("Belt.Set.subset", () => {
  test("Belt.Set.subset", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
      let s2 = Belt.Set.intersect(s0, s1)

      Belt.Set.subset(s2, s0)->assertEqual(true)
      Belt.Set.subset(s2, s1)->assertEqual(true)
      Belt.Set.subset(s1, s0)->assertEqual(false)
    }
    ()
  })
})

describe("Belt.Set.diff", () => {
  test("Belt.Set.diff", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))

      Belt.Set.diff(s0, s1)
      ->Belt.Set.toArray
      ->assertEqual([6])

      Belt.Set.diff(s1, s0)
      ->Belt.Set.toArray
      ->assertEqual([1, 4])
    }
    ()
  })
})

describe("Belt.Set.intersect", () => {
  test("Belt.Set.intersect", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))

      let intersect = Belt.Set.intersect(s0, s1)

      intersect
      ->Belt.Set.toArray
      ->assertEqual([2, 3, 5])
    }
    ()
  })
})

describe("Belt.Set.union", () => {
  test("Belt.Set.union", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
      let union = Belt.Set.union(s0, s1)

      union
      ->Belt.Set.toArray
      ->assertEqual([1, 2, 3, 4, 5, 6])
    }
    ()
  })
})

describe("Belt.Set.removeMany", () => {
  test("Belt.Set.removeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.fromArray([1, 2, 3, 4], ~id=module(IntCmp))

      let newSet = set->Belt.Set.removeMany([5, 4, 3, 2, 1])

      newSet
      ->Belt.Set.toArray
      ->assertEqual([])
    }
    ()
  })
})

describe("Belt.Set.remove", () => {
  test("Belt.Set.remove", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([2, 3, 1, 4, 5], ~id=module(IntCmp))
      let s1 = s0->Belt.Set.remove(1)
      let s2 = s1->Belt.Set.remove(3)
      let s3 = s2->Belt.Set.remove(3)

      s1->Belt.Set.toArray->assertEqual([2, 3, 4, 5])
      s2->Belt.Set.toArray->assertEqual([2, 4, 5])
      assertEqual(s2, s3)
    }
    ()
  })
})

describe("Belt.Set.mergeMany", () => {
  test("Belt.Set.mergeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.make(~id=module(IntCmp))

      let newSet = set->Belt.Set.mergeMany([5, 4, 3, 2, 1])

      newSet
      ->Belt.Set.toArray
      ->assertEqual([1, 2, 3, 4, 5])
    }
    ()
  })
})

describe("Belt.Set.add", () => {
  test("Belt.Set.add", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.make(~id=module(IntCmp))

      let s1 = s0->Belt.Set.add(1)
      let s2 = s1->Belt.Set.add(2)
      let s3 = s2->Belt.Set.add(2)

      s0->Belt.Set.toArray->assertEqual([])
      s1->Belt.Set.toArray->assertEqual([1])
      s2->Belt.Set.toArray->assertEqual([1, 2])
      s3->Belt.Set.toArray->assertEqual([1, 2])
      assertEqual(s2, s3)
    }
    ()
  })
})

describe("Belt.Set.has", () => {
  test("Belt.Set.has", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.fromArray([1, 4, 2, 5], ~id=module(IntCmp))

      set->Belt.Set.has(3)->assertEqual(false)
      set->Belt.Set.has(1)->assertEqual(true)
    }
    ()
  })
})

describe("Belt.Set.isEmpty", () => {
  test("Belt.Set.isEmpty", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let empty = Belt.Set.fromArray([], ~id=module(IntCmp))
      let notEmpty = Belt.Set.fromArray([1], ~id=module(IntCmp))

      Belt.Set.isEmpty(empty)->assertEqual(true)
      Belt.Set.isEmpty(notEmpty)->assertEqual(false)
    }
    ()
  })
})

describe("Belt.Set.fromArray", () => {
  test("Belt.Set.fromArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([1, 3, 2, 4], ~id=module(IntCmp))

      s0->Belt.Set.toArray->assertEqual([1, 2, 3, 4])
    }
    ()
  })
})

describe("Belt.Set.make", () => {
  test("Belt.Set.make", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.make(~id=module(IntCmp))

      Belt.Set.isEmpty(set)->assertEqual(true)
    }
    ()
  })
})

describe("Belt.Range.someBy", () => {
  test("Belt.Range.someBy", () => {
    module Test = {
      Belt.Range.someBy(1, 5, ~step=2, i => mod(i, 2) === 0) /* false */
      Belt.Range.someBy(0, 4, ~step=2, i => mod(i, 2) === 0) /* true */
    }
    ()
  })
})

describe("Belt.Range.some", () => {
  test("Belt.Range.some", () => {
    module Test = {
      Belt.Range.some(0, 4, i => i > 5) /* false */

      Belt.Range.some(0, 4, i => i > 2) /* true */
    }
    ()
  })
})

describe("Belt.Range.everyBy", () => {
  test("Belt.Range.everyBy", () => {
    module Test = {
      Belt.Range.everyBy(0, 4, ~step=1, i => mod(i, 2) === 0) /* false */

      Belt.Range.everyBy(0, 4, ~step=2, i => mod(i, 2) === 0) /* true */
    }
    ()
  })
})

describe("Belt.Range.every", () => {
  test("Belt.Range.every", () => {
    module Test = {
      Belt.Range.every(0, 4, i => i < 5) /* true */

      Belt.Range.every(0, 4, i => i < 4) /* false */
    }
    ()
  })
})

describe("Belt.Range.forEach", () => {
  test("Belt.Range.forEach", () => {
    module Test = {
      Belt.Range.forEach(0, 4, i => Js.log(i))

      // Prints:
      // 0
      // 1
      // 2
      // 3
      // 4
    }
    ()
  })
})

describe("Belt.List.sort", () => {
  test("Belt.List.sort", () => {
    module Test = {
      Belt.List.sort(list{5, 4, 9, 3, 7}, (a, b) => a - b) // list{3, 4, 5, 7, 9}
    }
    ()
  })
})

describe("Belt.List.setAssoc", () => {
  test("Belt.List.setAssoc", () => {
    module Test = {
      list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.setAssoc(
        2,
        "x",
        (a, b) => a == b,
      ) /* list{(1, "a"), (2, "x"), (3, "c")} */

      list{(1, "a"), (3, "c")}->Belt.List.setAssoc(
        2,
        "b",
        (a, b) => a == b,
      ) /* list{(2, "b"), (1, "a"), (3, "c")} */

      list{(9, "morning"), (3, "morning?!"), (22, "night")}->Belt.List.setAssoc(
        15,
        "afternoon",
        (a, b) => mod(a, 12) == mod(b, 12),
      )
      /* list{(9, "morning"), (15, "afternoon"), (22, "night")} */
    }
    ()
  })
})

describe("Belt.List.removeAssoc", () => {
  test("Belt.List.removeAssoc", () => {
    module Test = {
      list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.removeAssoc(
        1,
        (a, b) => a == b,
      ) /* list{(2, "b"), (3, "c")} */

      list{(9, "morning"), (15, "afternoon"), (22, "night")}->Belt.List.removeAssoc(
        9,
        (k, item) => k /* 9 */ == item /* 9, 5, 22 */,
      )
      /* list{(15, "afternoon"), (22, "night")} */
    }
    ()
  })
})

describe("Belt.List.hasAssoc", () => {
  test("Belt.List.hasAssoc", () => {
    module Test = {
      list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.hasAssoc(1, (a, b) => a == b) /* true */

      list{(9, "morning"), (15, "afternoon"), (22, "night")}->Belt.List.hasAssoc(
        25,
        (k, item) => k /* 25 */ == item /* 9, 5, 22 */,
      ) /* false */
    }
    ()
  })
})

describe("Belt.List.getAssoc", () => {
  test("Belt.List.getAssoc", () => {
    module Test = {
      list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.getAssoc(3, (a, b) => a == b) /* Some("c") */

      list{(9, "morning"), (15, "afternoon"), (22, "night")}->Belt.List.getAssoc(
        15,
        (k, item) => k /* 15 */ == item /* 9, 5, 22 */,
      )
      /* Some("afternoon") */
    }
    ()
  })
})

describe("Belt.List.unzip", () => {
  test("Belt.List.unzip", () => {
    module Test = {
      Belt.List.unzip(list{(1, 2), (3, 4)}) /* (list{1, 3}, list{2, 4}) */

      Belt.List.unzip(list{("H", "W"), ("e", "o"), ("l", "r"), ("l", "l"), ("o", "d"), (" ", "!")})
      /* (list{"H", "e", "l", "l", "o", " "}, list{"W", "o", "r", "l", "d", "!"}) */
    }
    ()
  })
})

describe("Belt.List.partition", () => {
  test("Belt.List.partition", () => {
    module Test = {
      list{1, 2, 3, 4}
      ->Belt.List.partition(x => x > 2)
      ->assertEqual((list{3, 4}, list{1, 2}))
    }
    ()
  })
})

describe("Belt.List.keepMap", () => {
  test("Belt.List.keepMap", () => {
    module Test = {
      let isEven = x => mod(x, 2) == 0

      list{1, 2, 3, 4}->Belt.List.keepMap(
        x =>
          if isEven(x) {
            Some(x)
          } else {
            None
          },
      ) /* list{2, 4} */

      list{Some(1), Some(2), None}->Belt.List.keepMap(x => x) /* list{1, 2} */
    }
    ()
  })
})

describe("Belt.List.filterWithIndex", () => {
  test("Belt.List.filterWithIndex", () => {
    module Test = {
      let isEven = x => mod(x, 2) == 0

      Belt.List.filterWithIndex(list{1, 2, 3, 4}, (_x, index) => isEven(index)) /* list{1, 3} */
    }
    ()
  })
})

describe("Belt.List.keepWithIndex", () => {
  test("Belt.List.keepWithIndex", () => {
    module Test = {
      let isEven = x => mod(x, 2) == 0

      Belt.List.keepWithIndex(list{1, 2, 3, 4}, (_x, index) => isEven(index)) /* list{1, 3} */
    }
    ()
  })
})

describe("Belt.List.filter", () => {
  test("Belt.List.filter", () => {
    module Test = {
      let isEven = x => mod(x, 2) == 0

      Belt.List.filter(list{1, 2, 3, 4}, isEven) /* list{2, 4} */

      Belt.List.filter(
        list{None, Some(2), Some(3), None},
        Belt.Option.isSome,
      ) /* list{Some(2), Some(3)} */
    }
    ()
  })
})

describe("Belt.List.keep", () => {
  test("Belt.List.keep", () => {
    module Test = {
      let isEven = x => mod(x, 2) == 0

      Belt.List.keep(list{1, 2, 3, 4}, isEven) /* list{2, 4} */

      Belt.List.keep(
        list{None, Some(2), Some(3), None},
        Belt.Option.isSome,
      ) /* list{Some(2), Some(3)} */
    }
    ()
  })
})

describe("Belt.List.getBy", () => {
  test("Belt.List.getBy", () => {
    module Test = {
      Belt.List.getBy(list{1, 4, 3, 2}, x => x > 3) /* Some(4) */

      Belt.List.getBy(list{1, 4, 3, 2}, x => x > 4) /* None */
    }
    ()
  })
})

describe("Belt.List.has", () => {
  test("Belt.List.has", () => {
    module Test = {
      list{1, 2, 3}->Belt.List.has(2, (a, b) => a == b) /* true */

      list{1, 2, 3}->Belt.List.has(4, (a, b) => a == b) /* false */

      list{-1, -2, -3}->Belt.List.has(2, (a, b) => abs(a) == abs(b)) /* true */
    }
    ()
  })
})

describe("Belt.List.eq", () => {
  test("Belt.List.eq", () => {
    module Test = {
      Belt.List.eq(list{1, 2, 3}, list{1, 2}, (a, b) => a == b) /* false */

      Belt.List.eq(list{1, 2}, list{1, 2}, (a, b) => a == b) /* true */

      Belt.List.eq(list{1, 2, 3}, list{-1, -2, -3}, (a, b) => abs(a) == abs(b)) /* true */
    }
    ()
  })
})

describe("Belt.List.cmp", () => {
  test("Belt.List.cmp", () => {
    module Test = {
      Belt.List.cmp(list{3}, list{3, 7}, (a, b) => compare(a, b)) /* (-1) */

      Belt.List.cmp(list{5, 3}, list{5}, (a, b) => compare(a, b)) /* 1 */

      Belt.List.cmp(list{1, 3, 5}, list{1, 4, 2}, (a, b) => compare(a, b)) /* (-1) */

      Belt.List.cmp(list{1, 3, 5}, list{1, 2, 3}, (a, b) => compare(a, b)) /* 1 */

      Belt.List.cmp(list{1, 3, 5}, list{1, 3, 5}, (a, b) => compare(a, b)) /* 0 */
    }
    ()
  })
})

describe("Belt.List.cmpByLength", () => {
  test("Belt.List.cmpByLength", () => {
    module Test = {
      Belt.List.cmpByLength(list{1, 2}, list{3, 4, 5, 6}) /* -1 */

      Belt.List.cmpByLength(list{1, 2, 3}, list{4, 5, 6}) /* = 0 */

      Belt.List.cmpByLength(list{1, 2, 3, 4}, list{5, 6}) /* = 1 */
    }
    ()
  })
})

describe("Belt.List.some2", () => {
  test("Belt.List.some2", () => {
    module Test = {
      Belt.List.some2(list{1, 2, 3}, list{0, 1}, (a, b) => a > b) /* true */

      Belt.List.some2(list{}, list{1}, (a, b) => a > b) /* false */

      Belt.List.some2(list{2, 3}, list{1}, (a, b) => a > b) /* true */

      Belt.List.some2(list{0, 1}, list{5, 0}, (a, b) => a > b) /* true */
    }
    ()
  })
})

describe("Belt.List.every2", () => {
  test("Belt.List.every2", () => {
    module Test = {
      Belt.List.every2(list{1, 2, 3}, list{0, 1}, (a, b) => a > b) /* true */

      Belt.List.every2(list{}, list{1}, (a, b) => a > b) /* true */

      Belt.List.every2(list{2, 3}, list{1}, (a, b) => a > b) /* true */

      Belt.List.every2(list{0, 1}, list{5, 0}, (a, b) => a > b) /* false */
    }
    ()
  })
})

describe("Belt.List.some", () => {
  test("Belt.List.some", () => {
    module Test = {
      let isAbove100 = value => value > 100

      list{101, 1, 2, 3}->Belt.List.some(isAbove100) /* true */

      list{1, 2, 3, 4}->Belt.List.some(isAbove100) /* false */
    }
    ()
  })
})

describe("Belt.List.every", () => {
  test("Belt.List.every", () => {
    module Test = {
      let isBelow10 = value => value < 10

      list{1, 9, 8, 2}->Belt.List.every(isBelow10) /* true */

      list{1, 99, 8, 2}->Belt.List.every(isBelow10) /* false */
    }
    ()
  })
})

describe("Belt.List.reduceReverse2", () => {
  test("Belt.List.reduceReverse2", () => {
    module Test = {
      Belt.List.reduceReverse2(
        list{1, 2, 3},
        list{4, 5},
        0,
        (acc, x, y) => acc + x * x + y,
      ) /* + (1 * 1 + 4) + (2 * 2 + 5) */
    }
    ()
  })
})

describe("Belt.List.reduce2", () => {
  test("Belt.List.reduce2", () => {
    module Test = {
      Belt.List.reduce2(
        list{1, 2, 3},
        list{4, 5},
        0,
        (acc, x, y) => acc + x * x + y,
      ) /* 0 + (1 * 1 + 4) + (2 * 2 + 5) */
    }
    ()
  })
})

describe("Belt.List.forEach2", () => {
  test("Belt.List.forEach2", () => {
    module Test = {
      Belt.List.forEach2(list{"Z", "Y"}, list{"A", "B", "C"}, (x, y) => Js.log2(x, y))

      /*
  prints:
  "Z" "A"
  "Y" "B"
*/
    }
    ()
  })
})

describe("Belt.List.mapReverse2", () => {
  test("Belt.List.mapReverse2", () => {
    module Test = {
      Belt.List.mapReverse2(list{1, 2, 3}, list{1, 2}, (a, b) => a + b) // list{4, 2}
    }
    ()
  })
})

describe("Belt.List.reduceReverse", () => {
  test("Belt.List.reduceReverse", () => {
    module Test = {
      list{1, 2, 3, 4}->Belt.List.reduceReverse(0, (a, b) => a + b) /* 10 */

      list{1, 2, 3, 4}->Belt.List.reduceReverse(10, (a, b) => a - b) /* 0 */

      list{1, 2, 3, 4}->Belt.List.reduceReverse(list{}, Belt.List.add) // list{1, 2, 3, 4}
    }
    ()
  })
})

describe("Belt.List.reduceWithIndex", () => {
  test("Belt.List.reduceWithIndex", () => {
    module Test = {
      list{1, 2, 3, 4}->Belt.List.reduceWithIndex(
        0,
        (acc, item, index) => acc + item + index,
      ) /* 16 */
    }
    ()
  })
})

describe("Belt.List.reduce", () => {
  test("Belt.List.reduce", () => {
    module Test = {
      list{1, 2, 3, 4}->Belt.List.reduce(0, (a, b) => a + b) /* 10 */

      /* same as */

      list{1, 2, 3, 4}->Belt.List.reduce(0, (acc, item) => acc + item) /* 10 */
    }
    ()
  })
})

describe("Belt.List.forEachWithIndex", () => {
  test("Belt.List.forEachWithIndex", () => {
    module Test = {
      Belt.List.forEachWithIndex(
        list{"a", "b", "c"},
        (index, x) => {
          Js.log("Item " ++ Belt.Int.toString(index) ++ " is " ++ x)
        },
      )
      /*
  prints:
  Item 0 is a
  Item 1 is b
  Item 2 is cc
*/
    }
    ()
  })
})

describe("Belt.List.forEach", () => {
  test("Belt.List.forEach", () => {
    module Test = {
      Belt.List.forEach(list{"a", "b", "c"}, x => Js.log("Item: " ++ x))
      /*
  prints:
  Item: a
  Item: b
  Item: c
*/
    }
    ()
  })
})

describe("Belt.List.mapReverse", () => {
  test("Belt.List.mapReverse", () => {
    module Test = {
      list{3, 4, 5}
      ->Belt.List.mapReverse(x => x * x)
      ->assertEqual(list{25, 16, 9})
    }
    ()
  })
})

describe("Belt.List.reverse", () => {
  test("Belt.List.reverse", () => {
    module Test = {
      Belt.List.reverse(list{1, 2, 3}) /* list{3, 2, 1} */
    }
    ()
  })
})

describe("Belt.List.toArray", () => {
  test("Belt.List.toArray", () => {
    module Test = {
      Belt.List.toArray(list{1, 2, 3}) // [1, 2, 3]
    }
    ()
  })
})

describe("Belt.List.fromArray", () => {
  test("Belt.List.fromArray", () => {
    module Test = {
      Belt.List.fromArray([1, 2, 3]) // list{1, 2, 3}
    }
    ()
  })
})

describe("Belt.List.mapWithIndex", () => {
  test("Belt.List.mapWithIndex", () => {
    module Test = {
      list{1, 2, 3}->Belt.List.mapWithIndex((index, x) => index + x) // list{1, 3, 5}
    }
    ()
  })
})

describe("Belt.List.zipBy", () => {
  test("Belt.List.zipBy", () => {
    module Test = {
      Belt.List.zipBy(list{1, 2, 3}, list{4, 5}, (a, b) => 2 * a + b) // list{6, 9}
    }
    ()
  })
})

describe("Belt.List.zip", () => {
  test("Belt.List.zip", () => {
    module Test = {
      Belt.List.zip(list{1, 2}, list{3, 4, 5}) // list{(1, 3), (2, 4)}
    }
    ()
  })
})

describe("Belt.List.map", () => {
  test("Belt.List.map", () => {
    module Test = {
      list{1, 2}->Belt.List.map(x => x + 1) // list{3, 4}
    }
    ()
  })
})

describe("Belt.List.flatten", () => {
  test("Belt.List.flatten", () => {
    module Test = {
      Belt.List.flatten(list{list{1, 2, 3}, list{}, list{3}}) // list{1, 2, 3, 3}
    }
    ()
  })
})

describe("Belt.List.reverseConcat", () => {
  test("Belt.List.reverseConcat", () => {
    module Test = {
      Belt.List.reverseConcat(list{1, 2}, list{3, 4}) // list{2, 1, 3, 4}
    }
    ()
  })
})

describe("Belt.List.concatMany", () => {
  test("Belt.List.concatMany", () => {
    module Test = {
      Belt.List.concatMany([list{1, 2, 3}, list{}, list{3}]) // list{1, 2, 3, 3}
    }
    ()
  })
})

describe("Belt.List.concat", () => {
  test("Belt.List.concat", () => {
    module Test = {
      Belt.List.concat(list{1, 2, 3}, list{4, 5}) // list{1, 2, 3, 4, 5}
    }
    ()
  })
})

describe("Belt.List.splitAt", () => {
  test("Belt.List.splitAt", () => {
    module Test = {
      list{"Hello", "World"}->Belt.List.splitAt(1) // Some((list{"Hello"}, list{"World"}))

      list{0, 1, 2, 3, 4}->Belt.List.splitAt(2) // Some((list{0, 1}, list{2, 3, 4}))
    }
    ()
  })
})

describe("Belt.List.take", () => {
  test("Belt.List.take", () => {
    module Test = {
      list{1, 2, 3}->Belt.List.take(1) // Some(list{1})

      list{1, 2, 3}->Belt.List.take(2) // Some(list{1, 2})

      list{1, 2, 3}->Belt.List.take(4) // None
    }
    ()
  })
})

describe("Belt.List.drop", () => {
  test("Belt.List.drop", () => {
    module Test = {
      list{1, 2, 3}->Belt.List.drop(2) // Some(list{3})

      list{1, 2, 3}->Belt.List.drop(3) // Some(list{})

      list{1, 2, 3}->Belt.List.drop(4) // None
    }
    ()
  })
})

describe("Belt.List.shuffle", () => {
  test("Belt.List.shuffle", () => {
    module Test = {
      Belt.List.shuffle(list{1, 2, 3}) // list{2, 1, 3}
    }
    ()
  })
})

describe("Belt.List.makeBy", () => {
  test("Belt.List.makeBy", () => {
    module Test = {
      Belt.List.makeBy(5, i => i) // list{0, 1, 2, 3, 4}

      Belt.List.makeBy(5, i => i * i) // list{0, 1, 4, 9, 16}
    }
    ()
  })
})

describe("Belt.List.make", () => {
  test("Belt.List.make", () => {
    module Test = {
      Belt.List.make(3, 1) // list{1, 1, 1}
    }
    ()
  })
})

describe("Belt.List.getExn", () => {
  test("Belt.List.getExn", () => {
    module Test = {
      let abc = list{"A", "B", "C"}

      abc->Belt.List.getExn(1)->assertEqual("B")

      switch abc->Belt.List.getExn(4) {
      // Raises an Error
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Belt.List.get", () => {
  test("Belt.List.get", () => {
    module Test = {
      let abc = list{"A", "B", "C"}

      abc->Belt.List.get(1) // Some("B")

      abc->Belt.List.get(4) // None
    }
    ()
  })
})

describe("Belt.List.add", () => {
  test("Belt.List.add", () => {
    module Test = {
      Belt.List.add(list{2, 3}, 1) // list{1, 2, 3}

      Belt.List.add(list{"World", "!"}, "Hello") // list{"Hello", "World", "!"}
    }
    ()
  })
})

describe("Belt.List.tailExn", () => {
  test("Belt.List.tailExn", () => {
    module Test = {
      Belt.List.tailExn(list{1, 2, 3})->assertEqual(list{2, 3})

      switch Belt.List.tailExn(list{}) {
      // Raises an Error
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Belt.List.tail", () => {
  test("Belt.List.tail", () => {
    module Test = {
      Belt.List.tail(list{1, 2, 3}) // Some(list{2, 3})

      Belt.List.tail(list{}) // None
    }
    ()
  })
})

describe("Belt.List.headExn", () => {
  test("Belt.List.headExn", () => {
    module Test = {
      Belt.List.headExn(list{1, 2, 3})->assertEqual(1)

      switch Belt.List.headExn(list{}) {
      // Raises an Error
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Belt.List.head", () => {
  test("Belt.List.head", () => {
    module Test = {
      Belt.List.head(list{}) // None
      Belt.List.head(list{1, 2, 3}) // Some(1)
    }
    ()
  })
})

describe("Belt.List.length", () => {
  test("Belt.List.length", () => {
    module Test = {
      Belt.List.length(list{1, 2, 3}) // 3
    }
    ()
  })
})

describe("Belt.SortArray.binarySearchBy", () => {
  test("Belt.SortArray.binarySearchBy", () => {
    module Test = {
      Belt.SortArray.binarySearchBy([1, 2, 3, 4, 33, 35, 36], 33, Pervasives.compare) == 4

      lnot(Belt.SortArray.binarySearchBy([1, 3, 5, 7], 4, Pervasives.compare)) == 2
    }
    ()
  })
})

describe("Belt.SortArray.strictlySortedLength", () => {
  test("Belt.SortArray.strictlySortedLength", () => {
    module Test = {
      Belt.SortArray.strictlySortedLength([1, 2, 3, 4, 3], (x, y) => x < y) == 4

      Belt.SortArray.strictlySortedLength([], (x, y) => x < y) == 0

      Belt.SortArray.strictlySortedLength([1], (x, y) => x < y) == 1

      Belt.SortArray.strictlySortedLength([4, 3, 2, 1], (x, y) => x < y) == -4
    }
    ()
  })
})

describe("Belt.Array.truncateToLengthUnsafe", () => {
  test("Belt.Array.truncateToLengthUnsafe", () => {
    module Test = {
      let arr = ["ant", "bee", "cat", "dog", "elk"]

      Belt.Array.truncateToLengthUnsafe(arr, 3)

      arr == ["ant", "bee", "cat"]
    }
    ()
  })
})

describe("Belt.Array.eq", () => {
  test("Belt.Array.eq", () => {
    module Test = {
      Belt.Array.eq([1, 2, 3], [-1, -2, -3], (a, b) => abs(a) == abs(b)) == true
    }
    ()
  })
})

describe("Belt.Array.cmp", () => {
  test("Belt.Array.cmp", () => {
    module Test = {
      Belt.Array.cmp([1, 3, 5], [1, 4, 2], (a, b) => compare(a, b)) == -1

      Belt.Array.cmp([1, 3, 5], [1, 2, 3], (a, b) => compare(a, b)) == 1

      Belt.Array.cmp([1, 3, 5], [1, 3, 5], (a, b) => compare(a, b)) == 0
    }
    ()
  })
})

describe("Belt.Array.some2", () => {
  test("Belt.Array.some2", () => {
    module Test = {
      Belt.Array.some2([0, 2], [1, 0, 3], (a, b) => a > b) == true

      Belt.Array.some2([], [1], (x, y) => x > y) == false

      Belt.Array.some2([2, 3], [1, 4], (x, y) => x > y) == true
    }
    ()
  })
})

describe("Belt.Array.every2", () => {
  test("Belt.Array.every2", () => {
    module Test = {
      Belt.Array.every2([1, 2, 3], [0, 1], (a, b) => a > b) == true

      Belt.Array.every2([], [1], (x, y) => x > y) == true

      Belt.Array.every2([2, 3], [1], (x, y) => x > y) == true

      Belt.Array.every2([0, 1], [5, 0], (x, y) => x > y) == false
    }
    ()
  })
})

describe("Belt.Array.every", () => {
  test("Belt.Array.every", () => {
    module Test = {
      Belt.Array.every([1, 3, 5], x => mod(x, 2) == 1) == true

      Belt.Array.every([1, -3, 5], x => x > 0) == false
    }
    ()
  })
})

describe("Belt.Array.some", () => {
  test("Belt.Array.some", () => {
    module Test = {
      Belt.Array.some([2, 3, 4], x => mod(x, 2) == 1) == true

      Belt.Array.some([-1, -3, -5], x => x > 0) == false
    }
    ()
  })
})

describe("Belt.Array.joinWith", () => {
  test("Belt.Array.joinWith", () => {
    module Test = {
      Belt.Array.joinWith([0, 1], ", ", Js.Int.toString) == "0, 1"
      Belt.Array.joinWith([], " ", Js.Int.toString) == ""
      Belt.Array.joinWith([1], " ", Js.Int.toString) == "1"
    }
    ()
  })
})

describe("Belt.Array.reduceWithIndex", () => {
  test("Belt.Array.reduceWithIndex", () => {
    module Test = {
      Belt.Array.reduceWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i) == 16
    }
    ()
  })
})

describe("Belt.Array.reduceReverse2", () => {
  test("Belt.Array.reduceReverse2", () => {
    module Test = {
      Belt.Array.reduceReverse2([1, 2, 3], [1, 2], 0, (acc, x, y) => acc + x + y) == 6
    }
    ()
  })
})

describe("Belt.Array.reduceReverse", () => {
  test("Belt.Array.reduceReverse", () => {
    module Test = {
      Belt.Array.reduceReverse(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "dcba"
    }
    ()
  })
})

describe("Belt.Array.reduce", () => {
  test("Belt.Array.reduce", () => {
    module Test = {
      Belt.Array.reduce([2, 3, 4], 1, (a, b) => a + b) == 10

      Belt.Array.reduce(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "abcd"
    }
    ()
  })
})

describe("Belt.Array.partition", () => {
  test("Belt.Array.partition", () => {
    module Test = {
      Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) == 0) == ([2, 4], [1, 3, 5])

      Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) != 0) == ([1, 3, 5], [2, 4])
    }
    ()
  })
})

describe("Belt.Array.mapWithIndex", () => {
  test("Belt.Array.mapWithIndex", () => {
    module Test = {
      Belt.Array.mapWithIndex([1, 2, 3], (i, x) => i + x) == [0 + 1, 1 + 2, 2 + 3]
    }
    ()
  })
})

describe("Belt.Array.forEachWithIndex", () => {
  test("Belt.Array.forEachWithIndex", () => {
    module Test = {
      Belt.Array.forEachWithIndex(
        ["a", "b", "c"],
        (i, x) => Js.log("Item " ++ Belt.Int.toString(i) ++ " is " ++ x),
      )

      /*
  prints:
  Item 0 is a
  Item 1 is b
  Item 2 is cc
*/
      let total = ref(0)

      Belt.Array.forEachWithIndex([10, 11, 12, 13], (i, x) => total := total.contents + x + i)

      total.contents == 0 + 10 + 1 + 11 + 2 + 12 + 3 + 13
    }
    ()
  })
})

describe("Belt.Array.keepMap", () => {
  test("Belt.Array.keepMap", () => {
    module Test = {
      Belt.Array.keepMap(
        [1, 2, 3],
        x =>
          if mod(x, 2) == 0 {
            Some(x)
          } else {
            None
          },
      ) == [2]
    }
    ()
  })
})

describe("Belt.Array.keepWithIndex", () => {
  test("Belt.Array.keepWithIndex", () => {
    module Test = {
      Belt.Array.keepWithIndex([1, 2, 3], (_x, i) => i == 1) == [2]
    }
    ()
  })
})

describe("Belt.Array.getIndexBy", () => {
  test("Belt.Array.getIndexBy", () => {
    module Test = {
      Belt.Array.getIndexBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(1)
      Belt.Array.getIndexBy([15, 13, 11], x => mod(x, 2) == 0) == None
    }
    ()
  })
})

describe("Belt.Array.getBy", () => {
  test("Belt.Array.getBy", () => {
    module Test = {
      Belt.Array.getBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(4)
      Belt.Array.getBy([15, 13, 11], x => mod(x, 2) == 0) == None
    }
    ()
  })
})

describe("Belt.Array.flatMap", () => {
  test("Belt.Array.flatMap", () => {
    module Test = {
      Belt.Array.flatMap([1, 2], x => [x + 10, x + 20]) == [11, 21, 12, 22]
    }
    ()
  })
})

describe("Belt.Array.map", () => {
  test("Belt.Array.map", () => {
    module Test = {
      Belt.Array.map([1, 2], x => x + 1) == [3, 4]
    }
    ()
  })
})

describe("Belt.Array.forEach", () => {
  test("Belt.Array.forEach", () => {
    module Test = {
      Belt.Array.forEach(["a", "b", "c"], x => Js.log("Item: " ++ x))

      /*
  prints:
  Item: a
  Item: b
  Item: c
*/
      let total = ref(0)

      Belt.Array.forEach([1, 2, 3, 4], x => total := total.contents + x)

      total.contents == 1 + 2 + 3 + 4
    }
    ()
  })
})

describe("Belt.Array.blit", () => {
  test("Belt.Array.blit", () => {
    module Test = {
      let v1 = [10, 11, 12, 13, 14, 15, 16, 17]
      let v2 = [20, 21, 22, 23, 24, 25, 26, 27]

      Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v2, ~dstOffset=2, ~len=3)
      v2 == [20, 21, 14, 15, 16, 25, 26, 27]

      Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v1, ~dstOffset=2, ~len=3)
      v1 == [10, 11, 14, 15, 16, 15, 16, 17]
    }
    ()
  })
})

describe("Belt.Array.fill", () => {
  test("Belt.Array.fill", () => {
    module Test = {
      let arr = Belt.Array.makeBy(5, i => i)

      Belt.Array.fill(arr, ~offset=2, ~len=2, 9)

      arr == [0, 1, 9, 9, 4]

      Belt.Array.fill(arr, ~offset=7, ~len=2, 8)

      arr == [0, 1, 9, 9, 4]
    }
    ()
  })
})

describe("Belt.Array.sliceToEnd", () => {
  test("Belt.Array.sliceToEnd", () => {
    module Test = {
      Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], 2) == [12, 13, 14, 15, 16]

      Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], -4) == [13, 14, 15, 16]
    }
    ()
  })
})

describe("Belt.Array.slice", () => {
  test("Belt.Array.slice", () => {
    module Test = {
      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=2, ~len=3) == [12, 13, 14]

      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=-4, ~len=3) == [13, 14, 15]

      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=4, ~len=9) == [14, 15, 16]
    }
    ()
  })
})

describe("Belt.Array.concatMany", () => {
  test("Belt.Array.concatMany", () => {
    module Test = {
      Belt.Array.concatMany([[1, 2, 3], [4, 5, 6], [7, 8]]) == [1, 2, 3, 4, 5, 6, 7, 8]
    }
    ()
  })
})

describe("Belt.Array.concat", () => {
  test("Belt.Array.concat", () => {
    module Test = {
      Belt.Array.concat([1, 2, 3], [4, 5]) == [1, 2, 3, 4, 5]

      Belt.Array.concat([], ["a", "b", "c"]) == ["a", "b", "c"]
    }
    ()
  })
})

describe("Belt.Array.unzip", () => {
  test("Belt.Array.unzip", () => {
    module Test = {
      Belt.Array.unzip([(1, 2), (3, 4)]) == ([1, 3], [2, 4])

      Belt.Array.unzip([(1, 2), (3, 4), (5, 6), (7, 8)]) == ([1, 3, 5, 7], [2, 4, 6, 8])
    }
    ()
  })
})

describe("Belt.Array.zipBy", () => {
  test("Belt.Array.zipBy", () => {
    module Test = {
      Belt.Array.zipBy([1, 2, 3], [4, 5], (a, b) => 2 * a + b) == [6, 9]
    }
    ()
  })
})

describe("Belt.Array.zip", () => {
  test("Belt.Array.zip", () => {
    module Test = {
      Belt.Array.zip([1, 2], [3, 4, 5]) == [(1, 3), (2, 4)]
    }
    ()
  })
})

describe("Belt.Array.makeBy", () => {
  test("Belt.Array.makeBy", () => {
    module Test = {
      Belt.Array.makeBy(5, i => i) == [0, 1, 2, 3, 4]

      Belt.Array.makeBy(5, i => i * i) == [0, 1, 4, 9, 16]
    }
    ()
  })
})

describe("Belt.Array.rangeBy", () => {
  test("Belt.Array.rangeBy", () => {
    module Test = {
      Belt.Array.rangeBy(0, 10, ~step=3) == [0, 3, 6, 9]

      Belt.Array.rangeBy(0, 12, ~step=3) == [0, 3, 6, 9, 12]

      Belt.Array.rangeBy(33, 0, ~step=1) == []

      Belt.Array.rangeBy(33, 0, ~step=-1) == []

      Belt.Array.rangeBy(3, 12, ~step=-1) == []

      Belt.Array.rangeBy(3, 3, ~step=0) == []

      Belt.Array.rangeBy(3, 3, ~step=1) == [3]
    }
    ()
  })
})

describe("Belt.Array.range", () => {
  test("Belt.Array.range", () => {
    module Test = {
      Belt.Array.range(0, 3) == [0, 1, 2, 3]

      Belt.Array.range(3, 0) == []

      Belt.Array.range(3, 3) == [3]
    }
    ()
  })
})

describe("Belt.Array.makeUninitializedUnsafe", () => {
  test("Belt.Array.makeUninitializedUnsafe", () => {
    module Test = {
      let arr = Belt.Array.makeUninitializedUnsafe(5)

      Js.log(Belt.Array.getExn(arr, 0)) // undefined

      Belt.Array.setExn(arr, 0, "example")

      Js.log(Belt.Array.getExn(arr, 0) == "example")
    }
    ()
  })
})

describe("Belt.Array.makeUninitialized", () => {
  test("Belt.Array.makeUninitialized", () => {
    module Test = {
      let arr: array<Js.undefined<string>> = Belt.Array.makeUninitialized(5)

      Belt.Array.getExn(arr, 0) == Js.undefined
    }
    ()
  })
})

describe("Belt.Array.reverse", () => {
  test("Belt.Array.reverse", () => {
    module Test = {
      Belt.Array.reverse([10, 11, 12, 13, 14]) == [14, 13, 12, 11, 10]
    }
    ()
  })
})

describe("Belt.Array.reverseInPlace", () => {
  test("Belt.Array.reverseInPlace", () => {
    module Test = {
      let arr = [10, 11, 12, 13, 14]

      let () = Belt.Array.reverseInPlace(arr)

      arr == [14, 13, 12, 11, 10]
    }
    ()
  })
})

describe("Belt.Array.get", () => {
  test("Belt.Array.get", () => {
    module Test = {
      Belt.Array.get(["a", "b", "c"], 0) == Some("a")
      Belt.Array.get(["a", "b", "c"], 3) == None
      Belt.Array.get(["a", "b", "c"], -1) == None
    }
    ()
  })
})

describe("Belt.Array.length", () => {
  test("Belt.Array.length", () => {
    module Test = {
      // Returns 1
      Belt.Array.length(["test"])
    }
    ()
  })
})

describe("Belt.Option", () => {
  test("Belt.Option", () => {
    module Test = {
      type option<'a> = None | Some('a)

      let someString: option<string> = Some("hello")
    }
    ()
  })
})

describe("Belt.HashMap", () => {
  test("Belt.HashMap", () => {
    module Test = {
      type t = int
      module I0 = unpack(Belt.Id.hashable(~hash=(_: t) => 0xff_ff, ~eq=(a, b) => a == b))
      let s0: Belt.HashMap.t<I0.t, int, I0.identity> = Belt.HashMap.make(
        ~hintSize=40,
        ~id=module(I0),
      )

      module I1 = unpack(Belt.Id.hashable(~hash=(_: t) => 0xff, ~eq=(a, b) => a == b))
      let s1: Belt.HashMap.t<I1.t, string, I1.identity> = Belt.HashMap.make(
        ~hintSize=40,
        ~id=module(I1),
      )

      let () = {
        Belt.HashMap.set(s0, 0, 3)
        Belt.HashMap.set(s1, 1, "3")
      }
    }
    ()
  })
})

describe("Belt.HashSet", () => {
  test("Belt.HashSet", () => {
    module Test = {
      module I0 = unpack(Belt.Id.hashable(~hash=(a: int) => land(a, 65535), ~eq=(a, b) => a == b))

      let s0 = Belt.HashSet.make(~id=module(I0), ~hintSize=40)

      module I1 = unpack(Belt.Id.hashable(~hash=(a: int) => land(a, 255), ~eq=(a, b) => a == b))

      let s1 = Belt.HashSet.make(~id=module(I1), ~hintSize=40)

      Belt.HashSet.add(s1, 0)
      Belt.HashSet.add(s1, 1)
    }
    ()
  })
})

describe("Belt.MutableSet", () => {
  test("Belt.MutableSet", () => {
    module Test = {
      module PairComparator = Belt.Id.MakeComparable({
        type t = (int, int)
        let cmp = ((a0, a1), (b0, b1)) =>
          switch Pervasives.compare(a0, b0) {
          | 0 => Pervasives.compare(a1, b1)
          | c => c
          }
      })

      let mySet = Belt.MutableSet.make(~id=module(PairComparator))
      mySet->Belt.MutableSet.add((1, 2))
    }
    ()
  })
})

describe("Belt.Set", () => {
  test("Belt.Set", () => {
    module Test = {
      module PairComparator = Belt.Id.MakeComparable({
        type t = (int, int)
        let cmp = ((a0, a1), (b0, b1)) =>
          switch Pervasives.compare(a0, b0) {
          | 0 => Pervasives.compare(a1, b1)
          | c => c
          }
      })

      let mySet = Belt.Set.make(~id=module(PairComparator))
      let mySet2 = Belt.Set.add(mySet, (1, 2))

      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })
    }
    ()
  })
})

describe("Belt_Int./", () => {
  test("Belt_Int./", () => {
    module Test = {
      open Belt.Int
      assertEqual(4 / 2, 2)
    }
    ()
  })
})

describe("Belt_Int.*", () => {
  test("Belt_Int.*", () => {
    module Test = {
      open Belt.Int
      assertEqual(2 * 2, 4)
    }
    ()
  })
})

describe("Belt_Int.-", () => {
  test("Belt_Int.-", () => {
    module Test = {
      open Belt.Int
      assertEqual(2 - 1, 1)
    }
    ()
  })
})

describe("Belt_Int.+", () => {
  test("Belt_Int.+", () => {
    module Test = {
      open Belt.Int
      assertEqual(2 + 2, 4)
    }
    ()
  })
})

describe("Belt_Int.toString", () => {
  test("Belt_Int.toString", () => {
    module Test = {
      Belt.Int.toString(1)->assertEqual("1")
    }
    ()
  })
})

describe("Belt_Int.fromString", () => {
  test("Belt_Int.fromString", () => {
    module Test = {
      Belt.Int.fromString("1")->assertEqual(Some(1))
    }
    ()
  })
})

describe("Belt_Int.fromFloat", () => {
  test("Belt_Int.fromFloat", () => {
    module Test = {
      Belt.Int.fromFloat(1.0)->assertEqual(1)
    }
    ()
  })
})

describe("Belt_Int.toFloat", () => {
  test("Belt_Int.toFloat", () => {
    module Test = {
      Belt.Int.toFloat(1)->assertEqual(1.0)
    }
    ()
  })
})

describe("Belt_List.sort", () => {
  test("Belt_List.sort", () => {
    module Test = {
      Belt.List.sort(list{5, 4, 9, 3, 7}, (a, b) => a - b) // list{3, 4, 5, 7, 9}
    }
    ()
  })
})

describe("Belt_List.setAssoc", () => {
  test("Belt_List.setAssoc", () => {
    module Test = {
      list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.setAssoc(
        2,
        "x",
        (a, b) => a == b,
      ) /* list{(1, "a"), (2, "x"), (3, "c")} */

      list{(1, "a"), (3, "c")}->Belt.List.setAssoc(
        2,
        "b",
        (a, b) => a == b,
      ) /* list{(2, "b"), (1, "a"), (3, "c")} */

      list{(9, "morning"), (3, "morning?!"), (22, "night")}->Belt.List.setAssoc(
        15,
        "afternoon",
        (a, b) => mod(a, 12) == mod(b, 12),
      )
      /* list{(9, "morning"), (15, "afternoon"), (22, "night")} */
    }
    ()
  })
})

describe("Belt_List.removeAssoc", () => {
  test("Belt_List.removeAssoc", () => {
    module Test = {
      list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.removeAssoc(
        1,
        (a, b) => a == b,
      ) /* list{(2, "b"), (3, "c")} */

      list{(9, "morning"), (15, "afternoon"), (22, "night")}->Belt.List.removeAssoc(
        9,
        (k, item) => k /* 9 */ == item /* 9, 5, 22 */,
      )
      /* list{(15, "afternoon"), (22, "night")} */
    }
    ()
  })
})

describe("Belt_List.hasAssoc", () => {
  test("Belt_List.hasAssoc", () => {
    module Test = {
      list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.hasAssoc(1, (a, b) => a == b) /* true */

      list{(9, "morning"), (15, "afternoon"), (22, "night")}->Belt.List.hasAssoc(
        25,
        (k, item) => k /* 25 */ == item /* 9, 5, 22 */,
      ) /* false */
    }
    ()
  })
})

describe("Belt_List.getAssoc", () => {
  test("Belt_List.getAssoc", () => {
    module Test = {
      list{(1, "a"), (2, "b"), (3, "c")}->Belt.List.getAssoc(3, (a, b) => a == b) /* Some("c") */

      list{(9, "morning"), (15, "afternoon"), (22, "night")}->Belt.List.getAssoc(
        15,
        (k, item) => k /* 15 */ == item /* 9, 5, 22 */,
      )
      /* Some("afternoon") */
    }
    ()
  })
})

describe("Belt_List.unzip", () => {
  test("Belt_List.unzip", () => {
    module Test = {
      Belt.List.unzip(list{(1, 2), (3, 4)}) /* (list{1, 3}, list{2, 4}) */

      Belt.List.unzip(list{("H", "W"), ("e", "o"), ("l", "r"), ("l", "l"), ("o", "d"), (" ", "!")})
      /* (list{"H", "e", "l", "l", "o", " "}, list{"W", "o", "r", "l", "d", "!"}) */
    }
    ()
  })
})

describe("Belt_List.partition", () => {
  test("Belt_List.partition", () => {
    module Test = {
      list{1, 2, 3, 4}
      ->Belt.List.partition(x => x > 2)
      ->assertEqual((list{3, 4}, list{1, 2}))
    }
    ()
  })
})

describe("Belt_List.keepMap", () => {
  test("Belt_List.keepMap", () => {
    module Test = {
      let isEven = x => mod(x, 2) == 0

      list{1, 2, 3, 4}->Belt.List.keepMap(
        x =>
          if isEven(x) {
            Some(x)
          } else {
            None
          },
      ) /* list{2, 4} */

      list{Some(1), Some(2), None}->Belt.List.keepMap(x => x) /* list{1, 2} */
    }
    ()
  })
})

describe("Belt_List.filterWithIndex", () => {
  test("Belt_List.filterWithIndex", () => {
    module Test = {
      let isEven = x => mod(x, 2) == 0

      Belt.List.filterWithIndex(list{1, 2, 3, 4}, (_x, index) => isEven(index)) /* list{1, 3} */
    }
    ()
  })
})

describe("Belt_List.keepWithIndex", () => {
  test("Belt_List.keepWithIndex", () => {
    module Test = {
      let isEven = x => mod(x, 2) == 0

      Belt.List.keepWithIndex(list{1, 2, 3, 4}, (_x, index) => isEven(index)) /* list{1, 3} */
    }
    ()
  })
})

describe("Belt_List.filter", () => {
  test("Belt_List.filter", () => {
    module Test = {
      let isEven = x => mod(x, 2) == 0

      Belt.List.filter(list{1, 2, 3, 4}, isEven) /* list{2, 4} */

      Belt.List.filter(
        list{None, Some(2), Some(3), None},
        Belt.Option.isSome,
      ) /* list{Some(2), Some(3)} */
    }
    ()
  })
})

describe("Belt_List.keep", () => {
  test("Belt_List.keep", () => {
    module Test = {
      let isEven = x => mod(x, 2) == 0

      Belt.List.keep(list{1, 2, 3, 4}, isEven) /* list{2, 4} */

      Belt.List.keep(
        list{None, Some(2), Some(3), None},
        Belt.Option.isSome,
      ) /* list{Some(2), Some(3)} */
    }
    ()
  })
})

describe("Belt_List.getBy", () => {
  test("Belt_List.getBy", () => {
    module Test = {
      Belt.List.getBy(list{1, 4, 3, 2}, x => x > 3) /* Some(4) */

      Belt.List.getBy(list{1, 4, 3, 2}, x => x > 4) /* None */
    }
    ()
  })
})

describe("Belt_List.has", () => {
  test("Belt_List.has", () => {
    module Test = {
      list{1, 2, 3}->Belt.List.has(2, (a, b) => a == b) /* true */

      list{1, 2, 3}->Belt.List.has(4, (a, b) => a == b) /* false */

      list{-1, -2, -3}->Belt.List.has(2, (a, b) => abs(a) == abs(b)) /* true */
    }
    ()
  })
})

describe("Belt_List.eq", () => {
  test("Belt_List.eq", () => {
    module Test = {
      Belt.List.eq(list{1, 2, 3}, list{1, 2}, (a, b) => a == b) /* false */

      Belt.List.eq(list{1, 2}, list{1, 2}, (a, b) => a == b) /* true */

      Belt.List.eq(list{1, 2, 3}, list{-1, -2, -3}, (a, b) => abs(a) == abs(b)) /* true */
    }
    ()
  })
})

describe("Belt_List.cmp", () => {
  test("Belt_List.cmp", () => {
    module Test = {
      Belt.List.cmp(list{3}, list{3, 7}, (a, b) => compare(a, b)) /* (-1) */

      Belt.List.cmp(list{5, 3}, list{5}, (a, b) => compare(a, b)) /* 1 */

      Belt.List.cmp(list{1, 3, 5}, list{1, 4, 2}, (a, b) => compare(a, b)) /* (-1) */

      Belt.List.cmp(list{1, 3, 5}, list{1, 2, 3}, (a, b) => compare(a, b)) /* 1 */

      Belt.List.cmp(list{1, 3, 5}, list{1, 3, 5}, (a, b) => compare(a, b)) /* 0 */
    }
    ()
  })
})

describe("Belt_List.cmpByLength", () => {
  test("Belt_List.cmpByLength", () => {
    module Test = {
      Belt.List.cmpByLength(list{1, 2}, list{3, 4, 5, 6}) /* -1 */

      Belt.List.cmpByLength(list{1, 2, 3}, list{4, 5, 6}) /* = 0 */

      Belt.List.cmpByLength(list{1, 2, 3, 4}, list{5, 6}) /* = 1 */
    }
    ()
  })
})

describe("Belt_List.some2", () => {
  test("Belt_List.some2", () => {
    module Test = {
      Belt.List.some2(list{1, 2, 3}, list{0, 1}, (a, b) => a > b) /* true */

      Belt.List.some2(list{}, list{1}, (a, b) => a > b) /* false */

      Belt.List.some2(list{2, 3}, list{1}, (a, b) => a > b) /* true */

      Belt.List.some2(list{0, 1}, list{5, 0}, (a, b) => a > b) /* true */
    }
    ()
  })
})

describe("Belt_List.every2", () => {
  test("Belt_List.every2", () => {
    module Test = {
      Belt.List.every2(list{1, 2, 3}, list{0, 1}, (a, b) => a > b) /* true */

      Belt.List.every2(list{}, list{1}, (a, b) => a > b) /* true */

      Belt.List.every2(list{2, 3}, list{1}, (a, b) => a > b) /* true */

      Belt.List.every2(list{0, 1}, list{5, 0}, (a, b) => a > b) /* false */
    }
    ()
  })
})

describe("Belt_List.some", () => {
  test("Belt_List.some", () => {
    module Test = {
      let isAbove100 = value => value > 100

      list{101, 1, 2, 3}->Belt.List.some(isAbove100) /* true */

      list{1, 2, 3, 4}->Belt.List.some(isAbove100) /* false */
    }
    ()
  })
})

describe("Belt_List.every", () => {
  test("Belt_List.every", () => {
    module Test = {
      let isBelow10 = value => value < 10

      list{1, 9, 8, 2}->Belt.List.every(isBelow10) /* true */

      list{1, 99, 8, 2}->Belt.List.every(isBelow10) /* false */
    }
    ()
  })
})

describe("Belt_List.reduceReverse2", () => {
  test("Belt_List.reduceReverse2", () => {
    module Test = {
      Belt.List.reduceReverse2(
        list{1, 2, 3},
        list{4, 5},
        0,
        (acc, x, y) => acc + x * x + y,
      ) /* + (1 * 1 + 4) + (2 * 2 + 5) */
    }
    ()
  })
})

describe("Belt_List.reduce2", () => {
  test("Belt_List.reduce2", () => {
    module Test = {
      Belt.List.reduce2(
        list{1, 2, 3},
        list{4, 5},
        0,
        (acc, x, y) => acc + x * x + y,
      ) /* 0 + (1 * 1 + 4) + (2 * 2 + 5) */
    }
    ()
  })
})

describe("Belt_List.forEach2", () => {
  test("Belt_List.forEach2", () => {
    module Test = {
      Belt.List.forEach2(list{"Z", "Y"}, list{"A", "B", "C"}, (x, y) => Js.log2(x, y))

      /*
  prints:
  "Z" "A"
  "Y" "B"
*/
    }
    ()
  })
})

describe("Belt_List.mapReverse2", () => {
  test("Belt_List.mapReverse2", () => {
    module Test = {
      Belt.List.mapReverse2(list{1, 2, 3}, list{1, 2}, (a, b) => a + b) // list{4, 2}
    }
    ()
  })
})

describe("Belt_List.reduceReverse", () => {
  test("Belt_List.reduceReverse", () => {
    module Test = {
      list{1, 2, 3, 4}->Belt.List.reduceReverse(0, (a, b) => a + b) /* 10 */

      list{1, 2, 3, 4}->Belt.List.reduceReverse(10, (a, b) => a - b) /* 0 */

      list{1, 2, 3, 4}->Belt.List.reduceReverse(list{}, Belt.List.add) // list{1, 2, 3, 4}
    }
    ()
  })
})

describe("Belt_List.reduceWithIndex", () => {
  test("Belt_List.reduceWithIndex", () => {
    module Test = {
      list{1, 2, 3, 4}->Belt.List.reduceWithIndex(
        0,
        (acc, item, index) => acc + item + index,
      ) /* 16 */
    }
    ()
  })
})

describe("Belt_List.reduce", () => {
  test("Belt_List.reduce", () => {
    module Test = {
      list{1, 2, 3, 4}->Belt.List.reduce(0, (a, b) => a + b) /* 10 */

      /* same as */

      list{1, 2, 3, 4}->Belt.List.reduce(0, (acc, item) => acc + item) /* 10 */
    }
    ()
  })
})

describe("Belt_List.forEachWithIndex", () => {
  test("Belt_List.forEachWithIndex", () => {
    module Test = {
      Belt.List.forEachWithIndex(
        list{"a", "b", "c"},
        (index, x) => {
          Js.log("Item " ++ Belt.Int.toString(index) ++ " is " ++ x)
        },
      )
      /*
  prints:
  Item 0 is a
  Item 1 is b
  Item 2 is cc
*/
    }
    ()
  })
})

describe("Belt_List.forEach", () => {
  test("Belt_List.forEach", () => {
    module Test = {
      Belt.List.forEach(list{"a", "b", "c"}, x => Js.log("Item: " ++ x))
      /*
  prints:
  Item: a
  Item: b
  Item: c
*/
    }
    ()
  })
})

describe("Belt_List.mapReverse", () => {
  test("Belt_List.mapReverse", () => {
    module Test = {
      list{3, 4, 5}
      ->Belt.List.mapReverse(x => x * x)
      ->assertEqual(list{25, 16, 9})
    }
    ()
  })
})

describe("Belt_List.reverse", () => {
  test("Belt_List.reverse", () => {
    module Test = {
      Belt.List.reverse(list{1, 2, 3}) /* list{3, 2, 1} */
    }
    ()
  })
})

describe("Belt_List.toArray", () => {
  test("Belt_List.toArray", () => {
    module Test = {
      Belt.List.toArray(list{1, 2, 3}) // [1, 2, 3]
    }
    ()
  })
})

describe("Belt_List.fromArray", () => {
  test("Belt_List.fromArray", () => {
    module Test = {
      Belt.List.fromArray([1, 2, 3]) // list{1, 2, 3}
    }
    ()
  })
})

describe("Belt_List.mapWithIndex", () => {
  test("Belt_List.mapWithIndex", () => {
    module Test = {
      list{1, 2, 3}->Belt.List.mapWithIndex((index, x) => index + x) // list{1, 3, 5}
    }
    ()
  })
})

describe("Belt_List.zipBy", () => {
  test("Belt_List.zipBy", () => {
    module Test = {
      Belt.List.zipBy(list{1, 2, 3}, list{4, 5}, (a, b) => 2 * a + b) // list{6, 9}
    }
    ()
  })
})

describe("Belt_List.zip", () => {
  test("Belt_List.zip", () => {
    module Test = {
      Belt.List.zip(list{1, 2}, list{3, 4, 5}) // list{(1, 3), (2, 4)}
    }
    ()
  })
})

describe("Belt_List.map", () => {
  test("Belt_List.map", () => {
    module Test = {
      list{1, 2}->Belt.List.map(x => x + 1) // list{3, 4}
    }
    ()
  })
})

describe("Belt_List.flatten", () => {
  test("Belt_List.flatten", () => {
    module Test = {
      Belt.List.flatten(list{list{1, 2, 3}, list{}, list{3}}) // list{1, 2, 3, 3}
    }
    ()
  })
})

describe("Belt_List.reverseConcat", () => {
  test("Belt_List.reverseConcat", () => {
    module Test = {
      Belt.List.reverseConcat(list{1, 2}, list{3, 4}) // list{2, 1, 3, 4}
    }
    ()
  })
})

describe("Belt_List.concatMany", () => {
  test("Belt_List.concatMany", () => {
    module Test = {
      Belt.List.concatMany([list{1, 2, 3}, list{}, list{3}]) // list{1, 2, 3, 3}
    }
    ()
  })
})

describe("Belt_List.concat", () => {
  test("Belt_List.concat", () => {
    module Test = {
      Belt.List.concat(list{1, 2, 3}, list{4, 5}) // list{1, 2, 3, 4, 5}
    }
    ()
  })
})

describe("Belt_List.splitAt", () => {
  test("Belt_List.splitAt", () => {
    module Test = {
      list{"Hello", "World"}->Belt.List.splitAt(1) // Some((list{"Hello"}, list{"World"}))

      list{0, 1, 2, 3, 4}->Belt.List.splitAt(2) // Some((list{0, 1}, list{2, 3, 4}))
    }
    ()
  })
})

describe("Belt_List.take", () => {
  test("Belt_List.take", () => {
    module Test = {
      list{1, 2, 3}->Belt.List.take(1) // Some(list{1})

      list{1, 2, 3}->Belt.List.take(2) // Some(list{1, 2})

      list{1, 2, 3}->Belt.List.take(4) // None
    }
    ()
  })
})

describe("Belt_List.drop", () => {
  test("Belt_List.drop", () => {
    module Test = {
      list{1, 2, 3}->Belt.List.drop(2) // Some(list{3})

      list{1, 2, 3}->Belt.List.drop(3) // Some(list{})

      list{1, 2, 3}->Belt.List.drop(4) // None
    }
    ()
  })
})

describe("Belt_List.shuffle", () => {
  test("Belt_List.shuffle", () => {
    module Test = {
      Belt.List.shuffle(list{1, 2, 3}) // list{2, 1, 3}
    }
    ()
  })
})

describe("Belt_List.makeBy", () => {
  test("Belt_List.makeBy", () => {
    module Test = {
      Belt.List.makeBy(5, i => i) // list{0, 1, 2, 3, 4}

      Belt.List.makeBy(5, i => i * i) // list{0, 1, 4, 9, 16}
    }
    ()
  })
})

describe("Belt_List.make", () => {
  test("Belt_List.make", () => {
    module Test = {
      Belt.List.make(3, 1) // list{1, 1, 1}
    }
    ()
  })
})

describe("Belt_List.getExn", () => {
  test("Belt_List.getExn", () => {
    module Test = {
      let abc = list{"A", "B", "C"}

      abc->Belt.List.getExn(1)->assertEqual("B")

      switch abc->Belt.List.getExn(4) {
      // Raises an Error
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Belt_List.get", () => {
  test("Belt_List.get", () => {
    module Test = {
      let abc = list{"A", "B", "C"}

      abc->Belt.List.get(1) // Some("B")

      abc->Belt.List.get(4) // None
    }
    ()
  })
})

describe("Belt_List.add", () => {
  test("Belt_List.add", () => {
    module Test = {
      Belt.List.add(list{2, 3}, 1) // list{1, 2, 3}

      Belt.List.add(list{"World", "!"}, "Hello") // list{"Hello", "World", "!"}
    }
    ()
  })
})

describe("Belt_List.tailExn", () => {
  test("Belt_List.tailExn", () => {
    module Test = {
      Belt.List.tailExn(list{1, 2, 3})->assertEqual(list{2, 3})

      switch Belt.List.tailExn(list{}) {
      // Raises an Error
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Belt_List.tail", () => {
  test("Belt_List.tail", () => {
    module Test = {
      Belt.List.tail(list{1, 2, 3}) // Some(list{2, 3})

      Belt.List.tail(list{}) // None
    }
    ()
  })
})

describe("Belt_List.headExn", () => {
  test("Belt_List.headExn", () => {
    module Test = {
      Belt.List.headExn(list{1, 2, 3})->assertEqual(1)

      switch Belt.List.headExn(list{}) {
      // Raises an Error
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Belt_List.head", () => {
  test("Belt_List.head", () => {
    module Test = {
      Belt.List.head(list{}) // None
      Belt.List.head(list{1, 2, 3}) // Some(1)
    }
    ()
  })
})

describe("Belt_List.length", () => {
  test("Belt_List.length", () => {
    module Test = {
      Belt.List.length(list{1, 2, 3}) // 3
    }
    ()
  })
})

describe("Belt_Map.Dict.findFirstBy", () => {
  test("Belt_Map.Dict.findFirstBy", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Map.Dict.fromArray([(4, "4"), (1, "1"), (2, "2"), (3, "3")], ~cmp=IntCmp.cmp)

      Belt.Map.Dict.findFirstBy(s0, (k, _) => k == 4) == Some((4, "4"))
    }
    ()
  })
})

describe("Belt_Map.String.findFirstBy", () => {
  test("Belt_Map.String.findFirstBy", () => {
    module Test = {
      let mapString = Belt.Map.String.fromArray([("1", "one"), ("2", "two"), ("3", "three")])

      mapString
      ->Belt.Map.String.findFirstBy((k, v) => k == "1" && v == "one")
      ->assertEqual(Some("1", "one"))
    }
    ()
  })
})

describe("Belt_Map.Int.findFirstBy", () => {
  test("Belt_Map.Int.findFirstBy", () => {
    module Test = {
      let mapInt = Belt.Map.Int.fromArray([(1, "one"), (2, "two"), (3, "three")])

      mapInt
      ->Belt.Map.Int.findFirstBy((k, v) => k == 1 && v == "one")
      ->assertEqual(Some(1, "one"))
    }
    ()
  })
})

describe("Belt_Map.set", () => {
  test("Belt_Map.set", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      let s0 = Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))

      let s1 = Belt.Map.set(s0, 2, "3")

      Belt.Map.valuesToArray(s1) == ["1", "3", "3"]
    }
    ()
  })
})

describe("Belt_Map.remove", () => {
  test("Belt_Map.remove", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      let s0 = Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))

      let s1 = Belt.Map.remove(s0, 1)

      let s2 = Belt.Map.remove(s1, 1)

      s1 === s2

      Belt.Map.keysToArray(s1) == [2, 3]
    }
    ()
  })
})

describe("Belt_Map.get", () => {
  test("Belt_Map.get", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.get(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp)), 2) ==
        Some("2")

      Belt.Map.get(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp)), 2) ==
        None
    }
    ()
  })
})

describe("Belt_Map.valuesToArray", () => {
  test("Belt_Map.valuesToArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.valuesToArray(
        Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp)),
      ) == ["1", "2", "3"]
    }
    ()
  })
})

describe("Belt_Map.keysToArray", () => {
  test("Belt_Map.keysToArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.keysToArray(
        Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp)),
      ) == [1, 2, 3]
    }
    ()
  })
})

describe("Belt_Map.fromArray", () => {
  test("Belt_Map.fromArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.toArray(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))) == [
          (1, "1"),
          (2, "2"),
          (3, "3"),
        ]
    }
    ()
  })
})

describe("Belt_Map.toArray", () => {
  test("Belt_Map.toArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.toArray(Belt.Map.fromArray([(2, "2"), (1, "1"), (3, "3")], ~id=module(IntCmp))) == [
          (1, "1"),
          (2, "2"),
          (3, "3"),
        ]
    }
    ()
  })
})

describe("Belt_Map.size", () => {
  test("Belt_Map.size", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.size(Belt.Map.fromArray([(2, "2"), (2, "1"), (3, "3")], ~id=module(IntCmp))) == 2
    }
    ()
  })
})

describe("Belt_Map.reduce", () => {
  test("Belt_Map.reduce", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      let s0 = Belt.Map.fromArray(~id=module(IntCmp), [(4, "4"), (1, "1"), (2, "2"), (3, "3")])

      Belt.Map.reduce(
        s0,
        list{},
        (acc, k, v) => list{(k, v), ...acc},
      ) /* [(4, "4"), (3, "3"), (2, "2"), (1, "1"), 0] */
    }
    ()
  })
})

describe("Belt_Map.forEach", () => {
  test("Belt_Map.forEach", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      let s0 = Belt.Map.fromArray(~id=module(IntCmp), [(4, "4"), (1, "1"), (2, "2"), (3, "")])

      let acc = ref(list{})

      Belt.Map.forEach(s0, (k, v) => acc := list{(k, v), ...acc.contents})

      acc.contents == list{(4, "4"), (3, "3"), (2, "2"), (1, "1")}
    }
    ()
  })
})

describe("Belt_Map.findFirstBy", () => {
  test("Belt_Map.findFirstBy", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      let s0 = Belt.Map.fromArray(~id=module(IntCmp), [(4, "4"), (1, "1"), (2, "2"), (3, "")])

      s0
      ->Belt.Map.findFirstBy((k, _) => k == 4)
      ->assertEqual(Some(4, "4"))
    }
    ()
  })
})

describe("Belt_Map.has", () => {
  test("Belt_Map.has", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.has(Belt.Map.fromArray([(1, "1")], ~id=module(IntCmp)), 1) == true
    }
    ()
  })
})

describe("Belt_Map.isEmpty", () => {
  test("Belt_Map.isEmpty", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      Belt.Map.isEmpty(Belt.Map.fromArray([(1, "1")], ~id=module(IntCmp))) == false
    }
    ()
  })
})

describe("Belt_Map.make", () => {
  test("Belt_Map.make", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = (a, b) => Pervasives.compare(a, b)
      })

      let m = Belt.Map.make(~id=module(IntCmp))

      Belt.Map.set(m, 0, "a")
    }
    ()
  })
})

describe("Belt_Map.Int", () => {
  test("Belt_Map.Int", () => {
    module Test = {
      type t<'key, 'value, 'identity>
      type id<'key, 'id> = Belt_Id.comparable<'key, 'id>
    }
    ()
  })
})

describe("Belt_MapString.findFirstBy", () => {
  test("Belt_MapString.findFirstBy", () => {
    module Test = {
      let mapString = Belt.Map.String.fromArray([("1", "one"), ("2", "two"), ("3", "three")])

      mapString
      ->Belt.Map.String.findFirstBy((k, v) => k == "1" && v == "one")
      ->assertEqual(Some("1", "one"))
    }
    ()
  })
})

describe("Belt_MapDict.findFirstBy", () => {
  test("Belt_MapDict.findFirstBy", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Map.Dict.fromArray([(4, "4"), (1, "1"), (2, "2"), (3, "3")], ~cmp=IntCmp.cmp)

      Belt.Map.Dict.findFirstBy(s0, (k, _) => k == 4) == Some((4, "4"))
    }
    ()
  })
})

describe("Belt_MapInt.findFirstBy", () => {
  test("Belt_MapInt.findFirstBy", () => {
    module Test = {
      let mapInt = Belt.Map.Int.fromArray([(1, "one"), (2, "two"), (3, "three")])

      mapInt
      ->Belt.Map.Int.findFirstBy((k, v) => k == 1 && v == "one")
      ->assertEqual(Some(1, "one"))
    }
    ()
  })
})

describe("Belt_MutableSet.split", () => {
  test("Belt_MutableSet.split", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))

      let ((smaller, larger), present) = s0->Belt.MutableSet.split(3)

      present /* true */
      smaller->Belt.MutableSet.toArray /* [1,2] */
      larger->Belt.MutableSet.toArray /* [4,5] */
    }
    ()
  })
})

describe("Belt_MutableSet.get", () => {
  test("Belt_MutableSet.get", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.get(3) /* Some(3) */
      s0->Belt.MutableSet.get(20) /* None */
    }
    ()
  })
})

describe("Belt_MutableSet.maxUndefined", () => {
  test("Belt_MutableSet.maxUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.make(~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.maxUndefined /* undefined */
      s1->Belt.MutableSet.maxUndefined /* 5 */
    }
    ()
  })
})

describe("Belt_MutableSet.maximum", () => {
  test("Belt_MutableSet.maximum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.make(~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.maximum /* None */
      s1->Belt.MutableSet.maximum /* Some(5) */
    }
    ()
  })
})

describe("Belt_MutableSet.minUndefined", () => {
  test("Belt_MutableSet.minUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.make(~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.minUndefined /* undefined */
      s1->Belt.MutableSet.minUndefined /* 1 */
    }
    ()
  })
})

describe("Belt_MutableSet.minimum", () => {
  test("Belt_MutableSet.minimum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.make(~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.minimum /* None */
      s1->Belt.MutableSet.minimum /* Some(1) */
    }
    ()
  })
})

describe("Belt_MutableSet.toArray", () => {
  test("Belt_MutableSet.toArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.toArray /* [1,2,3,5] */
    }
    ()
  })
})

describe("Belt_MutableSet.toList", () => {
  test("Belt_MutableSet.toList", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.MutableSet.toList /* [1,2,3,5] */
    }
    ()
  })
})

describe("Belt_MutableSet.size", () => {
  test("Belt_MutableSet.size", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4], ~id=module(IntCmp))

      s0->Belt.MutableSet.size /* 4 */
    }
    ()
  })
})

describe("Belt_MutableSet.partition", () => {
  test("Belt_MutableSet.partition", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))
      let (s1, s2) = s0->Belt.MutableSet.partition(isOdd)

      s1->Belt.MutableSet.toArray /* [1,3,5] */
      s2->Belt.MutableSet.toArray /* [2,4] */
    }
    ()
  })
})

describe("Belt_MutableSet.keep", () => {
  test("Belt_MutableSet.keep", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.MutableSet.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))
      let s1 = s0->Belt.MutableSet.keep(isEven)

      s1->Belt.MutableSet.toArray /* [2, 4] */
    }
    ()
  })
})

describe("Belt_MutableSet.some", () => {
  test("Belt_MutableSet.some", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.MutableSet.fromArray([1, 2, 4, 6, 8], ~id=module(IntCmp))
      s0->Belt.MutableSet.some(isOdd) /* true */
    }
    ()
  })
})

describe("Belt_MutableSet.every", () => {
  test("Belt_MutableSet.every", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.MutableSet.fromArray([2, 4, 6, 8], ~id=module(IntCmp))
      s0->Belt.MutableSet.every(isEven) /* true */
    }
    ()
  })
})

describe("Belt_MutableSet.reduce", () => {
  test("Belt_MutableSet.reduce", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      s0->Belt.MutableSet.reduce(
        list{},
        (acc, element) => acc->Belt.List.add(element),
      ) /* [6,5,3,2] */
    }
    ()
  })
})

describe("Belt_MutableSet.forEach", () => {
  test("Belt_MutableSet.forEach", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let acc = ref(list{})
      s0->Belt.MutableSet.forEach(x => acc := Belt.List.add(acc.contents, x))
      acc /* [6,5,3,2] */
    }
    ()
  })
})

describe("Belt_MutableSet.eq", () => {
  test("Belt_MutableSet.eq", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3], ~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([3, 2, 5], ~id=module(IntCmp))

      Belt.MutableSet.eq(s0, s1) /* true */
    }
    ()
  })
})

describe("Belt_MutableSet.subset", () => {
  test("Belt_MutableSet.subset", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
      let s2 = Belt.MutableSet.intersect(s0, s1)
      Belt.MutableSet.subset(s2, s0) /* true */
      Belt.MutableSet.subset(s2, s1) /* true */
      Belt.MutableSet.subset(s1, s0) /* false */
    }
    ()
  })
})

describe("Belt_MutableSet.diff", () => {
  test("Belt_MutableSet.diff", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
      Belt.MutableSet.toArray(Belt.MutableSet.diff(s0, s1)) /* [6] */
      Belt.MutableSet.toArray(Belt.MutableSet.diff(s1, s0)) /* [1,4] */
    }
    ()
  })
})

describe("Belt_MutableSet.intersect", () => {
  test("Belt_MutableSet.intersect", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
      let intersect = Belt.MutableSet.intersect(s0, s1)
      intersect->Belt.MutableSet.toArray /* [2,3,5] */
    }
    ()
  })
})

describe("Belt_MutableSet.union", () => {
  test("Belt_MutableSet.union", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.MutableSet.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
      let union = Belt.MutableSet.union(s0, s1)
      union->Belt.MutableSet.toArray /* [1,2,3,4,5,6] */
    }
    ()
  })
})

describe("Belt_MutableSet.removeMany", () => {
  test("Belt_MutableSet.removeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.MutableSet.fromArray([1, 2, 3, 4], ~id=module(IntCmp))

      set->Belt.MutableSet.removeMany([5, 4, 3, 2, 1])
      set->Belt.MutableSet.toArray /* [] */
    }
    ()
  })
})

describe("Belt_MutableSet.remove", () => {
  test("Belt_MutableSet.remove", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([2, 3, 1, 4, 5], ~id=module(IntCmp))
      s0->Belt.MutableSet.remove(1)
      s0->Belt.MutableSet.remove(3)
      s0->Belt.MutableSet.remove(3)

      s0->Belt.MutableSet.toArray /* [2,4,5] */
    }
    ()
  })
})

describe("Belt_MutableSet.mergeMany", () => {
  test("Belt_MutableSet.mergeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.MutableSet.make(~id=module(IntCmp))

      set->Belt.MutableSet.mergeMany([5, 4, 3, 2, 1])
      set->Belt.MutableSet.toArray /* [1, 2, 3, 4, 5] */
    }
    ()
  })
})

describe("Belt_MutableSet.add", () => {
  test("Belt_MutableSet.add", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.make(~id=module(IntCmp))
      s0->Belt.MutableSet.add(1)
      s0->Belt.MutableSet.add(2)
      s0->Belt.MutableSet.add(2)

      s0->Belt.MutableSet.toArray /* [1, 2] */
    }
    ()
  })
})

describe("Belt_MutableSet.has", () => {
  test("Belt_MutableSet.has", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.MutableSet.fromArray([1, 4, 2, 5], ~id=module(IntCmp))

      set->Belt.MutableSet.has(3) /* false */
      set->Belt.MutableSet.has(1) /* true */
    }
    ()
  })
})

describe("Belt_MutableSet.isEmpty", () => {
  test("Belt_MutableSet.isEmpty", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let empty = Belt.MutableSet.fromArray([], ~id=module(IntCmp))
      let notEmpty = Belt.MutableSet.fromArray([1], ~id=module(IntCmp))

      Belt.MutableSet.isEmpty(empty) /* true */
      Belt.MutableSet.isEmpty(notEmpty) /* false */
    }
    ()
  })
})

describe("Belt_MutableSet.copy", () => {
  test("Belt_MutableSet.copy", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([1, 3, 2, 4], ~id=module(IntCmp))

      let copied = s0->Belt.MutableSet.copy
      copied->Belt.MutableSet.toArray /* [1, 2, 3, 4] */
    }
    ()
  })
})

describe("Belt_MutableSet.fromArray", () => {
  test("Belt_MutableSet.fromArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.MutableSet.fromArray([1, 3, 2, 4], ~id=module(IntCmp))

      s0->Belt.MutableSet.toArray /* [1, 2, 3, 4] */
    }
    ()
  })
})

describe("Belt_Range.someBy", () => {
  test("Belt_Range.someBy", () => {
    module Test = {
      Belt.Range.someBy(1, 5, ~step=2, i => mod(i, 2) === 0) /* false */
      Belt.Range.someBy(0, 4, ~step=2, i => mod(i, 2) === 0) /* true */
    }
    ()
  })
})

describe("Belt_Range.some", () => {
  test("Belt_Range.some", () => {
    module Test = {
      Belt.Range.some(0, 4, i => i > 5) /* false */

      Belt.Range.some(0, 4, i => i > 2) /* true */
    }
    ()
  })
})

describe("Belt_Range.everyBy", () => {
  test("Belt_Range.everyBy", () => {
    module Test = {
      Belt.Range.everyBy(0, 4, ~step=1, i => mod(i, 2) === 0) /* false */

      Belt.Range.everyBy(0, 4, ~step=2, i => mod(i, 2) === 0) /* true */
    }
    ()
  })
})

describe("Belt_Range.every", () => {
  test("Belt_Range.every", () => {
    module Test = {
      Belt.Range.every(0, 4, i => i < 5) /* true */

      Belt.Range.every(0, 4, i => i < 4) /* false */
    }
    ()
  })
})

describe("Belt_Range.forEach", () => {
  test("Belt_Range.forEach", () => {
    module Test = {
      Belt.Range.forEach(0, 4, i => Js.log(i))

      // Prints:
      // 0
      // 1
      // 2
      // 3
      // 4
    }
    ()
  })
})

describe("Belt_Option.cmp", () => {
  test("Belt_Option.cmp", () => {
    module Test = {
      let clockCompare = (a, b) => compare(mod(a, 12), mod(b, 12))

      open Belt.Option

      cmp(Some(3), Some(15), clockCompare) /* 0 */

      cmp(Some(3), Some(14), clockCompare) /* 1 */

      cmp(Some(2), Some(15), clockCompare) /* (-1) */

      cmp(None, Some(15), clockCompare) /* (-1) */

      cmp(Some(14), None, clockCompare) /* 1 */

      cmp(None, None, clockCompare) /* 0 */
    }
    ()
  })
})

describe("Belt_Option.eq", () => {
  test("Belt_Option.eq", () => {
    module Test = {
      let clockEqual = (a, b) => mod(a, 12) == mod(b, 12)

      open Belt.Option

      eq(Some(3), Some(15), clockEqual) /* true */

      eq(Some(3), None, clockEqual) /* false */

      eq(None, Some(3), clockEqual) /* false */

      eq(None, None, clockEqual) /* true */
    }
    ()
  })
})

describe("Belt_Option.isNone", () => {
  test("Belt_Option.isNone", () => {
    module Test = {
      Belt.Option.isNone(None) /* true */

      Belt.Option.isNone(Some(1)) /* false */
    }
    ()
  })
})

describe("Belt_Option.isSome", () => {
  test("Belt_Option.isSome", () => {
    module Test = {
      Belt.Option.isSome(None) /* false */

      Belt.Option.isSome(Some(1)) /* true */
    }
    ()
  })
})

describe("Belt_Option.orElse", () => {
  test("Belt_Option.orElse", () => {
    module Test = {
      Belt.Option.orElse(Some(1812), Some(1066)) == Some(1812)
      Belt.Option.orElse(None, Some(1066)) == Some(1066)
      Belt.Option.orElse(None, None) == None
    }
    ()
  })
})

describe("Belt_Option.getWithDefault", () => {
  test("Belt_Option.getWithDefault", () => {
    module Test = {
      Belt.Option.getWithDefault(None, "Banana") /* Banana */

      Belt.Option.getWithDefault(Some("Apple"), "Banana") /* Apple */

      let greet = (firstName: option<string>) =>
        "Greetings " ++ firstName->Belt.Option.getWithDefault("Anonymous")

      Some("Jane")->greet /* "Greetings Jane" */

      None->greet /* "Greetings Anonymous" */
    }
    ()
  })
})

describe("Belt_Option.flatMap", () => {
  test("Belt_Option.flatMap", () => {
    module Test = {
      let addIfAboveOne = value =>
        if value > 1 {
          Some(value + 1)
        } else {
          None
        }

      Belt.Option.flatMap(Some(2), addIfAboveOne) /* Some(3) */

      Belt.Option.flatMap(Some(-4), addIfAboveOne) /* None */

      Belt.Option.flatMap(None, addIfAboveOne) /* None */
    }
    ()
  })
})

describe("Belt_Option.map", () => {
  test("Belt_Option.map", () => {
    module Test = {
      Belt.Option.map(Some(3), x => x * x) /* Some(9) */

      Belt.Option.map(None, x => x * x) /* None */
    }
    ()
  })
})

describe("Belt_Option.mapWithDefault", () => {
  test("Belt_Option.mapWithDefault", () => {
    module Test = {
      let someValue = Some(3)
      someValue->Belt.Option.mapWithDefault(0, x => x + 5) /* 8 */

      let noneValue = None
      noneValue->Belt.Option.mapWithDefault(0, x => x + 5) /* 0 */
    }
    ()
  })
})

describe("Belt_Option.getExn", () => {
  test("Belt_Option.getExn", () => {
    module Test = {
      Some(3)
      ->Belt.Option.getExn
      ->assertEqual(3)

      switch Belt.Option.getExn(None) {
      // Raises an exception
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Belt_Option.forEach", () => {
  test("Belt_Option.forEach", () => {
    module Test = {
      Belt.Option.forEach(Some("thing"), x => Js.log(x)) /* logs "thing" */
      Belt.Option.forEach(None, x => Js.log(x)) /* returns () */
    }
    ()
  })
})

describe("Belt_Option.keep", () => {
  test("Belt_Option.keep", () => {
    module Test = {
      Belt.Option.keep(Some(10), x => x > 5) /* returns `Some(10)` */
      Belt.Option.keep(Some(4), x => x > 5) /* returns `None` */
      Belt.Option.keep(None, x => x > 5) /* returns `None` */
    }
    ()
  })
})

describe("Belt_Result.cmp", () => {
  test("Belt_Result.cmp", () => {
    module Test = {
      let good1 = Belt.Result.Ok(59)

      let good2 = Belt.Result.Ok(37)

      let bad1 = Belt.Result.Error("invalid")

      let bad2 = Belt.Result.Error("really invalid")

      let mod10cmp = (a, b) => Pervasives.compare(mod(a, 10), mod(b, 10))

      Belt.Result.cmp(Ok(39), Ok(57), mod10cmp) == 1

      Belt.Result.cmp(Ok(57), Ok(39), mod10cmp) == -1

      Belt.Result.cmp(Ok(39), Error("y"), mod10cmp) == 1

      Belt.Result.cmp(Error("x"), Ok(57), mod10cmp) == -1

      Belt.Result.cmp(Error("x"), Error("y"), mod10cmp) == 0
    }
    ()
  })
})

describe("Belt_Result.eq", () => {
  test("Belt_Result.eq", () => {
    module Test = {
      let good1 = Belt.Result.Ok(42)

      let good2 = Belt.Result.Ok(32)

      let bad1 = Belt.Result.Error("invalid")

      let bad2 = Belt.Result.Error("really invalid")

      let mod10equal = (a, b) => mod(a, 10) === mod(b, 10)

      Belt.Result.eq(good1, good2, mod10equal) == true

      Belt.Result.eq(good1, bad1, mod10equal) == false

      Belt.Result.eq(bad2, good2, mod10equal) == false

      Belt.Result.eq(bad1, bad2, mod10equal) == true
    }
    ()
  })
})

describe("Belt_Result.getWithDefault", () => {
  test("Belt_Result.getWithDefault", () => {
    module Test = {
      Belt.Result.getWithDefault(Ok(42), 0) == 42

      Belt.Result.getWithDefault(Error("Invalid Data"), 0) == 0
    }
    ()
  })
})

describe("Belt_Result.flatMap", () => {
  test("Belt_Result.flatMap", () => {
    module Test = {
      let recip = x =>
        if x !== 0.0 {
          Belt.Result.Ok(1.0 /. x)
        } else {
          Belt.Result.Error("Divide by zero")
        }

      Belt.Result.flatMap(Ok(2.0), recip) == Ok(0.5)

      Belt.Result.flatMap(Ok(0.0), recip) == Error("Divide by zero")

      Belt.Result.flatMap(Error("Already bad"), recip) == Error("Already bad")
    }
    ()
  })
})

describe("Belt_Result.map", () => {
  test("Belt_Result.map", () => {
    module Test = {
      let f = x => sqrt(Belt.Int.toFloat(x))

      Belt.Result.map(Ok(64), f) == Ok(8.0)

      Belt.Result.map(Error("Invalid data"), f) == Error("Invalid data")
    }
    ()
  })
})

describe("Belt_Result.mapWithDefault", () => {
  test("Belt_Result.mapWithDefault", () => {
    module Test = {
      let ok = Belt.Result.Ok(42)
      Belt.Result.mapWithDefault(ok, 0, x => x / 2) == 21

      let error = Belt.Result.Error("Invalid data")
      Belt.Result.mapWithDefault(error, 0, x => x / 2) == 0
    }
    ()
  })
})

describe("Belt_Result.getExn", () => {
  test("Belt_Result.getExn", () => {
    module Test = {
      Belt.Result.Ok(42)
      ->Belt.Result.getExn
      ->assertEqual(42)

      switch Belt.Result.getExn(Belt.Result.Error("Invalid data")) {
      // raise a exception
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Belt_SetDict.split", () => {
  test("Belt_SetDict.split", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)

      let ((smaller, larger), present) = s0->Belt.Set.Dict.split(3, ~cmp=IntCmp.cmp)

      present /* true */
      smaller->Belt.Set.Dict.toArray /* [1,2] */
      larger->Belt.Set.Dict.toArray /* [4,5] */
    }
    ()
  })
})

describe("Belt_SetDict.get", () => {
  test("Belt_SetDict.get", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.get(3, ~cmp=IntCmp.cmp) /* Some(3) */
      s0->Belt.Set.Dict.get(20, ~cmp=IntCmp.cmp) /* None */
    }
    ()
  })
})

describe("Belt_SetDict.maxUndefined", () => {
  test("Belt_SetDict.maxUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.maxUndefined /* undefined */
      s1->Belt.Set.Dict.maxUndefined /* 5 */
    }
    ()
  })
})

describe("Belt_SetDict.maximum", () => {
  test("Belt_SetDict.maximum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.maximum /* None */
      s1->Belt.Set.Dict.maximum /* Some(5) */
    }
    ()
  })
})

describe("Belt_SetDict.minUndefined", () => {
  test("Belt_SetDict.minUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.minUndefined /* undefined */
      s1->Belt.Set.Dict.minUndefined /* 1 */
    }
    ()
  })
})

describe("Belt_SetDict.minimum", () => {
  test("Belt_SetDict.minimum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.minimum /* None */
      s1->Belt.Set.Dict.minimum /* Some(1) */
    }
    ()
  })
})

describe("Belt_SetDict.toArray", () => {
  test("Belt_SetDict.toArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.toArray /* [1,2,3,5] */
    }
    ()
  })
})

describe("Belt_SetDict.toList", () => {
  test("Belt_SetDict.toList", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.toList /* [1,2,3,5] */
    }
    ()
  })
})

describe("Belt_SetDict.size", () => {
  test("Belt_SetDict.size", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.size /* 4 */
    }
    ()
  })
})

describe("Belt_SetDict.partition", () => {
  test("Belt_SetDict.partition", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)
      let (s1, s2) = s0->Belt.Set.Dict.partition(isOdd)

      s1->Belt.Set.Dict.toArray /* [1,3,5] */
      s2->Belt.Set.Dict.toArray /* [2,4] */
    }
    ()
  })
})

describe("Belt_SetDict.keep", () => {
  test("Belt_SetDict.keep", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)
      let s1 = s0->Belt.Set.Dict.keep(isEven)

      s1->Belt.Set.Dict.toArray /* [2,4] */
    }
    ()
  })
})

describe("Belt_SetDict.some", () => {
  test("Belt_SetDict.some", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.Set.Dict.fromArray([1, 2, 4, 6, 8], ~cmp=IntCmp.cmp)
      s0->Belt.Set.Dict.some(isOdd) /* true */
    }
    ()
  })
})

describe("Belt_SetDict.every", () => {
  test("Belt_SetDict.every", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.Set.Dict.fromArray([2, 4, 6, 8], ~cmp=IntCmp.cmp)
      s0->Belt.Set.Dict.every(isEven) /* true */
    }
    ()
  })
})

describe("Belt_SetDict.reduce", () => {
  test("Belt_SetDict.reduce", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      s0->Belt.Set.Dict.reduce(
        list{},
        (acc, element) => acc->Belt.List.add(element),
      ) /* [6,5,3,2] */
    }
    ()
  })
})

describe("Belt_SetDict.forEach", () => {
  test("Belt_SetDict.forEach", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let acc = ref(list{})
      s0->Belt.Set.Dict.forEach(x => acc := Belt.List.add(acc.contents, x))
      acc /* [6,5,3,2] */
    }
    ()
  })
})

describe("Belt_SetDict.eq", () => {
  test("Belt_SetDict.eq", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([3, 2, 5], ~cmp=IntCmp.cmp)

      Belt.Set.Dict.eq(s0, s1, ~cmp=IntCmp.cmp) /* true */
    }
    ()
  })
})

describe("Belt_SetDict.subset", () => {
  test("Belt_SetDict.subset", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
      let s2 = Belt.Set.Dict.intersect(s0, s1, ~cmp=IntCmp.cmp)
      Belt.Set.Dict.subset(s2, s0, ~cmp=IntCmp.cmp) /* true */
      Belt.Set.Dict.subset(s2, s1, ~cmp=IntCmp.cmp) /* true */
      Belt.Set.Dict.subset(s1, s0, ~cmp=IntCmp.cmp) /* false */
    }
    ()
  })
})

describe("Belt_SetDict.diff", () => {
  test("Belt_SetDict.diff", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)

      let diff1 = Belt.Set.Dict.diff(s0, s1, ~cmp=IntCmp.cmp)
      let diff2 = Belt.Set.Dict.diff(s1, s0, ~cmp=IntCmp.cmp)

      diff1->Belt.Set.Dict.toArray /* [6] */
      diff2->Belt.Set.Dict.toArray /* [1,4] */
    }
    ()
  })
})

describe("Belt_SetDict.intersect", () => {
  test("Belt_SetDict.intersect", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
      let intersect = Belt.Set.Dict.intersect(s0, s1, ~cmp=IntCmp.cmp)
      intersect->Belt.Set.Dict.toArray /* [2,3,5] */
    }
    ()
  })
})

describe("Belt_SetDict.union", () => {
  test("Belt_SetDict.union", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
      let union = Belt.Set.Dict.union(s0, s1, ~cmp=IntCmp.cmp)
      union->Belt.Set.Dict.toArray /* [1,2,3,4,5,6] */
    }
    ()
  })
})

describe("Belt_SetDict.removeMany", () => {
  test("Belt_SetDict.removeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.Dict.fromArray([1, 2, 3, 4], ~cmp=IntCmp.cmp)

      let newSet = set->Belt.Set.Dict.removeMany([5, 4, 3, 2, 1], ~cmp=IntCmp.cmp)
      newSet->Belt.Set.Dict.toArray /* [] */
    }
    ()
  })
})

describe("Belt_SetDict.remove", () => {
  test("Belt_SetDict.remove", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([2, 3, 1, 4, 5], ~cmp=IntCmp.cmp)
      let s1 = s0->Belt.Set.Dict.remove(1, ~cmp=IntCmp.cmp)
      let s2 = s1->Belt.Set.Dict.remove(3, ~cmp=IntCmp.cmp)
      let s3 = s2->Belt.Set.Dict.remove(3, ~cmp=IntCmp.cmp)

      s1->Belt.Set.Dict.toArray /* [2,3,4,5] */
      s2->Belt.Set.Dict.toArray /* [2,4,5] */
      s2 == s3 /* true */
    }
    ()
  })
})

describe("Belt_SetDict.mergeMany", () => {
  test("Belt_SetDict.mergeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.Dict.empty

      let newSet = set->Belt.Set.Dict.mergeMany([5, 4, 3, 2, 1], ~cmp=IntCmp.cmp)
      newSet->Belt.Set.Dict.toArray /* [1, 2, 3, 4, 5] */
    }
    ()
  })
})

describe("Belt_SetDict.add", () => {
  test("Belt_SetDict.add", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = s0->Belt.Set.Dict.add(1, ~cmp=IntCmp.cmp)
      let s2 = s1->Belt.Set.Dict.add(2, ~cmp=IntCmp.cmp)
      let s3 = s2->Belt.Set.Dict.add(2, ~cmp=IntCmp.cmp)
      s0->Belt.Set.Dict.toArray /* [] */
      s1->Belt.Set.Dict.toArray /* [1] */
      s2->Belt.Set.Dict.toArray /* [1, 2] */
      s3->Belt.Set.Dict.toArray /* [1,2 ] */
      s2 == s3 /* true */
    }
    ()
  })
})

describe("Belt_SetDict.has", () => {
  test("Belt_SetDict.has", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.Dict.fromArray([1, 4, 2, 5], ~cmp=IntCmp.cmp)

      set->Belt.Set.Dict.has(3, ~cmp=IntCmp.cmp) /* false */
      set->Belt.Set.Dict.has(1, ~cmp=IntCmp.cmp) /* true */
    }
    ()
  })
})

describe("Belt_SetDict.isEmpty", () => {
  test("Belt_SetDict.isEmpty", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let empty = Belt.Set.Dict.fromArray([], ~cmp=IntCmp.cmp)
      let notEmpty = Belt.Set.Dict.fromArray([1], ~cmp=IntCmp.cmp)

      Belt.Set.Dict.isEmpty(empty) /* true */
      Belt.Set.Dict.isEmpty(notEmpty) /* false */
    }
    ()
  })
})

describe("Belt_SetDict.fromArray", () => {
  test("Belt_SetDict.fromArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([1, 3, 2, 4], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.toArray /* [1, 2, 3, 4] */
    }
    ()
  })
})

describe("Belt_SetDict.empty", () => {
  test("Belt_SetDict.empty", () => {
    module Test = {
      let s0 = Belt.Set.Dict.empty
    }
    ()
  })
})

describe("Belt_Set.Dict.split", () => {
  test("Belt_Set.Dict.split", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)

      let ((smaller, larger), present) = s0->Belt.Set.Dict.split(3, ~cmp=IntCmp.cmp)

      present /* true */
      smaller->Belt.Set.Dict.toArray /* [1,2] */
      larger->Belt.Set.Dict.toArray /* [4,5] */
    }
    ()
  })
})

describe("Belt_Set.Dict.get", () => {
  test("Belt_Set.Dict.get", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.get(3, ~cmp=IntCmp.cmp) /* Some(3) */
      s0->Belt.Set.Dict.get(20, ~cmp=IntCmp.cmp) /* None */
    }
    ()
  })
})

describe("Belt_Set.Dict.maxUndefined", () => {
  test("Belt_Set.Dict.maxUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.maxUndefined /* undefined */
      s1->Belt.Set.Dict.maxUndefined /* 5 */
    }
    ()
  })
})

describe("Belt_Set.Dict.maximum", () => {
  test("Belt_Set.Dict.maximum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.maximum /* None */
      s1->Belt.Set.Dict.maximum /* Some(5) */
    }
    ()
  })
})

describe("Belt_Set.Dict.minUndefined", () => {
  test("Belt_Set.Dict.minUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.minUndefined /* undefined */
      s1->Belt.Set.Dict.minUndefined /* 1 */
    }
    ()
  })
})

describe("Belt_Set.Dict.minimum", () => {
  test("Belt_Set.Dict.minimum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.minimum /* None */
      s1->Belt.Set.Dict.minimum /* Some(1) */
    }
    ()
  })
})

describe("Belt_Set.Dict.toArray", () => {
  test("Belt_Set.Dict.toArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.toArray /* [1,2,3,5] */
    }
    ()
  })
})

describe("Belt_Set.Dict.toList", () => {
  test("Belt_Set.Dict.toList", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([3, 2, 1, 5], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.toList /* [1,2,3,5] */
    }
    ()
  })
})

describe("Belt_Set.Dict.size", () => {
  test("Belt_Set.Dict.size", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.size /* 4 */
    }
    ()
  })
})

describe("Belt_Set.Dict.partition", () => {
  test("Belt_Set.Dict.partition", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)
      let (s1, s2) = s0->Belt.Set.Dict.partition(isOdd)

      s1->Belt.Set.Dict.toArray /* [1,3,5] */
      s2->Belt.Set.Dict.toArray /* [2,4] */
    }
    ()
  })
})

describe("Belt_Set.Dict.keep", () => {
  test("Belt_Set.Dict.keep", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.Set.Dict.fromArray([1, 2, 3, 4, 5], ~cmp=IntCmp.cmp)
      let s1 = s0->Belt.Set.Dict.keep(isEven)

      s1->Belt.Set.Dict.toArray /* [2,4] */
    }
    ()
  })
})

describe("Belt_Set.Dict.some", () => {
  test("Belt_Set.Dict.some", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.Set.Dict.fromArray([1, 2, 4, 6, 8], ~cmp=IntCmp.cmp)
      s0->Belt.Set.Dict.some(isOdd) /* true */
    }
    ()
  })
})

describe("Belt_Set.Dict.every", () => {
  test("Belt_Set.Dict.every", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.Set.Dict.fromArray([2, 4, 6, 8], ~cmp=IntCmp.cmp)
      s0->Belt.Set.Dict.every(isEven) /* true */
    }
    ()
  })
})

describe("Belt_Set.Dict.reduce", () => {
  test("Belt_Set.Dict.reduce", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      s0->Belt.Set.Dict.reduce(
        list{},
        (acc, element) => acc->Belt.List.add(element),
      ) /* [6,5,3,2] */
    }
    ()
  })
})

describe("Belt_Set.Dict.forEach", () => {
  test("Belt_Set.Dict.forEach", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let acc = ref(list{})
      s0->Belt.Set.Dict.forEach(x => acc := Belt.List.add(acc.contents, x))
      acc /* [6,5,3,2] */
    }
    ()
  })
})

describe("Belt_Set.Dict.eq", () => {
  test("Belt_Set.Dict.eq", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([3, 2, 5], ~cmp=IntCmp.cmp)

      Belt.Set.Dict.eq(s0, s1, ~cmp=IntCmp.cmp) /* true */
    }
    ()
  })
})

describe("Belt_Set.Dict.subset", () => {
  test("Belt_Set.Dict.subset", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
      let s2 = Belt.Set.Dict.intersect(s0, s1, ~cmp=IntCmp.cmp)
      Belt.Set.Dict.subset(s2, s0, ~cmp=IntCmp.cmp) /* true */
      Belt.Set.Dict.subset(s2, s1, ~cmp=IntCmp.cmp) /* true */
      Belt.Set.Dict.subset(s1, s0, ~cmp=IntCmp.cmp) /* false */
    }
    ()
  })
})

describe("Belt_Set.Dict.diff", () => {
  test("Belt_Set.Dict.diff", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)

      let diff1 = Belt.Set.Dict.diff(s0, s1, ~cmp=IntCmp.cmp)
      let diff2 = Belt.Set.Dict.diff(s1, s0, ~cmp=IntCmp.cmp)

      diff1->Belt.Set.Dict.toArray /* [6] */
      diff2->Belt.Set.Dict.toArray /* [1,4] */
    }
    ()
  })
})

describe("Belt_Set.Dict.intersect", () => {
  test("Belt_Set.Dict.intersect", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
      let intersect = Belt.Set.Dict.intersect(s0, s1, ~cmp=IntCmp.cmp)
      intersect->Belt.Set.Dict.toArray /* [2,3,5] */
    }
    ()
  })
})

describe("Belt_Set.Dict.union", () => {
  test("Belt_Set.Dict.union", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([5, 2, 3, 5, 6], ~cmp=IntCmp.cmp)
      let s1 = Belt.Set.Dict.fromArray([5, 2, 3, 1, 5, 4], ~cmp=IntCmp.cmp)
      let union = Belt.Set.Dict.union(s0, s1, ~cmp=IntCmp.cmp)
      union->Belt.Set.Dict.toArray /* [1,2,3,4,5,6] */
    }
    ()
  })
})

describe("Belt_Set.Dict.removeMany", () => {
  test("Belt_Set.Dict.removeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.Dict.fromArray([1, 2, 3, 4], ~cmp=IntCmp.cmp)

      let newSet = set->Belt.Set.Dict.removeMany([5, 4, 3, 2, 1], ~cmp=IntCmp.cmp)
      newSet->Belt.Set.Dict.toArray /* [] */
    }
    ()
  })
})

describe("Belt_Set.Dict.remove", () => {
  test("Belt_Set.Dict.remove", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([2, 3, 1, 4, 5], ~cmp=IntCmp.cmp)
      let s1 = s0->Belt.Set.Dict.remove(1, ~cmp=IntCmp.cmp)
      let s2 = s1->Belt.Set.Dict.remove(3, ~cmp=IntCmp.cmp)
      let s3 = s2->Belt.Set.Dict.remove(3, ~cmp=IntCmp.cmp)

      s1->Belt.Set.Dict.toArray /* [2,3,4,5] */
      s2->Belt.Set.Dict.toArray /* [2,4,5] */
      s2 == s3 /* true */
    }
    ()
  })
})

describe("Belt_Set.Dict.mergeMany", () => {
  test("Belt_Set.Dict.mergeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.Dict.empty

      let newSet = set->Belt.Set.Dict.mergeMany([5, 4, 3, 2, 1], ~cmp=IntCmp.cmp)
      newSet->Belt.Set.Dict.toArray /* [1, 2, 3, 4, 5] */
    }
    ()
  })
})

describe("Belt_Set.Dict.add", () => {
  test("Belt_Set.Dict.add", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.empty
      let s1 = s0->Belt.Set.Dict.add(1, ~cmp=IntCmp.cmp)
      let s2 = s1->Belt.Set.Dict.add(2, ~cmp=IntCmp.cmp)
      let s3 = s2->Belt.Set.Dict.add(2, ~cmp=IntCmp.cmp)
      s0->Belt.Set.Dict.toArray /* [] */
      s1->Belt.Set.Dict.toArray /* [1] */
      s2->Belt.Set.Dict.toArray /* [1, 2] */
      s3->Belt.Set.Dict.toArray /* [1,2 ] */
      s2 == s3 /* true */
    }
    ()
  })
})

describe("Belt_Set.Dict.has", () => {
  test("Belt_Set.Dict.has", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.Dict.fromArray([1, 4, 2, 5], ~cmp=IntCmp.cmp)

      set->Belt.Set.Dict.has(3, ~cmp=IntCmp.cmp) /* false */
      set->Belt.Set.Dict.has(1, ~cmp=IntCmp.cmp) /* true */
    }
    ()
  })
})

describe("Belt_Set.Dict.isEmpty", () => {
  test("Belt_Set.Dict.isEmpty", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let empty = Belt.Set.Dict.fromArray([], ~cmp=IntCmp.cmp)
      let notEmpty = Belt.Set.Dict.fromArray([1], ~cmp=IntCmp.cmp)

      Belt.Set.Dict.isEmpty(empty) /* true */
      Belt.Set.Dict.isEmpty(notEmpty) /* false */
    }
    ()
  })
})

describe("Belt_Set.Dict.fromArray", () => {
  test("Belt_Set.Dict.fromArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.Dict.fromArray([1, 3, 2, 4], ~cmp=IntCmp.cmp)

      s0->Belt.Set.Dict.toArray /* [1, 2, 3, 4] */
    }
    ()
  })
})

describe("Belt_Set.Dict.empty", () => {
  test("Belt_Set.Dict.empty", () => {
    module Test = {
      let s0 = Belt.Set.Dict.empty
    }
    ()
  })
})

describe("Belt_Set.split", () => {
  test("Belt_Set.split", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))

      let ((smaller, larger), present) = s0->Belt.Set.split(3)

      present->assertEqual(true)
      smaller->Belt.Set.toArray->assertEqual([1, 2])
      larger->Belt.Set.toArray->assertEqual([4, 5])
    }
    ()
  })
})

describe("Belt_Set.get", () => {
  test("Belt_Set.get", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))

      s0->Belt.Set.get(3)->assertEqual(Some(3))
      s0->Belt.Set.get(20)->assertEqual(None)
    }
    ()
  })
})

describe("Belt_Set.maxUndefined", () => {
  test("Belt_Set.maxUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.make(~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0
      ->Belt.Set.maxUndefined
      ->Js.Undefined.toOption
      ->assertEqual(None)

      s1
      ->Belt.Set.maxUndefined
      ->Js.Undefined.toOption
      ->assertEqual(Some(5))
    }
    ()
  })
})

describe("Belt_Set.maximum", () => {
  test("Belt_Set.maximum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.make(~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.Set.maximum->assertEqual(None)
      s1->Belt.Set.maximum->assertEqual(Some(5))
    }
    ()
  })
})

describe("Belt_Set.minUndefined", () => {
  test("Belt_Set.minUndefined", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.make(~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.Set.minUndefined->Js.Undefined.toOption->assertEqual(None)
      s1->Belt.Set.minUndefined->Js.Undefined.toOption->assertEqual(Some(1))
    }
    ()
  })
})

describe("Belt_Set.minimum", () => {
  test("Belt_Set.minimum", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.make(~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.Set.minimum->assertEqual(None)
      s1->Belt.Set.minimum->assertEqual(Some(1))
    }
    ()
  })
})

describe("Belt_Set.toList", () => {
  test("Belt_Set.toList", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.Set.toList->assertEqual(list{1, 2, 3, 5})
    }
    ()
  })
})

describe("Belt_Set.toArray", () => {
  test("Belt_Set.toArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([3, 2, 1, 5], ~id=module(IntCmp))

      s0->Belt.Set.toArray->assertEqual([1, 2, 3, 5])
    }
    ()
  })
})

describe("Belt_Set.size", () => {
  test("Belt_Set.size", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([1, 2, 3, 4], ~id=module(IntCmp))

      s0->Belt.Set.size->assertEqual(4)
    }
    ()
  })
})

describe("Belt_Set.partition", () => {
  test("Belt_Set.partition", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.Set.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))
      let (s1, s2) = s0->Belt.Set.partition(isOdd)

      s1->Belt.Set.toArray->assertEqual([1, 3, 5])
      s2->Belt.Set.toArray->assertEqual([2, 4])
    }
    ()
  })
})

describe("Belt_Set.keep", () => {
  test("Belt_Set.keep", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.Set.fromArray([1, 2, 3, 4, 5], ~id=module(IntCmp))
      let s1 = s0->Belt.Set.keep(isEven)

      s1->Belt.Set.toArray->assertEqual([2, 4])
    }
    ()
  })
})

describe("Belt_Set.some", () => {
  test("Belt_Set.some", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isOdd = x => mod(x, 2) != 0

      let s0 = Belt.Set.fromArray([1, 2, 4, 6, 8], ~id=module(IntCmp))
      s0->Belt.Set.some(isOdd)->assertEqual(true)
    }
    ()
  })
})

describe("Belt_Set.every", () => {
  test("Belt_Set.every", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let isEven = x => mod(x, 2) == 0

      let s0 = Belt.Set.fromArray([2, 4, 6, 8], ~id=module(IntCmp))
      s0->Belt.Set.every(isEven)->assertEqual(true)
    }
    ()
  })
})

describe("Belt_Set.reduce", () => {
  test("Belt_Set.reduce", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      s0
      ->Belt.Set.reduce(list{}, (acc, element) => acc->Belt.List.add(element))
      ->assertEqual(list{6, 5, 3, 2})
    }
    ()
  })
})

describe("Belt_Set.forEach", () => {
  test("Belt_Set.forEach", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))

      let acc = ref(list{})

      s0->Belt.Set.forEach(
        x => {
          acc := Belt.List.add(acc.contents, x)
        },
      )

      acc.contents->assertEqual(list{6, 5, 3, 2})
    }
    ()
  })
})

describe("Belt_Set.eq", () => {
  test("Belt_Set.eq", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3], ~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([3, 2, 5], ~id=module(IntCmp))

      Belt.Set.eq(s0, s1)->assertEqual(true)
    }
    ()
  })
})

describe("Belt_Set.subset", () => {
  test("Belt_Set.subset", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
      let s2 = Belt.Set.intersect(s0, s1)

      Belt.Set.subset(s2, s0)->assertEqual(true)
      Belt.Set.subset(s2, s1)->assertEqual(true)
      Belt.Set.subset(s1, s0)->assertEqual(false)
    }
    ()
  })
})

describe("Belt_Set.diff", () => {
  test("Belt_Set.diff", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))

      Belt.Set.diff(s0, s1)
      ->Belt.Set.toArray
      ->assertEqual([6])

      Belt.Set.diff(s1, s0)
      ->Belt.Set.toArray
      ->assertEqual([1, 4])
    }
    ()
  })
})

describe("Belt_Set.intersect", () => {
  test("Belt_Set.intersect", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))

      let intersect = Belt.Set.intersect(s0, s1)

      intersect
      ->Belt.Set.toArray
      ->assertEqual([2, 3, 5])
    }
    ()
  })
})

describe("Belt_Set.union", () => {
  test("Belt_Set.union", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([5, 2, 3, 5, 6], ~id=module(IntCmp))
      let s1 = Belt.Set.fromArray([5, 2, 3, 1, 5, 4], ~id=module(IntCmp))
      let union = Belt.Set.union(s0, s1)

      union
      ->Belt.Set.toArray
      ->assertEqual([1, 2, 3, 4, 5, 6])
    }
    ()
  })
})

describe("Belt_Set.removeMany", () => {
  test("Belt_Set.removeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.fromArray([1, 2, 3, 4], ~id=module(IntCmp))

      let newSet = set->Belt.Set.removeMany([5, 4, 3, 2, 1])

      newSet
      ->Belt.Set.toArray
      ->assertEqual([])
    }
    ()
  })
})

describe("Belt_Set.remove", () => {
  test("Belt_Set.remove", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([2, 3, 1, 4, 5], ~id=module(IntCmp))
      let s1 = s0->Belt.Set.remove(1)
      let s2 = s1->Belt.Set.remove(3)
      let s3 = s2->Belt.Set.remove(3)

      s1->Belt.Set.toArray->assertEqual([2, 3, 4, 5])
      s2->Belt.Set.toArray->assertEqual([2, 4, 5])
      assertEqual(s2, s3)
    }
    ()
  })
})

describe("Belt_Set.mergeMany", () => {
  test("Belt_Set.mergeMany", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.make(~id=module(IntCmp))

      let newSet = set->Belt.Set.mergeMany([5, 4, 3, 2, 1])

      newSet
      ->Belt.Set.toArray
      ->assertEqual([1, 2, 3, 4, 5])
    }
    ()
  })
})

describe("Belt_Set.add", () => {
  test("Belt_Set.add", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.make(~id=module(IntCmp))

      let s1 = s0->Belt.Set.add(1)
      let s2 = s1->Belt.Set.add(2)
      let s3 = s2->Belt.Set.add(2)

      s0->Belt.Set.toArray->assertEqual([])
      s1->Belt.Set.toArray->assertEqual([1])
      s2->Belt.Set.toArray->assertEqual([1, 2])
      s3->Belt.Set.toArray->assertEqual([1, 2])
      assertEqual(s2, s3)
    }
    ()
  })
})

describe("Belt_Set.has", () => {
  test("Belt_Set.has", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.fromArray([1, 4, 2, 5], ~id=module(IntCmp))

      set->Belt.Set.has(3)->assertEqual(false)
      set->Belt.Set.has(1)->assertEqual(true)
    }
    ()
  })
})

describe("Belt_Set.isEmpty", () => {
  test("Belt_Set.isEmpty", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let empty = Belt.Set.fromArray([], ~id=module(IntCmp))
      let notEmpty = Belt.Set.fromArray([1], ~id=module(IntCmp))

      Belt.Set.isEmpty(empty)->assertEqual(true)
      Belt.Set.isEmpty(notEmpty)->assertEqual(false)
    }
    ()
  })
})

describe("Belt_Set.fromArray", () => {
  test("Belt_Set.fromArray", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let s0 = Belt.Set.fromArray([1, 3, 2, 4], ~id=module(IntCmp))

      s0->Belt.Set.toArray->assertEqual([1, 2, 3, 4])
    }
    ()
  })
})

describe("Belt_Set.make", () => {
  test("Belt_Set.make", () => {
    module Test = {
      module IntCmp = Belt.Id.MakeComparable({
        type t = int
        let cmp = Pervasives.compare
      })

      let set = Belt.Set.make(~id=module(IntCmp))

      Belt.Set.isEmpty(set)->assertEqual(true)
    }
    ()
  })
})

describe("Belt_SortArray.binarySearchBy", () => {
  test("Belt_SortArray.binarySearchBy", () => {
    module Test = {
      Belt.SortArray.binarySearchBy([1, 2, 3, 4, 33, 35, 36], 33, Pervasives.compare) == 4

      lnot(Belt.SortArray.binarySearchBy([1, 3, 5, 7], 4, Pervasives.compare)) == 2
    }
    ()
  })
})

describe("Belt_SortArray.strictlySortedLength", () => {
  test("Belt_SortArray.strictlySortedLength", () => {
    module Test = {
      Belt.SortArray.strictlySortedLength([1, 2, 3, 4, 3], (x, y) => x < y) == 4

      Belt.SortArray.strictlySortedLength([], (x, y) => x < y) == 0

      Belt.SortArray.strictlySortedLength([1], (x, y) => x < y) == 1

      Belt.SortArray.strictlySortedLength([4, 3, 2, 1], (x, y) => x < y) == -4
    }
    ()
  })
})

describe("BigInt.toLocaleString", () => {
  test("BigInt.toLocaleString", () => {
    module Test = {
      /* prints "123" */
      Js.BigInt.toString(123n)->Js.log
    }
    ()
  })
})

describe("BigInt.toString", () => {
  test("BigInt.toString", () => {
    module Test = {
      /* prints "123" */
      Js.BigInt.toString(123n)->Js.log
    }
    ()
  })
})

describe("BigInt.fromStringExn", () => {
  test("BigInt.fromStringExn", () => {
    module Test = {
      /* returns 123n */
      BigInt.fromStringExn("123")

      /* returns 0n */
      BigInt.fromStringExn("")

      /* returns 17n */
      BigInt.fromStringExn("0x11")

      /* returns 3n */
      BigInt.fromStringExn("0b11")

      /* returns 9n */
      BigInt.fromStringExn("0o11")

      /* catch exception */
      try {
        BigInt.fromStringExn("a")
      } catch {
      | Exn.Error(_error) => 0n
      }
    }
    ()
  })
})

describe("Belt_internalSetString.A.truncateToLengthUnsafe", () => {
  test("Belt_internalSetString.A.truncateToLengthUnsafe", () => {
    module Test = {
      let arr = ["ant", "bee", "cat", "dog", "elk"]

      Belt.Array.truncateToLengthUnsafe(arr, 3)

      arr == ["ant", "bee", "cat"]
    }
    ()
  })
})

describe("Belt_internalSetString.A.eq", () => {
  test("Belt_internalSetString.A.eq", () => {
    module Test = {
      Belt.Array.eq([1, 2, 3], [-1, -2, -3], (a, b) => abs(a) == abs(b)) == true
    }
    ()
  })
})

describe("Belt_internalSetString.A.cmp", () => {
  test("Belt_internalSetString.A.cmp", () => {
    module Test = {
      Belt.Array.cmp([1, 3, 5], [1, 4, 2], (a, b) => compare(a, b)) == -1

      Belt.Array.cmp([1, 3, 5], [1, 2, 3], (a, b) => compare(a, b)) == 1

      Belt.Array.cmp([1, 3, 5], [1, 3, 5], (a, b) => compare(a, b)) == 0
    }
    ()
  })
})

describe("Belt_internalSetString.A.some2", () => {
  test("Belt_internalSetString.A.some2", () => {
    module Test = {
      Belt.Array.some2([0, 2], [1, 0, 3], (a, b) => a > b) == true

      Belt.Array.some2([], [1], (x, y) => x > y) == false

      Belt.Array.some2([2, 3], [1, 4], (x, y) => x > y) == true
    }
    ()
  })
})

describe("Belt_internalSetString.A.every2", () => {
  test("Belt_internalSetString.A.every2", () => {
    module Test = {
      Belt.Array.every2([1, 2, 3], [0, 1], (a, b) => a > b) == true

      Belt.Array.every2([], [1], (x, y) => x > y) == true

      Belt.Array.every2([2, 3], [1], (x, y) => x > y) == true

      Belt.Array.every2([0, 1], [5, 0], (x, y) => x > y) == false
    }
    ()
  })
})

describe("Belt_internalSetString.A.every", () => {
  test("Belt_internalSetString.A.every", () => {
    module Test = {
      Belt.Array.every([1, 3, 5], x => mod(x, 2) == 1) == true

      Belt.Array.every([1, -3, 5], x => x > 0) == false
    }
    ()
  })
})

describe("Belt_internalSetString.A.some", () => {
  test("Belt_internalSetString.A.some", () => {
    module Test = {
      Belt.Array.some([2, 3, 4], x => mod(x, 2) == 1) == true

      Belt.Array.some([-1, -3, -5], x => x > 0) == false
    }
    ()
  })
})

describe("Belt_internalSetString.A.joinWith", () => {
  test("Belt_internalSetString.A.joinWith", () => {
    module Test = {
      Belt.Array.joinWith([0, 1], ", ", Js.Int.toString) == "0, 1"
      Belt.Array.joinWith([], " ", Js.Int.toString) == ""
      Belt.Array.joinWith([1], " ", Js.Int.toString) == "1"
    }
    ()
  })
})

describe("Belt_internalSetString.A.reduceWithIndex", () => {
  test("Belt_internalSetString.A.reduceWithIndex", () => {
    module Test = {
      Belt.Array.reduceWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i) == 16
    }
    ()
  })
})

describe("Belt_internalSetString.A.reduceReverse2", () => {
  test("Belt_internalSetString.A.reduceReverse2", () => {
    module Test = {
      Belt.Array.reduceReverse2([1, 2, 3], [1, 2], 0, (acc, x, y) => acc + x + y) == 6
    }
    ()
  })
})

describe("Belt_internalSetString.A.reduceReverse", () => {
  test("Belt_internalSetString.A.reduceReverse", () => {
    module Test = {
      Belt.Array.reduceReverse(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "dcba"
    }
    ()
  })
})

describe("Belt_internalSetString.A.reduce", () => {
  test("Belt_internalSetString.A.reduce", () => {
    module Test = {
      Belt.Array.reduce([2, 3, 4], 1, (a, b) => a + b) == 10

      Belt.Array.reduce(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "abcd"
    }
    ()
  })
})

describe("Belt_internalSetString.A.partition", () => {
  test("Belt_internalSetString.A.partition", () => {
    module Test = {
      Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) == 0) == ([2, 4], [1, 3, 5])

      Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) != 0) == ([1, 3, 5], [2, 4])
    }
    ()
  })
})

describe("Belt_internalSetString.A.mapWithIndex", () => {
  test("Belt_internalSetString.A.mapWithIndex", () => {
    module Test = {
      Belt.Array.mapWithIndex([1, 2, 3], (i, x) => i + x) == [0 + 1, 1 + 2, 2 + 3]
    }
    ()
  })
})

describe("Belt_internalSetString.A.forEachWithIndex", () => {
  test("Belt_internalSetString.A.forEachWithIndex", () => {
    module Test = {
      Belt.Array.forEachWithIndex(
        ["a", "b", "c"],
        (i, x) => Js.log("Item " ++ Belt.Int.toString(i) ++ " is " ++ x),
      )

      /*
  prints:
  Item 0 is a
  Item 1 is b
  Item 2 is cc
*/
      let total = ref(0)

      Belt.Array.forEachWithIndex([10, 11, 12, 13], (i, x) => total := total.contents + x + i)

      total.contents == 0 + 10 + 1 + 11 + 2 + 12 + 3 + 13
    }
    ()
  })
})

describe("Belt_internalSetString.A.keepMap", () => {
  test("Belt_internalSetString.A.keepMap", () => {
    module Test = {
      Belt.Array.keepMap(
        [1, 2, 3],
        x =>
          if mod(x, 2) == 0 {
            Some(x)
          } else {
            None
          },
      ) == [2]
    }
    ()
  })
})

describe("Belt_internalSetString.A.keepWithIndex", () => {
  test("Belt_internalSetString.A.keepWithIndex", () => {
    module Test = {
      Belt.Array.keepWithIndex([1, 2, 3], (_x, i) => i == 1) == [2]
    }
    ()
  })
})

describe("Belt_internalSetString.A.getIndexBy", () => {
  test("Belt_internalSetString.A.getIndexBy", () => {
    module Test = {
      Belt.Array.getIndexBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(1)
      Belt.Array.getIndexBy([15, 13, 11], x => mod(x, 2) == 0) == None
    }
    ()
  })
})

describe("Belt_internalSetString.A.getBy", () => {
  test("Belt_internalSetString.A.getBy", () => {
    module Test = {
      Belt.Array.getBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(4)
      Belt.Array.getBy([15, 13, 11], x => mod(x, 2) == 0) == None
    }
    ()
  })
})

describe("Belt_internalSetString.A.flatMap", () => {
  test("Belt_internalSetString.A.flatMap", () => {
    module Test = {
      Belt.Array.flatMap([1, 2], x => [x + 10, x + 20]) == [11, 21, 12, 22]
    }
    ()
  })
})

describe("Belt_internalSetString.A.map", () => {
  test("Belt_internalSetString.A.map", () => {
    module Test = {
      Belt.Array.map([1, 2], x => x + 1) == [3, 4]
    }
    ()
  })
})

describe("Belt_internalSetString.A.forEach", () => {
  test("Belt_internalSetString.A.forEach", () => {
    module Test = {
      Belt.Array.forEach(["a", "b", "c"], x => Js.log("Item: " ++ x))

      /*
  prints:
  Item: a
  Item: b
  Item: c
*/
      let total = ref(0)

      Belt.Array.forEach([1, 2, 3, 4], x => total := total.contents + x)

      total.contents == 1 + 2 + 3 + 4
    }
    ()
  })
})

describe("Belt_internalSetString.A.blit", () => {
  test("Belt_internalSetString.A.blit", () => {
    module Test = {
      let v1 = [10, 11, 12, 13, 14, 15, 16, 17]
      let v2 = [20, 21, 22, 23, 24, 25, 26, 27]

      Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v2, ~dstOffset=2, ~len=3)
      v2 == [20, 21, 14, 15, 16, 25, 26, 27]

      Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v1, ~dstOffset=2, ~len=3)
      v1 == [10, 11, 14, 15, 16, 15, 16, 17]
    }
    ()
  })
})

describe("Belt_internalSetString.A.fill", () => {
  test("Belt_internalSetString.A.fill", () => {
    module Test = {
      let arr = Belt.Array.makeBy(5, i => i)

      Belt.Array.fill(arr, ~offset=2, ~len=2, 9)

      arr == [0, 1, 9, 9, 4]

      Belt.Array.fill(arr, ~offset=7, ~len=2, 8)

      arr == [0, 1, 9, 9, 4]
    }
    ()
  })
})

describe("Belt_internalSetString.A.sliceToEnd", () => {
  test("Belt_internalSetString.A.sliceToEnd", () => {
    module Test = {
      Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], 2) == [12, 13, 14, 15, 16]

      Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], -4) == [13, 14, 15, 16]
    }
    ()
  })
})

describe("Belt_internalSetString.A.slice", () => {
  test("Belt_internalSetString.A.slice", () => {
    module Test = {
      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=2, ~len=3) == [12, 13, 14]

      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=-4, ~len=3) == [13, 14, 15]

      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=4, ~len=9) == [14, 15, 16]
    }
    ()
  })
})

describe("Belt_internalSetString.A.concatMany", () => {
  test("Belt_internalSetString.A.concatMany", () => {
    module Test = {
      Belt.Array.concatMany([[1, 2, 3], [4, 5, 6], [7, 8]]) == [1, 2, 3, 4, 5, 6, 7, 8]
    }
    ()
  })
})

describe("Belt_internalSetString.A.concat", () => {
  test("Belt_internalSetString.A.concat", () => {
    module Test = {
      Belt.Array.concat([1, 2, 3], [4, 5]) == [1, 2, 3, 4, 5]

      Belt.Array.concat([], ["a", "b", "c"]) == ["a", "b", "c"]
    }
    ()
  })
})

describe("Belt_internalSetString.A.unzip", () => {
  test("Belt_internalSetString.A.unzip", () => {
    module Test = {
      Belt.Array.unzip([(1, 2), (3, 4)]) == ([1, 3], [2, 4])

      Belt.Array.unzip([(1, 2), (3, 4), (5, 6), (7, 8)]) == ([1, 3, 5, 7], [2, 4, 6, 8])
    }
    ()
  })
})

describe("Belt_internalSetString.A.zipBy", () => {
  test("Belt_internalSetString.A.zipBy", () => {
    module Test = {
      Belt.Array.zipBy([1, 2, 3], [4, 5], (a, b) => 2 * a + b) == [6, 9]
    }
    ()
  })
})

describe("Belt_internalSetString.A.zip", () => {
  test("Belt_internalSetString.A.zip", () => {
    module Test = {
      Belt.Array.zip([1, 2], [3, 4, 5]) == [(1, 3), (2, 4)]
    }
    ()
  })
})

describe("Belt_internalSetString.A.makeBy", () => {
  test("Belt_internalSetString.A.makeBy", () => {
    module Test = {
      Belt.Array.makeBy(5, i => i) == [0, 1, 2, 3, 4]

      Belt.Array.makeBy(5, i => i * i) == [0, 1, 4, 9, 16]
    }
    ()
  })
})

describe("Belt_internalSetString.A.rangeBy", () => {
  test("Belt_internalSetString.A.rangeBy", () => {
    module Test = {
      Belt.Array.rangeBy(0, 10, ~step=3) == [0, 3, 6, 9]

      Belt.Array.rangeBy(0, 12, ~step=3) == [0, 3, 6, 9, 12]

      Belt.Array.rangeBy(33, 0, ~step=1) == []

      Belt.Array.rangeBy(33, 0, ~step=-1) == []

      Belt.Array.rangeBy(3, 12, ~step=-1) == []

      Belt.Array.rangeBy(3, 3, ~step=0) == []

      Belt.Array.rangeBy(3, 3, ~step=1) == [3]
    }
    ()
  })
})

describe("Belt_internalSetString.A.range", () => {
  test("Belt_internalSetString.A.range", () => {
    module Test = {
      Belt.Array.range(0, 3) == [0, 1, 2, 3]

      Belt.Array.range(3, 0) == []

      Belt.Array.range(3, 3) == [3]
    }
    ()
  })
})

describe("Belt_internalSetString.A.makeUninitializedUnsafe", () => {
  test("Belt_internalSetString.A.makeUninitializedUnsafe", () => {
    module Test = {
      let arr = Belt.Array.makeUninitializedUnsafe(5)

      Js.log(Belt.Array.getExn(arr, 0)) // undefined

      Belt.Array.setExn(arr, 0, "example")

      Js.log(Belt.Array.getExn(arr, 0) == "example")
    }
    ()
  })
})

describe("Belt_internalSetString.A.makeUninitialized", () => {
  test("Belt_internalSetString.A.makeUninitialized", () => {
    module Test = {
      let arr: array<Js.undefined<string>> = Belt.Array.makeUninitialized(5)

      Belt.Array.getExn(arr, 0) == Js.undefined
    }
    ()
  })
})

describe("Belt_internalSetString.A.reverse", () => {
  test("Belt_internalSetString.A.reverse", () => {
    module Test = {
      Belt.Array.reverse([10, 11, 12, 13, 14]) == [14, 13, 12, 11, 10]
    }
    ()
  })
})

describe("Belt_internalSetString.A.reverseInPlace", () => {
  test("Belt_internalSetString.A.reverseInPlace", () => {
    module Test = {
      let arr = [10, 11, 12, 13, 14]

      let () = Belt.Array.reverseInPlace(arr)

      arr == [14, 13, 12, 11, 10]
    }
    ()
  })
})

describe("Belt_internalSetString.A.get", () => {
  test("Belt_internalSetString.A.get", () => {
    module Test = {
      Belt.Array.get(["a", "b", "c"], 0) == Some("a")
      Belt.Array.get(["a", "b", "c"], 3) == None
      Belt.Array.get(["a", "b", "c"], -1) == None
    }
    ()
  })
})

describe("Belt_internalSetString.A.length", () => {
  test("Belt_internalSetString.A.length", () => {
    module Test = {
      // Returns 1
      Belt.Array.length(["test"])
    }
    ()
  })
})

describe("Belt_internalMapInt.S.binarySearchBy", () => {
  test("Belt_internalMapInt.S.binarySearchBy", () => {
    module Test = {
      Belt.SortArray.binarySearchBy([1, 2, 3, 4, 33, 35, 36], 33, Pervasives.compare) == 4

      lnot(Belt.SortArray.binarySearchBy([1, 3, 5, 7], 4, Pervasives.compare)) == 2
    }
    ()
  })
})

describe("Belt_internalMapInt.S.strictlySortedLength", () => {
  test("Belt_internalMapInt.S.strictlySortedLength", () => {
    module Test = {
      Belt.SortArray.strictlySortedLength([1, 2, 3, 4, 3], (x, y) => x < y) == 4

      Belt.SortArray.strictlySortedLength([], (x, y) => x < y) == 0

      Belt.SortArray.strictlySortedLength([1], (x, y) => x < y) == 1

      Belt.SortArray.strictlySortedLength([4, 3, 2, 1], (x, y) => x < y) == -4
    }
    ()
  })
})

describe("Belt_internalMapInt.A.truncateToLengthUnsafe", () => {
  test("Belt_internalMapInt.A.truncateToLengthUnsafe", () => {
    module Test = {
      let arr = ["ant", "bee", "cat", "dog", "elk"]

      Belt.Array.truncateToLengthUnsafe(arr, 3)

      arr == ["ant", "bee", "cat"]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.eq", () => {
  test("Belt_internalMapInt.A.eq", () => {
    module Test = {
      Belt.Array.eq([1, 2, 3], [-1, -2, -3], (a, b) => abs(a) == abs(b)) == true
    }
    ()
  })
})

describe("Belt_internalMapInt.A.cmp", () => {
  test("Belt_internalMapInt.A.cmp", () => {
    module Test = {
      Belt.Array.cmp([1, 3, 5], [1, 4, 2], (a, b) => compare(a, b)) == -1

      Belt.Array.cmp([1, 3, 5], [1, 2, 3], (a, b) => compare(a, b)) == 1

      Belt.Array.cmp([1, 3, 5], [1, 3, 5], (a, b) => compare(a, b)) == 0
    }
    ()
  })
})

describe("Belt_internalMapInt.A.some2", () => {
  test("Belt_internalMapInt.A.some2", () => {
    module Test = {
      Belt.Array.some2([0, 2], [1, 0, 3], (a, b) => a > b) == true

      Belt.Array.some2([], [1], (x, y) => x > y) == false

      Belt.Array.some2([2, 3], [1, 4], (x, y) => x > y) == true
    }
    ()
  })
})

describe("Belt_internalMapInt.A.every2", () => {
  test("Belt_internalMapInt.A.every2", () => {
    module Test = {
      Belt.Array.every2([1, 2, 3], [0, 1], (a, b) => a > b) == true

      Belt.Array.every2([], [1], (x, y) => x > y) == true

      Belt.Array.every2([2, 3], [1], (x, y) => x > y) == true

      Belt.Array.every2([0, 1], [5, 0], (x, y) => x > y) == false
    }
    ()
  })
})

describe("Belt_internalMapInt.A.every", () => {
  test("Belt_internalMapInt.A.every", () => {
    module Test = {
      Belt.Array.every([1, 3, 5], x => mod(x, 2) == 1) == true

      Belt.Array.every([1, -3, 5], x => x > 0) == false
    }
    ()
  })
})

describe("Belt_internalMapInt.A.some", () => {
  test("Belt_internalMapInt.A.some", () => {
    module Test = {
      Belt.Array.some([2, 3, 4], x => mod(x, 2) == 1) == true

      Belt.Array.some([-1, -3, -5], x => x > 0) == false
    }
    ()
  })
})

describe("Belt_internalMapInt.A.joinWith", () => {
  test("Belt_internalMapInt.A.joinWith", () => {
    module Test = {
      Belt.Array.joinWith([0, 1], ", ", Js.Int.toString) == "0, 1"
      Belt.Array.joinWith([], " ", Js.Int.toString) == ""
      Belt.Array.joinWith([1], " ", Js.Int.toString) == "1"
    }
    ()
  })
})

describe("Belt_internalMapInt.A.reduceWithIndex", () => {
  test("Belt_internalMapInt.A.reduceWithIndex", () => {
    module Test = {
      Belt.Array.reduceWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i) == 16
    }
    ()
  })
})

describe("Belt_internalMapInt.A.reduceReverse2", () => {
  test("Belt_internalMapInt.A.reduceReverse2", () => {
    module Test = {
      Belt.Array.reduceReverse2([1, 2, 3], [1, 2], 0, (acc, x, y) => acc + x + y) == 6
    }
    ()
  })
})

describe("Belt_internalMapInt.A.reduceReverse", () => {
  test("Belt_internalMapInt.A.reduceReverse", () => {
    module Test = {
      Belt.Array.reduceReverse(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "dcba"
    }
    ()
  })
})

describe("Belt_internalMapInt.A.reduce", () => {
  test("Belt_internalMapInt.A.reduce", () => {
    module Test = {
      Belt.Array.reduce([2, 3, 4], 1, (a, b) => a + b) == 10

      Belt.Array.reduce(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "abcd"
    }
    ()
  })
})

describe("Belt_internalMapInt.A.partition", () => {
  test("Belt_internalMapInt.A.partition", () => {
    module Test = {
      Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) == 0) == ([2, 4], [1, 3, 5])

      Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) != 0) == ([1, 3, 5], [2, 4])
    }
    ()
  })
})

describe("Belt_internalMapInt.A.mapWithIndex", () => {
  test("Belt_internalMapInt.A.mapWithIndex", () => {
    module Test = {
      Belt.Array.mapWithIndex([1, 2, 3], (i, x) => i + x) == [0 + 1, 1 + 2, 2 + 3]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.forEachWithIndex", () => {
  test("Belt_internalMapInt.A.forEachWithIndex", () => {
    module Test = {
      Belt.Array.forEachWithIndex(
        ["a", "b", "c"],
        (i, x) => Js.log("Item " ++ Belt.Int.toString(i) ++ " is " ++ x),
      )

      /*
  prints:
  Item 0 is a
  Item 1 is b
  Item 2 is cc
*/
      let total = ref(0)

      Belt.Array.forEachWithIndex([10, 11, 12, 13], (i, x) => total := total.contents + x + i)

      total.contents == 0 + 10 + 1 + 11 + 2 + 12 + 3 + 13
    }
    ()
  })
})

describe("Belt_internalMapInt.A.keepMap", () => {
  test("Belt_internalMapInt.A.keepMap", () => {
    module Test = {
      Belt.Array.keepMap(
        [1, 2, 3],
        x =>
          if mod(x, 2) == 0 {
            Some(x)
          } else {
            None
          },
      ) == [2]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.keepWithIndex", () => {
  test("Belt_internalMapInt.A.keepWithIndex", () => {
    module Test = {
      Belt.Array.keepWithIndex([1, 2, 3], (_x, i) => i == 1) == [2]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.getIndexBy", () => {
  test("Belt_internalMapInt.A.getIndexBy", () => {
    module Test = {
      Belt.Array.getIndexBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(1)
      Belt.Array.getIndexBy([15, 13, 11], x => mod(x, 2) == 0) == None
    }
    ()
  })
})

describe("Belt_internalMapInt.A.getBy", () => {
  test("Belt_internalMapInt.A.getBy", () => {
    module Test = {
      Belt.Array.getBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(4)
      Belt.Array.getBy([15, 13, 11], x => mod(x, 2) == 0) == None
    }
    ()
  })
})

describe("Belt_internalMapInt.A.flatMap", () => {
  test("Belt_internalMapInt.A.flatMap", () => {
    module Test = {
      Belt.Array.flatMap([1, 2], x => [x + 10, x + 20]) == [11, 21, 12, 22]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.map", () => {
  test("Belt_internalMapInt.A.map", () => {
    module Test = {
      Belt.Array.map([1, 2], x => x + 1) == [3, 4]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.forEach", () => {
  test("Belt_internalMapInt.A.forEach", () => {
    module Test = {
      Belt.Array.forEach(["a", "b", "c"], x => Js.log("Item: " ++ x))

      /*
  prints:
  Item: a
  Item: b
  Item: c
*/
      let total = ref(0)

      Belt.Array.forEach([1, 2, 3, 4], x => total := total.contents + x)

      total.contents == 1 + 2 + 3 + 4
    }
    ()
  })
})

describe("Belt_internalMapInt.A.blit", () => {
  test("Belt_internalMapInt.A.blit", () => {
    module Test = {
      let v1 = [10, 11, 12, 13, 14, 15, 16, 17]
      let v2 = [20, 21, 22, 23, 24, 25, 26, 27]

      Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v2, ~dstOffset=2, ~len=3)
      v2 == [20, 21, 14, 15, 16, 25, 26, 27]

      Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v1, ~dstOffset=2, ~len=3)
      v1 == [10, 11, 14, 15, 16, 15, 16, 17]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.fill", () => {
  test("Belt_internalMapInt.A.fill", () => {
    module Test = {
      let arr = Belt.Array.makeBy(5, i => i)

      Belt.Array.fill(arr, ~offset=2, ~len=2, 9)

      arr == [0, 1, 9, 9, 4]

      Belt.Array.fill(arr, ~offset=7, ~len=2, 8)

      arr == [0, 1, 9, 9, 4]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.sliceToEnd", () => {
  test("Belt_internalMapInt.A.sliceToEnd", () => {
    module Test = {
      Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], 2) == [12, 13, 14, 15, 16]

      Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], -4) == [13, 14, 15, 16]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.slice", () => {
  test("Belt_internalMapInt.A.slice", () => {
    module Test = {
      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=2, ~len=3) == [12, 13, 14]

      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=-4, ~len=3) == [13, 14, 15]

      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=4, ~len=9) == [14, 15, 16]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.concatMany", () => {
  test("Belt_internalMapInt.A.concatMany", () => {
    module Test = {
      Belt.Array.concatMany([[1, 2, 3], [4, 5, 6], [7, 8]]) == [1, 2, 3, 4, 5, 6, 7, 8]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.concat", () => {
  test("Belt_internalMapInt.A.concat", () => {
    module Test = {
      Belt.Array.concat([1, 2, 3], [4, 5]) == [1, 2, 3, 4, 5]

      Belt.Array.concat([], ["a", "b", "c"]) == ["a", "b", "c"]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.unzip", () => {
  test("Belt_internalMapInt.A.unzip", () => {
    module Test = {
      Belt.Array.unzip([(1, 2), (3, 4)]) == ([1, 3], [2, 4])

      Belt.Array.unzip([(1, 2), (3, 4), (5, 6), (7, 8)]) == ([1, 3, 5, 7], [2, 4, 6, 8])
    }
    ()
  })
})

describe("Belt_internalMapInt.A.zipBy", () => {
  test("Belt_internalMapInt.A.zipBy", () => {
    module Test = {
      Belt.Array.zipBy([1, 2, 3], [4, 5], (a, b) => 2 * a + b) == [6, 9]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.zip", () => {
  test("Belt_internalMapInt.A.zip", () => {
    module Test = {
      Belt.Array.zip([1, 2], [3, 4, 5]) == [(1, 3), (2, 4)]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.makeBy", () => {
  test("Belt_internalMapInt.A.makeBy", () => {
    module Test = {
      Belt.Array.makeBy(5, i => i) == [0, 1, 2, 3, 4]

      Belt.Array.makeBy(5, i => i * i) == [0, 1, 4, 9, 16]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.rangeBy", () => {
  test("Belt_internalMapInt.A.rangeBy", () => {
    module Test = {
      Belt.Array.rangeBy(0, 10, ~step=3) == [0, 3, 6, 9]

      Belt.Array.rangeBy(0, 12, ~step=3) == [0, 3, 6, 9, 12]

      Belt.Array.rangeBy(33, 0, ~step=1) == []

      Belt.Array.rangeBy(33, 0, ~step=-1) == []

      Belt.Array.rangeBy(3, 12, ~step=-1) == []

      Belt.Array.rangeBy(3, 3, ~step=0) == []

      Belt.Array.rangeBy(3, 3, ~step=1) == [3]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.range", () => {
  test("Belt_internalMapInt.A.range", () => {
    module Test = {
      Belt.Array.range(0, 3) == [0, 1, 2, 3]

      Belt.Array.range(3, 0) == []

      Belt.Array.range(3, 3) == [3]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.makeUninitializedUnsafe", () => {
  test("Belt_internalMapInt.A.makeUninitializedUnsafe", () => {
    module Test = {
      let arr = Belt.Array.makeUninitializedUnsafe(5)

      Js.log(Belt.Array.getExn(arr, 0)) // undefined

      Belt.Array.setExn(arr, 0, "example")

      Js.log(Belt.Array.getExn(arr, 0) == "example")
    }
    ()
  })
})

describe("Belt_internalMapInt.A.makeUninitialized", () => {
  test("Belt_internalMapInt.A.makeUninitialized", () => {
    module Test = {
      let arr: array<Js.undefined<string>> = Belt.Array.makeUninitialized(5)

      Belt.Array.getExn(arr, 0) == Js.undefined
    }
    ()
  })
})

describe("Belt_internalMapInt.A.reverse", () => {
  test("Belt_internalMapInt.A.reverse", () => {
    module Test = {
      Belt.Array.reverse([10, 11, 12, 13, 14]) == [14, 13, 12, 11, 10]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.reverseInPlace", () => {
  test("Belt_internalMapInt.A.reverseInPlace", () => {
    module Test = {
      let arr = [10, 11, 12, 13, 14]

      let () = Belt.Array.reverseInPlace(arr)

      arr == [14, 13, 12, 11, 10]
    }
    ()
  })
})

describe("Belt_internalMapInt.A.get", () => {
  test("Belt_internalMapInt.A.get", () => {
    module Test = {
      Belt.Array.get(["a", "b", "c"], 0) == Some("a")
      Belt.Array.get(["a", "b", "c"], 3) == None
      Belt.Array.get(["a", "b", "c"], -1) == None
    }
    ()
  })
})

describe("Belt_internalMapInt.A.length", () => {
  test("Belt_internalMapInt.A.length", () => {
    module Test = {
      // Returns 1
      Belt.Array.length(["test"])
    }
    ()
  })
})

describe("Belt_internalMapString.S.binarySearchBy", () => {
  test("Belt_internalMapString.S.binarySearchBy", () => {
    module Test = {
      Belt.SortArray.binarySearchBy([1, 2, 3, 4, 33, 35, 36], 33, Pervasives.compare) == 4

      lnot(Belt.SortArray.binarySearchBy([1, 3, 5, 7], 4, Pervasives.compare)) == 2
    }
    ()
  })
})

describe("Belt_internalMapString.S.strictlySortedLength", () => {
  test("Belt_internalMapString.S.strictlySortedLength", () => {
    module Test = {
      Belt.SortArray.strictlySortedLength([1, 2, 3, 4, 3], (x, y) => x < y) == 4

      Belt.SortArray.strictlySortedLength([], (x, y) => x < y) == 0

      Belt.SortArray.strictlySortedLength([1], (x, y) => x < y) == 1

      Belt.SortArray.strictlySortedLength([4, 3, 2, 1], (x, y) => x < y) == -4
    }
    ()
  })
})

describe("Belt_internalMapString.A.truncateToLengthUnsafe", () => {
  test("Belt_internalMapString.A.truncateToLengthUnsafe", () => {
    module Test = {
      let arr = ["ant", "bee", "cat", "dog", "elk"]

      Belt.Array.truncateToLengthUnsafe(arr, 3)

      arr == ["ant", "bee", "cat"]
    }
    ()
  })
})

describe("Belt_internalMapString.A.eq", () => {
  test("Belt_internalMapString.A.eq", () => {
    module Test = {
      Belt.Array.eq([1, 2, 3], [-1, -2, -3], (a, b) => abs(a) == abs(b)) == true
    }
    ()
  })
})

describe("Belt_internalMapString.A.cmp", () => {
  test("Belt_internalMapString.A.cmp", () => {
    module Test = {
      Belt.Array.cmp([1, 3, 5], [1, 4, 2], (a, b) => compare(a, b)) == -1

      Belt.Array.cmp([1, 3, 5], [1, 2, 3], (a, b) => compare(a, b)) == 1

      Belt.Array.cmp([1, 3, 5], [1, 3, 5], (a, b) => compare(a, b)) == 0
    }
    ()
  })
})

describe("Belt_internalMapString.A.some2", () => {
  test("Belt_internalMapString.A.some2", () => {
    module Test = {
      Belt.Array.some2([0, 2], [1, 0, 3], (a, b) => a > b) == true

      Belt.Array.some2([], [1], (x, y) => x > y) == false

      Belt.Array.some2([2, 3], [1, 4], (x, y) => x > y) == true
    }
    ()
  })
})

describe("Belt_internalMapString.A.every2", () => {
  test("Belt_internalMapString.A.every2", () => {
    module Test = {
      Belt.Array.every2([1, 2, 3], [0, 1], (a, b) => a > b) == true

      Belt.Array.every2([], [1], (x, y) => x > y) == true

      Belt.Array.every2([2, 3], [1], (x, y) => x > y) == true

      Belt.Array.every2([0, 1], [5, 0], (x, y) => x > y) == false
    }
    ()
  })
})

describe("Belt_internalMapString.A.every", () => {
  test("Belt_internalMapString.A.every", () => {
    module Test = {
      Belt.Array.every([1, 3, 5], x => mod(x, 2) == 1) == true

      Belt.Array.every([1, -3, 5], x => x > 0) == false
    }
    ()
  })
})

describe("Belt_internalMapString.A.some", () => {
  test("Belt_internalMapString.A.some", () => {
    module Test = {
      Belt.Array.some([2, 3, 4], x => mod(x, 2) == 1) == true

      Belt.Array.some([-1, -3, -5], x => x > 0) == false
    }
    ()
  })
})

describe("Belt_internalMapString.A.joinWith", () => {
  test("Belt_internalMapString.A.joinWith", () => {
    module Test = {
      Belt.Array.joinWith([0, 1], ", ", Js.Int.toString) == "0, 1"
      Belt.Array.joinWith([], " ", Js.Int.toString) == ""
      Belt.Array.joinWith([1], " ", Js.Int.toString) == "1"
    }
    ()
  })
})

describe("Belt_internalMapString.A.reduceWithIndex", () => {
  test("Belt_internalMapString.A.reduceWithIndex", () => {
    module Test = {
      Belt.Array.reduceWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i) == 16
    }
    ()
  })
})

describe("Belt_internalMapString.A.reduceReverse2", () => {
  test("Belt_internalMapString.A.reduceReverse2", () => {
    module Test = {
      Belt.Array.reduceReverse2([1, 2, 3], [1, 2], 0, (acc, x, y) => acc + x + y) == 6
    }
    ()
  })
})

describe("Belt_internalMapString.A.reduceReverse", () => {
  test("Belt_internalMapString.A.reduceReverse", () => {
    module Test = {
      Belt.Array.reduceReverse(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "dcba"
    }
    ()
  })
})

describe("Belt_internalMapString.A.reduce", () => {
  test("Belt_internalMapString.A.reduce", () => {
    module Test = {
      Belt.Array.reduce([2, 3, 4], 1, (a, b) => a + b) == 10

      Belt.Array.reduce(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "abcd"
    }
    ()
  })
})

describe("Belt_internalMapString.A.partition", () => {
  test("Belt_internalMapString.A.partition", () => {
    module Test = {
      Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) == 0) == ([2, 4], [1, 3, 5])

      Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) != 0) == ([1, 3, 5], [2, 4])
    }
    ()
  })
})

describe("Belt_internalMapString.A.mapWithIndex", () => {
  test("Belt_internalMapString.A.mapWithIndex", () => {
    module Test = {
      Belt.Array.mapWithIndex([1, 2, 3], (i, x) => i + x) == [0 + 1, 1 + 2, 2 + 3]
    }
    ()
  })
})

describe("Belt_internalMapString.A.forEachWithIndex", () => {
  test("Belt_internalMapString.A.forEachWithIndex", () => {
    module Test = {
      Belt.Array.forEachWithIndex(
        ["a", "b", "c"],
        (i, x) => Js.log("Item " ++ Belt.Int.toString(i) ++ " is " ++ x),
      )

      /*
  prints:
  Item 0 is a
  Item 1 is b
  Item 2 is cc
*/
      let total = ref(0)

      Belt.Array.forEachWithIndex([10, 11, 12, 13], (i, x) => total := total.contents + x + i)

      total.contents == 0 + 10 + 1 + 11 + 2 + 12 + 3 + 13
    }
    ()
  })
})

describe("Belt_internalMapString.A.keepMap", () => {
  test("Belt_internalMapString.A.keepMap", () => {
    module Test = {
      Belt.Array.keepMap(
        [1, 2, 3],
        x =>
          if mod(x, 2) == 0 {
            Some(x)
          } else {
            None
          },
      ) == [2]
    }
    ()
  })
})

describe("Belt_internalMapString.A.keepWithIndex", () => {
  test("Belt_internalMapString.A.keepWithIndex", () => {
    module Test = {
      Belt.Array.keepWithIndex([1, 2, 3], (_x, i) => i == 1) == [2]
    }
    ()
  })
})

describe("Belt_internalMapString.A.getIndexBy", () => {
  test("Belt_internalMapString.A.getIndexBy", () => {
    module Test = {
      Belt.Array.getIndexBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(1)
      Belt.Array.getIndexBy([15, 13, 11], x => mod(x, 2) == 0) == None
    }
    ()
  })
})

describe("Belt_internalMapString.A.getBy", () => {
  test("Belt_internalMapString.A.getBy", () => {
    module Test = {
      Belt.Array.getBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(4)
      Belt.Array.getBy([15, 13, 11], x => mod(x, 2) == 0) == None
    }
    ()
  })
})

describe("Belt_internalMapString.A.flatMap", () => {
  test("Belt_internalMapString.A.flatMap", () => {
    module Test = {
      Belt.Array.flatMap([1, 2], x => [x + 10, x + 20]) == [11, 21, 12, 22]
    }
    ()
  })
})

describe("Belt_internalMapString.A.map", () => {
  test("Belt_internalMapString.A.map", () => {
    module Test = {
      Belt.Array.map([1, 2], x => x + 1) == [3, 4]
    }
    ()
  })
})

describe("Belt_internalMapString.A.forEach", () => {
  test("Belt_internalMapString.A.forEach", () => {
    module Test = {
      Belt.Array.forEach(["a", "b", "c"], x => Js.log("Item: " ++ x))

      /*
  prints:
  Item: a
  Item: b
  Item: c
*/
      let total = ref(0)

      Belt.Array.forEach([1, 2, 3, 4], x => total := total.contents + x)

      total.contents == 1 + 2 + 3 + 4
    }
    ()
  })
})

describe("Belt_internalMapString.A.blit", () => {
  test("Belt_internalMapString.A.blit", () => {
    module Test = {
      let v1 = [10, 11, 12, 13, 14, 15, 16, 17]
      let v2 = [20, 21, 22, 23, 24, 25, 26, 27]

      Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v2, ~dstOffset=2, ~len=3)
      v2 == [20, 21, 14, 15, 16, 25, 26, 27]

      Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v1, ~dstOffset=2, ~len=3)
      v1 == [10, 11, 14, 15, 16, 15, 16, 17]
    }
    ()
  })
})

describe("Belt_internalMapString.A.fill", () => {
  test("Belt_internalMapString.A.fill", () => {
    module Test = {
      let arr = Belt.Array.makeBy(5, i => i)

      Belt.Array.fill(arr, ~offset=2, ~len=2, 9)

      arr == [0, 1, 9, 9, 4]

      Belt.Array.fill(arr, ~offset=7, ~len=2, 8)

      arr == [0, 1, 9, 9, 4]
    }
    ()
  })
})

describe("Belt_internalMapString.A.sliceToEnd", () => {
  test("Belt_internalMapString.A.sliceToEnd", () => {
    module Test = {
      Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], 2) == [12, 13, 14, 15, 16]

      Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], -4) == [13, 14, 15, 16]
    }
    ()
  })
})

describe("Belt_internalMapString.A.slice", () => {
  test("Belt_internalMapString.A.slice", () => {
    module Test = {
      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=2, ~len=3) == [12, 13, 14]

      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=-4, ~len=3) == [13, 14, 15]

      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=4, ~len=9) == [14, 15, 16]
    }
    ()
  })
})

describe("Belt_internalMapString.A.concatMany", () => {
  test("Belt_internalMapString.A.concatMany", () => {
    module Test = {
      Belt.Array.concatMany([[1, 2, 3], [4, 5, 6], [7, 8]]) == [1, 2, 3, 4, 5, 6, 7, 8]
    }
    ()
  })
})

describe("Belt_internalMapString.A.concat", () => {
  test("Belt_internalMapString.A.concat", () => {
    module Test = {
      Belt.Array.concat([1, 2, 3], [4, 5]) == [1, 2, 3, 4, 5]

      Belt.Array.concat([], ["a", "b", "c"]) == ["a", "b", "c"]
    }
    ()
  })
})

describe("Belt_internalMapString.A.unzip", () => {
  test("Belt_internalMapString.A.unzip", () => {
    module Test = {
      Belt.Array.unzip([(1, 2), (3, 4)]) == ([1, 3], [2, 4])

      Belt.Array.unzip([(1, 2), (3, 4), (5, 6), (7, 8)]) == ([1, 3, 5, 7], [2, 4, 6, 8])
    }
    ()
  })
})

describe("Belt_internalMapString.A.zipBy", () => {
  test("Belt_internalMapString.A.zipBy", () => {
    module Test = {
      Belt.Array.zipBy([1, 2, 3], [4, 5], (a, b) => 2 * a + b) == [6, 9]
    }
    ()
  })
})

describe("Belt_internalMapString.A.zip", () => {
  test("Belt_internalMapString.A.zip", () => {
    module Test = {
      Belt.Array.zip([1, 2], [3, 4, 5]) == [(1, 3), (2, 4)]
    }
    ()
  })
})

describe("Belt_internalMapString.A.makeBy", () => {
  test("Belt_internalMapString.A.makeBy", () => {
    module Test = {
      Belt.Array.makeBy(5, i => i) == [0, 1, 2, 3, 4]

      Belt.Array.makeBy(5, i => i * i) == [0, 1, 4, 9, 16]
    }
    ()
  })
})

describe("Belt_internalMapString.A.rangeBy", () => {
  test("Belt_internalMapString.A.rangeBy", () => {
    module Test = {
      Belt.Array.rangeBy(0, 10, ~step=3) == [0, 3, 6, 9]

      Belt.Array.rangeBy(0, 12, ~step=3) == [0, 3, 6, 9, 12]

      Belt.Array.rangeBy(33, 0, ~step=1) == []

      Belt.Array.rangeBy(33, 0, ~step=-1) == []

      Belt.Array.rangeBy(3, 12, ~step=-1) == []

      Belt.Array.rangeBy(3, 3, ~step=0) == []

      Belt.Array.rangeBy(3, 3, ~step=1) == [3]
    }
    ()
  })
})

describe("Belt_internalMapString.A.range", () => {
  test("Belt_internalMapString.A.range", () => {
    module Test = {
      Belt.Array.range(0, 3) == [0, 1, 2, 3]

      Belt.Array.range(3, 0) == []

      Belt.Array.range(3, 3) == [3]
    }
    ()
  })
})

describe("Belt_internalMapString.A.makeUninitializedUnsafe", () => {
  test("Belt_internalMapString.A.makeUninitializedUnsafe", () => {
    module Test = {
      let arr = Belt.Array.makeUninitializedUnsafe(5)

      Js.log(Belt.Array.getExn(arr, 0)) // undefined

      Belt.Array.setExn(arr, 0, "example")

      Js.log(Belt.Array.getExn(arr, 0) == "example")
    }
    ()
  })
})

describe("Belt_internalMapString.A.makeUninitialized", () => {
  test("Belt_internalMapString.A.makeUninitialized", () => {
    module Test = {
      let arr: array<Js.undefined<string>> = Belt.Array.makeUninitialized(5)

      Belt.Array.getExn(arr, 0) == Js.undefined
    }
    ()
  })
})

describe("Belt_internalMapString.A.reverse", () => {
  test("Belt_internalMapString.A.reverse", () => {
    module Test = {
      Belt.Array.reverse([10, 11, 12, 13, 14]) == [14, 13, 12, 11, 10]
    }
    ()
  })
})

describe("Belt_internalMapString.A.reverseInPlace", () => {
  test("Belt_internalMapString.A.reverseInPlace", () => {
    module Test = {
      let arr = [10, 11, 12, 13, 14]

      let () = Belt.Array.reverseInPlace(arr)

      arr == [14, 13, 12, 11, 10]
    }
    ()
  })
})

describe("Belt_internalMapString.A.get", () => {
  test("Belt_internalMapString.A.get", () => {
    module Test = {
      Belt.Array.get(["a", "b", "c"], 0) == Some("a")
      Belt.Array.get(["a", "b", "c"], 3) == None
      Belt.Array.get(["a", "b", "c"], -1) == None
    }
    ()
  })
})

describe("Belt_internalMapString.A.length", () => {
  test("Belt_internalMapString.A.length", () => {
    module Test = {
      // Returns 1
      Belt.Array.length(["test"])
    }
    ()
  })
})

describe("Belt_internalSetInt.A.truncateToLengthUnsafe", () => {
  test("Belt_internalSetInt.A.truncateToLengthUnsafe", () => {
    module Test = {
      let arr = ["ant", "bee", "cat", "dog", "elk"]

      Belt.Array.truncateToLengthUnsafe(arr, 3)

      arr == ["ant", "bee", "cat"]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.eq", () => {
  test("Belt_internalSetInt.A.eq", () => {
    module Test = {
      Belt.Array.eq([1, 2, 3], [-1, -2, -3], (a, b) => abs(a) == abs(b)) == true
    }
    ()
  })
})

describe("Belt_internalSetInt.A.cmp", () => {
  test("Belt_internalSetInt.A.cmp", () => {
    module Test = {
      Belt.Array.cmp([1, 3, 5], [1, 4, 2], (a, b) => compare(a, b)) == -1

      Belt.Array.cmp([1, 3, 5], [1, 2, 3], (a, b) => compare(a, b)) == 1

      Belt.Array.cmp([1, 3, 5], [1, 3, 5], (a, b) => compare(a, b)) == 0
    }
    ()
  })
})

describe("Belt_internalSetInt.A.some2", () => {
  test("Belt_internalSetInt.A.some2", () => {
    module Test = {
      Belt.Array.some2([0, 2], [1, 0, 3], (a, b) => a > b) == true

      Belt.Array.some2([], [1], (x, y) => x > y) == false

      Belt.Array.some2([2, 3], [1, 4], (x, y) => x > y) == true
    }
    ()
  })
})

describe("Belt_internalSetInt.A.every2", () => {
  test("Belt_internalSetInt.A.every2", () => {
    module Test = {
      Belt.Array.every2([1, 2, 3], [0, 1], (a, b) => a > b) == true

      Belt.Array.every2([], [1], (x, y) => x > y) == true

      Belt.Array.every2([2, 3], [1], (x, y) => x > y) == true

      Belt.Array.every2([0, 1], [5, 0], (x, y) => x > y) == false
    }
    ()
  })
})

describe("Belt_internalSetInt.A.every", () => {
  test("Belt_internalSetInt.A.every", () => {
    module Test = {
      Belt.Array.every([1, 3, 5], x => mod(x, 2) == 1) == true

      Belt.Array.every([1, -3, 5], x => x > 0) == false
    }
    ()
  })
})

describe("Belt_internalSetInt.A.some", () => {
  test("Belt_internalSetInt.A.some", () => {
    module Test = {
      Belt.Array.some([2, 3, 4], x => mod(x, 2) == 1) == true

      Belt.Array.some([-1, -3, -5], x => x > 0) == false
    }
    ()
  })
})

describe("Belt_internalSetInt.A.joinWith", () => {
  test("Belt_internalSetInt.A.joinWith", () => {
    module Test = {
      Belt.Array.joinWith([0, 1], ", ", Js.Int.toString) == "0, 1"
      Belt.Array.joinWith([], " ", Js.Int.toString) == ""
      Belt.Array.joinWith([1], " ", Js.Int.toString) == "1"
    }
    ()
  })
})

describe("Belt_internalSetInt.A.reduceWithIndex", () => {
  test("Belt_internalSetInt.A.reduceWithIndex", () => {
    module Test = {
      Belt.Array.reduceWithIndex([1, 2, 3, 4], 0, (acc, x, i) => acc + x + i) == 16
    }
    ()
  })
})

describe("Belt_internalSetInt.A.reduceReverse2", () => {
  test("Belt_internalSetInt.A.reduceReverse2", () => {
    module Test = {
      Belt.Array.reduceReverse2([1, 2, 3], [1, 2], 0, (acc, x, y) => acc + x + y) == 6
    }
    ()
  })
})

describe("Belt_internalSetInt.A.reduceReverse", () => {
  test("Belt_internalSetInt.A.reduceReverse", () => {
    module Test = {
      Belt.Array.reduceReverse(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "dcba"
    }
    ()
  })
})

describe("Belt_internalSetInt.A.reduce", () => {
  test("Belt_internalSetInt.A.reduce", () => {
    module Test = {
      Belt.Array.reduce([2, 3, 4], 1, (a, b) => a + b) == 10

      Belt.Array.reduce(["a", "b", "c", "d"], "", (a, b) => a ++ b) == "abcd"
    }
    ()
  })
})

describe("Belt_internalSetInt.A.partition", () => {
  test("Belt_internalSetInt.A.partition", () => {
    module Test = {
      Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) == 0) == ([2, 4], [1, 3, 5])

      Belt.Array.partition([1, 2, 3, 4, 5], x => mod(x, 2) != 0) == ([1, 3, 5], [2, 4])
    }
    ()
  })
})

describe("Belt_internalSetInt.A.mapWithIndex", () => {
  test("Belt_internalSetInt.A.mapWithIndex", () => {
    module Test = {
      Belt.Array.mapWithIndex([1, 2, 3], (i, x) => i + x) == [0 + 1, 1 + 2, 2 + 3]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.forEachWithIndex", () => {
  test("Belt_internalSetInt.A.forEachWithIndex", () => {
    module Test = {
      Belt.Array.forEachWithIndex(
        ["a", "b", "c"],
        (i, x) => Js.log("Item " ++ Belt.Int.toString(i) ++ " is " ++ x),
      )

      /*
  prints:
  Item 0 is a
  Item 1 is b
  Item 2 is cc
*/
      let total = ref(0)

      Belt.Array.forEachWithIndex([10, 11, 12, 13], (i, x) => total := total.contents + x + i)

      total.contents == 0 + 10 + 1 + 11 + 2 + 12 + 3 + 13
    }
    ()
  })
})

describe("Belt_internalSetInt.A.keepMap", () => {
  test("Belt_internalSetInt.A.keepMap", () => {
    module Test = {
      Belt.Array.keepMap(
        [1, 2, 3],
        x =>
          if mod(x, 2) == 0 {
            Some(x)
          } else {
            None
          },
      ) == [2]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.keepWithIndex", () => {
  test("Belt_internalSetInt.A.keepWithIndex", () => {
    module Test = {
      Belt.Array.keepWithIndex([1, 2, 3], (_x, i) => i == 1) == [2]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.getIndexBy", () => {
  test("Belt_internalSetInt.A.getIndexBy", () => {
    module Test = {
      Belt.Array.getIndexBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(1)
      Belt.Array.getIndexBy([15, 13, 11], x => mod(x, 2) == 0) == None
    }
    ()
  })
})

describe("Belt_internalSetInt.A.getBy", () => {
  test("Belt_internalSetInt.A.getBy", () => {
    module Test = {
      Belt.Array.getBy([1, 4, 3, 2], x => mod(x, 2) == 0) == Some(4)
      Belt.Array.getBy([15, 13, 11], x => mod(x, 2) == 0) == None
    }
    ()
  })
})

describe("Belt_internalSetInt.A.flatMap", () => {
  test("Belt_internalSetInt.A.flatMap", () => {
    module Test = {
      Belt.Array.flatMap([1, 2], x => [x + 10, x + 20]) == [11, 21, 12, 22]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.map", () => {
  test("Belt_internalSetInt.A.map", () => {
    module Test = {
      Belt.Array.map([1, 2], x => x + 1) == [3, 4]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.forEach", () => {
  test("Belt_internalSetInt.A.forEach", () => {
    module Test = {
      Belt.Array.forEach(["a", "b", "c"], x => Js.log("Item: " ++ x))

      /*
  prints:
  Item: a
  Item: b
  Item: c
*/
      let total = ref(0)

      Belt.Array.forEach([1, 2, 3, 4], x => total := total.contents + x)

      total.contents == 1 + 2 + 3 + 4
    }
    ()
  })
})

describe("Belt_internalSetInt.A.blit", () => {
  test("Belt_internalSetInt.A.blit", () => {
    module Test = {
      let v1 = [10, 11, 12, 13, 14, 15, 16, 17]
      let v2 = [20, 21, 22, 23, 24, 25, 26, 27]

      Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v2, ~dstOffset=2, ~len=3)
      v2 == [20, 21, 14, 15, 16, 25, 26, 27]

      Belt.Array.blit(~src=v1, ~srcOffset=4, ~dst=v1, ~dstOffset=2, ~len=3)
      v1 == [10, 11, 14, 15, 16, 15, 16, 17]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.fill", () => {
  test("Belt_internalSetInt.A.fill", () => {
    module Test = {
      let arr = Belt.Array.makeBy(5, i => i)

      Belt.Array.fill(arr, ~offset=2, ~len=2, 9)

      arr == [0, 1, 9, 9, 4]

      Belt.Array.fill(arr, ~offset=7, ~len=2, 8)

      arr == [0, 1, 9, 9, 4]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.sliceToEnd", () => {
  test("Belt_internalSetInt.A.sliceToEnd", () => {
    module Test = {
      Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], 2) == [12, 13, 14, 15, 16]

      Belt.Array.sliceToEnd([10, 11, 12, 13, 14, 15, 16], -4) == [13, 14, 15, 16]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.slice", () => {
  test("Belt_internalSetInt.A.slice", () => {
    module Test = {
      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=2, ~len=3) == [12, 13, 14]

      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=-4, ~len=3) == [13, 14, 15]

      Belt.Array.slice([10, 11, 12, 13, 14, 15, 16], ~offset=4, ~len=9) == [14, 15, 16]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.concatMany", () => {
  test("Belt_internalSetInt.A.concatMany", () => {
    module Test = {
      Belt.Array.concatMany([[1, 2, 3], [4, 5, 6], [7, 8]]) == [1, 2, 3, 4, 5, 6, 7, 8]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.concat", () => {
  test("Belt_internalSetInt.A.concat", () => {
    module Test = {
      Belt.Array.concat([1, 2, 3], [4, 5]) == [1, 2, 3, 4, 5]

      Belt.Array.concat([], ["a", "b", "c"]) == ["a", "b", "c"]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.unzip", () => {
  test("Belt_internalSetInt.A.unzip", () => {
    module Test = {
      Belt.Array.unzip([(1, 2), (3, 4)]) == ([1, 3], [2, 4])

      Belt.Array.unzip([(1, 2), (3, 4), (5, 6), (7, 8)]) == ([1, 3, 5, 7], [2, 4, 6, 8])
    }
    ()
  })
})

describe("Belt_internalSetInt.A.zipBy", () => {
  test("Belt_internalSetInt.A.zipBy", () => {
    module Test = {
      Belt.Array.zipBy([1, 2, 3], [4, 5], (a, b) => 2 * a + b) == [6, 9]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.zip", () => {
  test("Belt_internalSetInt.A.zip", () => {
    module Test = {
      Belt.Array.zip([1, 2], [3, 4, 5]) == [(1, 3), (2, 4)]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.makeBy", () => {
  test("Belt_internalSetInt.A.makeBy", () => {
    module Test = {
      Belt.Array.makeBy(5, i => i) == [0, 1, 2, 3, 4]

      Belt.Array.makeBy(5, i => i * i) == [0, 1, 4, 9, 16]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.rangeBy", () => {
  test("Belt_internalSetInt.A.rangeBy", () => {
    module Test = {
      Belt.Array.rangeBy(0, 10, ~step=3) == [0, 3, 6, 9]

      Belt.Array.rangeBy(0, 12, ~step=3) == [0, 3, 6, 9, 12]

      Belt.Array.rangeBy(33, 0, ~step=1) == []

      Belt.Array.rangeBy(33, 0, ~step=-1) == []

      Belt.Array.rangeBy(3, 12, ~step=-1) == []

      Belt.Array.rangeBy(3, 3, ~step=0) == []

      Belt.Array.rangeBy(3, 3, ~step=1) == [3]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.range", () => {
  test("Belt_internalSetInt.A.range", () => {
    module Test = {
      Belt.Array.range(0, 3) == [0, 1, 2, 3]

      Belt.Array.range(3, 0) == []

      Belt.Array.range(3, 3) == [3]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.makeUninitializedUnsafe", () => {
  test("Belt_internalSetInt.A.makeUninitializedUnsafe", () => {
    module Test = {
      let arr = Belt.Array.makeUninitializedUnsafe(5)

      Js.log(Belt.Array.getExn(arr, 0)) // undefined

      Belt.Array.setExn(arr, 0, "example")

      Js.log(Belt.Array.getExn(arr, 0) == "example")
    }
    ()
  })
})

describe("Belt_internalSetInt.A.makeUninitialized", () => {
  test("Belt_internalSetInt.A.makeUninitialized", () => {
    module Test = {
      let arr: array<Js.undefined<string>> = Belt.Array.makeUninitialized(5)

      Belt.Array.getExn(arr, 0) == Js.undefined
    }
    ()
  })
})

describe("Belt_internalSetInt.A.reverse", () => {
  test("Belt_internalSetInt.A.reverse", () => {
    module Test = {
      Belt.Array.reverse([10, 11, 12, 13, 14]) == [14, 13, 12, 11, 10]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.reverseInPlace", () => {
  test("Belt_internalSetInt.A.reverseInPlace", () => {
    module Test = {
      let arr = [10, 11, 12, 13, 14]

      let () = Belt.Array.reverseInPlace(arr)

      arr == [14, 13, 12, 11, 10]
    }
    ()
  })
})

describe("Belt_internalSetInt.A.get", () => {
  test("Belt_internalSetInt.A.get", () => {
    module Test = {
      Belt.Array.get(["a", "b", "c"], 0) == Some("a")
      Belt.Array.get(["a", "b", "c"], 3) == None
      Belt.Array.get(["a", "b", "c"], -1) == None
    }
    ()
  })
})

describe("Belt_internalSetInt.A.length", () => {
  test("Belt_internalSetInt.A.length", () => {
    module Test = {
      // Returns 1
      Belt.Array.length(["test"])
    }
    ()
  })
})

describe("Console.warnMany", () => {
  test("Console.warnMany", () => {
    module Test = {
      Console.warnMany(["Hello", "World"])
      Console.warnMany([1, 2, 3])
    }
    ()
  })
})

describe("Console.warn6", () => {
  test("Console.warn6", () => {
    module Test = {
      Console.warn6("Hello", "World", "from", "JS", "!!!", '!')
      Console.warn6([1, 2], (3, 4), [#5, #6], #polyvar, {"name": "ReScript"}, 42)
    }
    ()
  })
})

describe("Console.warn5", () => {
  test("Console.warn5", () => {
    module Test = {
      Console.warn5("Hello", "World", "from", "JS", "!!!")
      Console.warn5([1, 2], (3, 4), [#5, #6], #polyvar, {"name": "ReScript"})
    }
    ()
  })
})

describe("Console.warn4", () => {
  test("Console.warn4", () => {
    module Test = {
      Console.warn4("Hello", "World", "ReScript", "!!!")
      Console.warn4(#first, #second, #third, "fourth")
    }
    ()
  })
})

describe("Console.warn3", () => {
  test("Console.warn3", () => {
    module Test = {
      Console.warn3("Hello", "World", "ReScript")
      Console.warn3([1, 2, 3], #4, #5)
    }
    ()
  })
})

describe("Console.warn2", () => {
  test("Console.warn2", () => {
    module Test = {
      Console.warn2("Hello", "World")
      Console.warn2([1, 2, 3], 4)
    }
    ()
  })
})

describe("Console.warn", () => {
  test("Console.warn", () => {
    module Test = {
      Console.warn("Warning")
      Console.warn(("Warning", "invalid number"))
    }
    ()
  })
})

describe("Console.trace", () => {
  test("Console.trace", () => {
    module Test = {
      let main = () => {
        Console.trace()
      }
      main()
      // In the console, the following trace will be displayed:
      // main
      // <anonymous>
    }
    ()
  })
})

describe("Console.timeLog", () => {
  test("Console.timeLog", () => {
    module Test = {
      Console.time("for_time")
      for x in 3 downto 1 {
        Console.log(x)
        Console.timeLog("for_time")
      }
      Console.timeEnd("for_time")
    }
    ()
  })
})

describe("Console.timeEnd", () => {
  test("Console.timeEnd", () => {
    module Test = {
      Console.time("for_time")
      for x in 3 downto 1 {
        Console.log(x)
        Console.timeLog("for_time")
      }
      Console.timeEnd("for_time")
    }
    ()
  })
})

describe("Console.time", () => {
  test("Console.time", () => {
    module Test = {
      Console.time("for_time")
      for x in 3 downto 1 {
        Console.log(x)
        Console.timeLog("for_time")
      }
      Console.timeEnd("for_time")
    }
    ()
  })
})

describe("Console.table", () => {
  test("Console.table", () => {
    module Test = {
      Console.table({"language": "rescript", "version": "10.1.2"})
    }
    ()
  })
})

describe("Console.logMany", () => {
  test("Console.logMany", () => {
    module Test = {
      Console.logMany(["Hello", "World"])
      Console.logMany([1, 2, 3])
    }
    ()
  })
})

describe("Console.log6", () => {
  test("Console.log6", () => {
    module Test = {
      Console.log6("Hello", "World", "JS", '!', '!', '?')
      Console.log6([1, 2], (3, 4), [#5, #6], #polyvar, {"name": "ReScript"}, 42)
    }
    ()
  })
})

describe("Console.log5", () => {
  test("Console.log5", () => {
    module Test = {
      Console.log5("Hello", "World", "JS", '!', '!')
      Console.log5([1, 2], (3, 4), [#5, #6], #polyvar, {"name": "ReScript"})
    }
    ()
  })
})

describe("Console.log4", () => {
  test("Console.log4", () => {
    module Test = {
      Console.log4("Hello", "World", "ReScript", "!!!")
      Console.log4([1, 2], (3, 4), [#5, #6], #polyvar)
    }
    ()
  })
})

describe("Console.log3", () => {
  test("Console.log3", () => {
    module Test = {
      Console.log3("Hello", "World", "ReScript")
      Console.log3("One", 2, #3)
    }
    ()
  })
})

describe("Console.log2", () => {
  test("Console.log2", () => {
    module Test = {
      Console.log2("Hello", "World")
      Console.log2([1, 2, 3], '4')
    }
    ()
  })
})

describe("Console.log", () => {
  test("Console.log", () => {
    module Test = {
      Console.log("Hello")
      let obj = {"name": "ReScript", "version": 10}
      Console.log(obj)
    }
    ()
  })
})

describe("Console.infoMany", () => {
  test("Console.infoMany", () => {
    module Test = {
      Console.infoMany(["Hello", "World"])
      Console.infoMany([1, 2, 3])
    }
    ()
  })
})

describe("Console.info6", () => {
  test("Console.info6", () => {
    module Test = {
      Console.info6("Hello", "World", "from", "JS", "!!!", '!')
      Console.info6([1, 2], (3, 4), [#5, #6], #polyvar, {"name": "ReScript"}, 42)
    }
    ()
  })
})

describe("Console.info5", () => {
  test("Console.info5", () => {
    module Test = {
      Console.info5("Hello", "World", "from", "JS", "!!!")
      Console.info5([1, 2], (3, 4), [#5, #6], #polyvar, {"name": "ReScript"})
    }
    ()
  })
})

describe("Console.info4", () => {
  test("Console.info4", () => {
    module Test = {
      Console.info4("Hello", "World", "ReScript", '!')
      Console.info4([1, 2, 3], #4, #5, #lastinfo)
    }
    ()
  })
})

describe("Console.info3", () => {
  test("Console.info3", () => {
    module Test = {
      Console.info3("Hello", "World", "ReScript")
      Console.info3([1, 2, 3], #4, #5)
    }
    ()
  })
})

describe("Console.info2", () => {
  test("Console.info2", () => {
    module Test = {
      Console.info2("Info", "failed to download")
      Console.info2(#info, {"name": "ReScript"})
    }
    ()
  })
})

describe("Console.info", () => {
  test("Console.info", () => {
    module Test = {
      Console.info("Information")
      Console.info(("Hello", "JS"))
    }
    ()
  })
})

describe("Console.errorMany", () => {
  test("Console.errorMany", () => {
    module Test = {
      Console.errorMany(["Hello", "World"])
      Console.errorMany([1, 2, 3])
    }
    ()
  })
})

describe("Console.group", () => {
  test("Console.group", () => {
    module Test = {
      Console.group("first group")
      Console.group("second group")
      Console.log("a message on the second level")
      Console.groupEnd()
      Console.log("a message message on the first level")
      Console.groupEnd()
    }
    ()
  })
})

describe("Console.error6", () => {
  test("Console.error6", () => {
    module Test = {
      Console.error6("Hello", "World", "from", "JS", "!!!", '!')
      Console.error6([1, 2], (3, 4), [#5, #6], #polyvar, {"name": "ReScript"}, 42)
    }
    ()
  })
})

describe("Console.error5", () => {
  test("Console.error5", () => {
    module Test = {
      Console.error5('e', 'r', 'r', 'o', 'r')
      Console.error5(1, #second, #third, "fourth", 'c')
    }
    ()
  })
})

describe("Console.error4", () => {
  test("Console.error4", () => {
    module Test = {
      Console.error4("Hello", "World", "ReScript", '!')
      Console.error4(#first, #second, #third, "fourth")
    }
    ()
  })
})

describe("Console.error3", () => {
  test("Console.error3", () => {
    module Test = {
      Console.error3("Hello", "World", "!!!")
      Console.error3(#first, #second, #third)
    }
    ()
  })
})

describe("Console.error2", () => {
  test("Console.error2", () => {
    module Test = {
      Console.error2("Error", "here")
      Console.error2(("log", "error"), "message")
    }
    ()
  })
})

describe("Console.error", () => {
  test("Console.error", () => {
    module Test = {
      Console.error("error message")
      Console.error(("error", "invalid value"))
    }
    ()
  })
})

describe("Console.dir", () => {
  test("Console.dir", () => {
    module Test = {
      Console.dir({"language": "rescript", "version": "10.1.2"})
    }
    ()
  })
})

describe("Console.debugMany", () => {
  test("Console.debugMany", () => {
    module Test = {
      Console.debugMany(["Hello", "World"])
      Console.debugMany([1, 2, 3])
    }
    ()
  })
})

describe("Console.debug6", () => {
  test("Console.debug6", () => {
    module Test = {
      Console.debug6("Hello", "World", "JS", '!', '!', '?')
      Console.debug6([1, 2], (3, 4), [#5, #6], #polyvar, {"name": "ReScript"}, 42)
    }
    ()
  })
})

describe("Console.debug5", () => {
  test("Console.debug5", () => {
    module Test = {
      Console.debug5("Hello", "World", "JS", '!', '!')
      Console.debug5([1, 2], (3, 4), [#5, #6], #polyvar, {"name": "ReScript"})
    }
    ()
  })
})

describe("Console.debug4", () => {
  test("Console.debug4", () => {
    module Test = {
      Console.debug4("Hello", "World", "ReScript", "!!!")
      Console.debug4([1, 2], (3, 4), [#5, #6], #polyvar)
    }
    ()
  })
})

describe("Console.debug3", () => {
  test("Console.debug3", () => {
    module Test = {
      Console.debug3("Hello", "World", "ReScript")
      Console.debug3("One", 2, #3)
    }
    ()
  })
})

describe("Console.debug2", () => {
  test("Console.debug2", () => {
    module Test = {
      Console.debug2("Hello", "World")
      Console.debug2([1, 2, 3], '4')
    }
    ()
  })
})

describe("Console.debug", () => {
  test("Console.debug", () => {
    module Test = {
      Console.debug("Hello")
      let obj = {"name": "ReScript", "version": 10}
      Console.debug(obj)
    }
    ()
  })
})

describe("Console.countReset", () => {
  test("Console.countReset", () => {
    module Test = {
      Console.countReset("rescript")
    }
    ()
  })
})

describe("Console.count", () => {
  test("Console.count", () => {
    module Test = {
      Console.count("rescript")
    }
    ()
  })
})

describe("Console.clear", () => {
  test("Console.clear", () => {
    module Test = {
      Console.clear()
    }
    ()
  })
})

describe("Console.assertMany", () => {
  test("Console.assertMany", () => {
    module Test = {
      let value = 42
      Console.assertMany(false, ["Hello", "World"])
      Console.assertMany(value == 42, [1, 2, 3])
    }
    ()
  })
})

describe("Console.assert6", () => {
  test("Console.assert6", () => {
    module Test = {
      let value = 42
      Console.assert6(false, "Hello", "World", "JS", '!', '!', '?')
      Console.assert6(value == 42, [1, 2], (3, 4), [#5, #6], #polyvar, {"name": "ReScript"}, 42)
    }
    ()
  })
})

describe("Console.assert5", () => {
  test("Console.assert5", () => {
    module Test = {
      let value = 42
      Console.assert5(false, "Hello", "World", "JS", '!', '!')
      Console.assert5(value == 42, [1, 2], (3, 4), [#5, #6], #polyvar, {"name": "ReScript"})
    }
    ()
  })
})

describe("Console.assert4", () => {
  test("Console.assert4", () => {
    module Test = {
      let value = 42
      Console.assert4(false, "Hello", "World", "ReScript", "!!!")
      Console.assert4(value == 42, [1, 2], (3, 4), [#5, #6], #polyvar)
    }
    ()
  })
})

describe("Console.assert3", () => {
  test("Console.assert3", () => {
    module Test = {
      Console.assert3(false, "Hello", "World", "ReScript")
      Console.assert3(42 == 42, "One", 2, #3)
    }
    ()
  })
})

describe("Console.assert2", () => {
  test("Console.assert2", () => {
    module Test = {
      Console.assert2(false, "Hello", "World")
      Console.assert2(42 == 42, [1, 2, 3], '4')
    }
    ()
  })
})

describe("Console.assert_", () => {
  test("Console.assert_", () => {
    module Test = {
      Console.assert_(false, "Hello World!")
      Console.assert_(42 == 42, "The answer")
    }
    ()
  })
})

describe("Date.UTC.makeWithYMDHMSM", () => {
  test("Date.UTC.makeWithYMDHMSM", () => {
    module Test = {
      Date.UTC.makeWithYMDHMSM(
        ~year=2023,
        ~month=1,
        ~date=20,
        ~hours=16,
        ~minutes=40,
        ~seconds=0,
        ~milliseconds=0,
      )->Console.log
      // 1676911200000

      Date.UTC.makeWithYMDHMSM(
        ~year=2023,
        ~month=1,
        ~date=20,
        ~hours=16,
        ~minutes=40,
        ~seconds=0,
        ~milliseconds=1000,
      )->Console.log
      // 1676911201000

      Date.UTC.makeWithYMDHMSM(
        ~year=2023,
        ~month=1,
        ~date=20,
        ~hours=16,
        ~minutes=40,
        ~seconds=0,
        ~milliseconds=-1,
      )->Console.log
      // 1676911199999
    }
    ()
  })
})

describe("Date.UTC.makeWithYMDHMS", () => {
  test("Date.UTC.makeWithYMDHMS", () => {
    module Test = {
      Date.UTC.makeWithYMDHMS(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=0)
      // 1676911200000

      Date.UTC.makeWithYMDHMS(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=60)
      // 1676911260000

      Date.UTC.makeWithYMDHMS(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=-1)
      // 1676911199000
    }
    ()
  })
})

describe("Date.UTC.makeWithYMDHM", () => {
  test("Date.UTC.makeWithYMDHM", () => {
    module Test = {
      Date.UTC.makeWithYMDHM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40)
      // 1676911200000

      Date.UTC.makeWithYMDHM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=60)
      // 1676912400000

      Date.UTC.makeWithYMDHM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=-1)
      // 1676908740000
    }
    ()
  })
})

describe("Date.UTC.makeWithYMDH", () => {
  test("Date.UTC.makeWithYMDH", () => {
    module Test = {
      Date.UTC.makeWithYMDH(~year=2023, ~month=1, ~date=20, ~hours=16)
      // 1676908800000

      Date.UTC.makeWithYMDH(~year=2023, ~month=1, ~date=20, ~hours=24)
      // 1676937600000

      Date.UTC.makeWithYMDH(~year=2023, ~month=1, ~date=20, ~hours=-1)
      // 1676847600000
    }
    ()
  })
})

describe("Date.UTC.makeWithYMD", () => {
  test("Date.UTC.makeWithYMD", () => {
    module Test = {
      Date.UTC.makeWithYMD(~year=2023, ~month=1, ~date=20)
      // 1676851200000

      Date.UTC.makeWithYMD(~year=2023, ~month=1, ~date=-1)
      // 1675036800000

      Date.UTC.makeWithYMD(~year=2023, ~month=1, ~date=29)
      // 1677628800000
    }
    ()
  })
})

describe("Date.UTC.makeWithYM", () => {
  test("Date.UTC.makeWithYM", () => {
    module Test = {
      Date.UTC.makeWithYM(~year=2023, ~month=0)
      // 1672531200000

      Date.UTC.makeWithYM(~year=2023, ~month=11)
      // 1701388800000

      Date.UTC.makeWithYM(~year=2023, ~month=12)
      // 1704067200000

      Date.UTC.makeWithYM(~year=2023, ~month=-1)
      // 1669852800000
    }
    ()
  })
})

describe("Date.toJSON", () => {
  test("Date.toJSON", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+00:00")->Date.toJSON
      // Some("2023-01-01T00:00:00.000Z")

      Date.fromString("")->Date.toJSON
      // None
    }
    ()
  })
})

describe("Date.toUTCString", () => {
  test("Date.toUTCString", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+00:00")->Date.toUTCString->Console.log
      // Sun, 01 Jan 2023 00:00:00 GMT

      Date.fromString("2023-01-01T00:00:00.00+08:00")->Date.toUTCString->Console.log
      // Sat, 31 Dec 2022 16:00:00 GMT
    }
    ()
  })
})

describe("Date.toISOString", () => {
  test("Date.toISOString", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+00:00")->Date.toISOString->Console.log
      // 2023-01-01T00:00:00.000Z

      Date.fromString("2023-01-01T00:00:00.00+08:00")->Date.toISOString->Console.log
      // 2022-12-31T16:00:00.000Z
    }
    ()
  })
})

describe("Date.toLocaleTimeStringWithLocaleAndOptions", () => {
  test("Date.toLocaleTimeStringWithLocaleAndOptions", () => {
    module Test = {
      Date.make()
      ->Date.toLocaleTimeStringWithLocaleAndOptions("en-US", {timeStyle: #long})
      ->Console.log
      // 3:40:00 PM GMT+1

      Date.make()
      ->Date.toLocaleTimeStringWithLocaleAndOptions("de", {hour: #"2-digit", minute: #"2-digit"})
      ->Console.log
      // 15:40
    }
    ()
  })
})

describe("Date.toLocaleTimeStringWithLocale", () => {
  test("Date.toLocaleTimeStringWithLocale", () => {
    module Test = {
      Date.make()->Date.toLocaleTimeStringWithLocale("en-US")->Console.log
      // 3:40:00 PM
    }
    ()
  })
})

describe("Date.toLocaleTimeString", () => {
  test("Date.toLocaleTimeString", () => {
    module Test = {
      Date.make()->Date.toLocaleTimeString->Console.log
      // 3:40:00 PM
    }
    ()
  })
})

describe("Date.toLocaleStringWithLocaleAndOptions", () => {
  test("Date.toLocaleStringWithLocaleAndOptions", () => {
    module Test = {
      Date.make()
      ->Date.toLocaleStringWithLocaleAndOptions("en", {dateStyle: #short, timeStyle: #short})
      ->Console.log
      // 2/19/23, 3:40 PM

      Date.make()
      ->Date.toLocaleStringWithLocaleAndOptions(
        "en",
        {
          era: #long,
          year: #numeric,
          month: #"2-digit",
          day: #"2-digit",
          hour: #numeric,
          timeZoneName: #short,
        },
      )
      ->Console.log
      // 02/19/2023 Anno Domini, 3 PM GMT+1
    }
    ()
  })
})

describe("Date.toLocaleStringWithLocale", () => {
  test("Date.toLocaleStringWithLocale", () => {
    module Test = {
      Date.make()->Date.toLocaleStringWithLocale("en-US")->Console.log
      // 2/19/2023, 3:40:00 PM
    }
    ()
  })
})

describe("Date.toLocaleString", () => {
  test("Date.toLocaleString", () => {
    module Test = {
      Date.make()->Date.toLocaleString->Console.log
      // 2/19/2023, 3:40:00 PM
    }
    ()
  })
})

describe("Date.toLocaleDateStringWithLocaleAndOptions", () => {
  test("Date.toLocaleDateStringWithLocaleAndOptions", () => {
    module Test = {
      Date.make()
      ->Date.toLocaleDateStringWithLocaleAndOptions("en-US", {dateStyle: #long})
      ->Console.log
      // February 19, 2023

      Date.make()
      ->Date.toLocaleDateStringWithLocaleAndOptions("de", {hour: #"2-digit", minute: #"2-digit"})
      ->Console.log
      // 19.2.2023, 15:40

      Date.make()->Date.toLocaleDateStringWithLocaleAndOptions("de", {year: #numeric})->Console.log
      // 2023
    }
    ()
  })
})

describe("Date.toLocaleDateStringWithLocale", () => {
  test("Date.toLocaleDateStringWithLocale", () => {
    module Test = {
      Date.make()->Date.toLocaleDateStringWithLocale("en-US")->Console.log
      // 2/19/2023
    }
    ()
  })
})

describe("Date.toLocaleDateString", () => {
  test("Date.toLocaleDateString", () => {
    module Test = {
      Date.make()->Date.toLocaleDateString->Console.log
      // 2/19/2023
    }
    ()
  })
})

describe("Date.toTimeString", () => {
  test("Date.toTimeString", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.toTimeString->Console.log
      // 00:00:00 GMT+0100 (Central European Standard Time)

      Date.fromString("2023-01-01T00:00:00.00+08:00")->Date.toTimeString->Console.log
      // 17:00:00 GMT+0100 (Central European Standard Time)
    }
    ()
  })
})

describe("Date.toString", () => {
  test("Date.toString", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.toString->Console.log
      // Sun Jan 01 2023 00:00:00 GMT+0100 (Central European Standard Time)

      Date.fromString("2023-06-01T00:00:00.00+01:00")->Date.toString->Console.log
      // Thu Jun 01 2023 01:00:00 GMT+0200 (Central European Summer Time)
    }
    ()
  })
})

describe("Date.toDateString", () => {
  test("Date.toDateString", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.toDateString->Console.log
      // Sun Jan 01 2023

      Date.fromString("2023-01-01T00:00:00.00+08:00")->Date.toDateString->Console.log
      // Sat Dec 31 2022
    }
    ()
  })
})

describe("Date.setUTCMilliseconds", () => {
  test("Date.setUTCMilliseconds", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCMilliseconds(0)
    }
    ()
  })
})

describe("Date.setUTCSecondsMs", () => {
  test("Date.setUTCSecondsMs", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCSecondsMs(~seconds=0, ~milliseconds=0)
    }
    ()
  })
})

describe("Date.setUTCSeconds", () => {
  test("Date.setUTCSeconds", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCSeconds(0)
    }
    ()
  })
})

describe("Date.setUTCMinutesSMs", () => {
  test("Date.setUTCMinutesSMs", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCMinutesSMs(
        ~minutes=0,
        ~seconds=0,
        ~milliseconds=0,
      )
    }
    ()
  })
})

describe("Date.setUTCMinutesS", () => {
  test("Date.setUTCMinutesS", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCMinutesS(~minutes=0, ~seconds=0)
    }
    ()
  })
})

describe("Date.setUTCMinutes", () => {
  test("Date.setUTCMinutes", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCMinutes(0)
    }
    ()
  })
})

describe("Date.setUTCHoursMSMs", () => {
  test("Date.setUTCHoursMSMs", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCHoursMSMs(
        ~hours=0,
        ~minutes=0,
        ~seconds=0,
        ~milliseconds=0,
      )
    }
    ()
  })
})

describe("Date.setUTCHoursMS", () => {
  test("Date.setUTCHoursMS", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCHoursMS(
        ~hours=0,
        ~minutes=0,
        ~seconds=0,
      )
    }
    ()
  })
})

describe("Date.setUTCHoursM", () => {
  test("Date.setUTCHoursM", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCHoursM(~hours=0, ~minutes=0)
    }
    ()
  })
})

describe("Date.setUTCHours", () => {
  test("Date.setUTCHours", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCHours(0)
    }
    ()
  })
})

describe("Date.setUTCDate", () => {
  test("Date.setUTCDate", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCDate(1)
    }
    ()
  })
})

describe("Date.setUTCMonth", () => {
  test("Date.setUTCMonth", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCMonth(0)
    }
    ()
  })
})

describe("Date.setUTCFullYearMD", () => {
  test("Date.setUTCFullYearMD", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCFullYearMD(
        ~year=2024,
        ~month=0,
        ~date=1,
      )
    }
    ()
  })
})

describe("Date.setUTCFullYearM", () => {
  test("Date.setUTCFullYearM", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCFullYearM(~year=2024, ~month=0)
    }
    ()
  })
})

describe("Date.setUTCFullYear", () => {
  test("Date.setUTCFullYear", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setUTCFullYear(2024)
    }
    ()
  })
})

describe("Date.getUTCDay", () => {
  test("Date.getUTCDay", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCDay // 6
    }
    ()
  })
})

describe("Date.getUTCMilliseconds", () => {
  test("Date.getUTCMilliseconds", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCMilliseconds // 0
    }
    ()
  })
})

describe("Date.getUTCSeconds", () => {
  test("Date.getUTCSeconds", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCSeconds // 0
    }
    ()
  })
})

describe("Date.getUTCMinutes", () => {
  test("Date.getUTCMinutes", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCMinutes // 0
    }
    ()
  })
})

describe("Date.getUTCHours", () => {
  test("Date.getUTCHours", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCHours // 23
    }
    ()
  })
})

describe("Date.getUTCDate", () => {
  test("Date.getUTCDate", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCDate // 31
    }
    ()
  })
})

describe("Date.getUTCMonth", () => {
  test("Date.getUTCMonth", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCMonth // 11
    }
    ()
  })
})

describe("Date.getUTCFullYear", () => {
  test("Date.getUTCFullYear", () => {
    module Test = {
      Date.fromString("2023-01-01T00:00:00.00+01:00")->Date.getUTCFullYear // 2022
    }
    ()
  })
})

describe("Date.setMilliseconds", () => {
  test("Date.setMilliseconds", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setMilliseconds(0)
    }
    ()
  })
})

describe("Date.setSecondsMs", () => {
  test("Date.setSecondsMs", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setSecondsMs(~seconds=0, ~milliseconds=0)
    }
    ()
  })
})

describe("Date.setSeconds", () => {
  test("Date.setSeconds", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setSeconds(0)
    }
    ()
  })
})

describe("Date.setMinutesSMs", () => {
  test("Date.setMinutesSMs", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setMinutesSMs(
        ~minutes=0,
        ~seconds=0,
        ~milliseconds=0,
      )
    }
    ()
  })
})

describe("Date.setMinutesS", () => {
  test("Date.setMinutesS", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setMinutesS(~minutes=0, ~seconds=0)
    }
    ()
  })
})

describe("Date.setMinutes", () => {
  test("Date.setMinutes", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setMinutes(0)
    }
    ()
  })
})

describe("Date.setHoursMSMs", () => {
  test("Date.setHoursMSMs", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setHoursMSMs(
        ~hours=0,
        ~minutes=0,
        ~seconds=0,
        ~milliseconds=0,
      )
    }
    ()
  })
})

describe("Date.setHoursMS", () => {
  test("Date.setHoursMS", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setHoursMS(~hours=0, ~minutes=0, ~seconds=0)
    }
    ()
  })
})

describe("Date.setHoursM", () => {
  test("Date.setHoursM", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setHoursM(~hours=0, ~minutes=0)
    }
    ()
  })
})

describe("Date.setHours", () => {
  test("Date.setHours", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setHours(0)
    }
    ()
  })
})

describe("Date.setDate", () => {
  test("Date.setDate", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setDate(1)
    }
    ()
  })
})

describe("Date.setMonth", () => {
  test("Date.setMonth", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setMonth(0)
    }
    ()
  })
})

describe("Date.setFullYearMD", () => {
  test("Date.setFullYearMD", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setFullYearMD(~year=2024, ~month=0, ~date=1)
    }
    ()
  })
})

describe("Date.setFullYearM", () => {
  test("Date.setFullYearM", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setFullYearM(~year=2024, ~month=0)
    }
    ()
  })
})

describe("Date.setFullYear", () => {
  test("Date.setFullYear", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.setFullYear(2024)
    }
    ()
  })
})

describe("Date.getDay", () => {
  test("Date.getDay", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.getDay
      // 1
    }
    ()
  })
})

describe("Date.getMilliseconds", () => {
  test("Date.getMilliseconds", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.getMilliseconds
      // 0
    }
    ()
  })
})

describe("Date.getSeconds", () => {
  test("Date.getSeconds", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.getSeconds
      // 0
    }
    ()
  })
})

describe("Date.getMinutes", () => {
  test("Date.getMinutes", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.getMinutes
      // 40
    }
    ()
  })
})

describe("Date.getHours", () => {
  test("Date.getHours", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.getHours
      // 16
    }
    ()
  })
})

describe("Date.getDate", () => {
  test("Date.getDate", () => {
    module Test = {
      Date.fromString("2023-02-20T16:40:00.00")->Date.getDate
      // 20
    }
    ()
  })
})

describe("Date.getMonth", () => {
  test("Date.getMonth", () => {
    module Test = {
      Date.fromString("2023-01-01")->Date.getMonth
      // 0
    }
    ()
  })
})

describe("Date.getFullYear", () => {
  test("Date.getFullYear", () => {
    module Test = {
      Date.fromString("2023-02-20")->Date.getFullYear
      // 2023
    }
    ()
  })
})

describe("Date.getTimezoneOffset", () => {
  test("Date.getTimezoneOffset", () => {
    module Test = {
      Date.fromString("2023-01-01")->Date.getTimezoneOffset
      // -60 with local time zone = Europe/Berlin

      Date.fromString("2023-06-01")->Date.getTimezoneOffset
      // -120 with local time zone = Europe/Berlin
    }
    ()
  })
})

describe("Date.getTime", () => {
  test("Date.getTime", () => {
    module Test = {
      Date.fromString("2023-02-20")->Date.getTime
      // 1676851200000
    }
    ()
  })
})

describe("Date.makeWithYMDHMSM", () => {
  test("Date.makeWithYMDHMSM", () => {
    module Test = {
      Date.makeWithYMDHMSM(
        ~year=2023,
        ~month=1,
        ~date=20,
        ~hours=16,
        ~minutes=40,
        ~seconds=0,
        ~milliseconds=0,
      )
      // 2023-02-20T16:40:00.000Z

      Date.makeWithYMDHMSM(
        ~year=2023,
        ~month=1,
        ~date=20,
        ~hours=16,
        ~minutes=40,
        ~seconds=0,
        ~milliseconds=1000,
      )
      // 2023-02-20T16:40:01.000Z

      Date.makeWithYMDHMSM(
        ~year=2023,
        ~month=1,
        ~date=20,
        ~hours=16,
        ~minutes=40,
        ~seconds=0,
        ~milliseconds=-1,
      )
      // 2023-02-20T16:39:59.999Z

      // Note: The output depends on your local time zone.
      // In nodejs you can change it by using the TZ env (`export TZ='Europe/London' && node index.bs.js`)
    }
    ()
  })
})

describe("Date.makeWithYMDHMS", () => {
  test("Date.makeWithYMDHMS", () => {
    module Test = {
      Date.makeWithYMDHMS(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=0)
      // 2023-02-20T16:40:00.000Z

      Date.makeWithYMDHMS(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=60)
      // 2023-02-20T16:41:00.000Z

      Date.makeWithYMDHMS(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40, ~seconds=-1)
      // 2023-02-20T16:39:59.000Z

      // Note: The output depends on your local time zone.
      // In nodejs you can change it by using the TZ env (`export TZ='Europe/London' && node index.bs.js`)
    }
    ()
  })
})

describe("Date.makeWithYMDHM", () => {
  test("Date.makeWithYMDHM", () => {
    module Test = {
      Date.makeWithYMDHM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=40)
      // 2023-02-20T16:40:00.000Z

      Date.makeWithYMDHM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=60)
      // 2023-02-20T17:00:00.000Z

      Date.makeWithYMDHM(~year=2023, ~month=1, ~date=20, ~hours=16, ~minutes=-1)
      // 2023-02-20T15:59:00.000Z

      // Note: The output depends on your local time zone.
      // In nodejs you can change it by using the TZ env (`export TZ='Europe/London' && node index.bs.js`)
    }
    ()
  })
})

describe("Date.makeWithYMDH", () => {
  test("Date.makeWithYMDH", () => {
    module Test = {
      Date.makeWithYMDH(~year=2023, ~month=1, ~date=20, ~hours=16)
      // 2023-02-20T16:00:00.000Z

      Date.makeWithYMDH(~year=2023, ~month=1, ~date=20, ~hours=24)
      // 2023-02-21T00:00:00.000Z

      Date.makeWithYMDH(~year=2023, ~month=1, ~date=20, ~hours=-1)
      // 2023-02-19T23:00:00.000Z

      // Note: The output depends on your local time zone.
      // In nodejs you can change it by using the TZ env (`export TZ='Europe/London' && node index.bs.js`)
    }
    ()
  })
})

describe("Date.makeWithYMD", () => {
  test("Date.makeWithYMD", () => {
    module Test = {
      Date.makeWithYMD(~year=2023, ~month=1, ~date=20)
      // 2023-02-20T00:00:00.000Z

      Date.makeWithYMD(~year=2023, ~month=1, ~date=-1)
      // 2022-11-29T00:00:00.000Z

      Date.makeWithYMD(~year=2023, ~month=1, ~date=29)
      // 2023-03-01T00:00:00.000Z
    }
    ()
  })
})

describe("Date.makeWithYM", () => {
  test("Date.makeWithYM", () => {
    module Test = {
      Date.makeWithYM(~year=2023, ~month=0)
      // 2023-01-01T00:00:00.000Z

      Date.makeWithYM(~year=2023, ~month=11)
      // 2023-12-01T00:00:00.000Z

      Date.makeWithYM(~year=2023, ~month=12)
      // 2024-01-01T00:00:00.000Z

      Date.makeWithYM(~year=2023, ~month=-1)
      // 2022-12-01T00:00:00.000Z

      // Note: The output depends on your local time zone.
      // In nodejs you can change it by using the TZ env (`export TZ='Europe/London' && node index.bs.js`)
    }
    ()
  })
})

describe("Date.fromTime", () => {
  test("Date.fromTime", () => {
    module Test = {
      Date.fromTime(0.0)
      // 1970-01-01T00:00:00.000Z

      Date.fromTime(-86_400_000.0)
      // 1969-12-31T00:00:00.000Z

      Date.fromTime(86_400_000.0)
      // 1970-01-02T00:00:00.000Z
    }
    ()
  })
})

describe("Date.fromString", () => {
  test("Date.fromString", () => {
    module Test = {
      Date.fromString("2023") // 2023-01-01T00:00:00.000Z

      Date.fromString("2023-02-20") // 2023-02-20T00:00:00.000Z

      Date.fromString("2023-02-20T16:40:00.00Z") // 2023-02-20T16:40:00.000Z

      Date.fromString("") // Invalid Date

      Date.fromString("")->Date.getTime // NaN
    }
    ()
  })
})

describe("Date.make", () => {
  test("Date.make", () => {
    module Test = {
      Date.make()
    }
    ()
  })
})

describe("Dict.mapValues", () => {
  test("Dict.mapValues", () => {
    module Test = {
      let dict = Dict.fromArray([("key1", 1), ("key2", 2)])

      dict->Dict.mapValues(v => v + 10)->Dict.toArray // [("key1", 11), ("key2", 12)]
      dict->Dict.mapValues(v => Int.toString(v))->Dict.toArray // [("key1", "1"), ("key2", "2")]
    }
    ()
  })
})

describe("Dict.forEachWithKey", () => {
  test("Dict.forEachWithKey", () => {
    module Test = {
      let dict = Dict.fromArray([("key1", "value1"), ("key2", "value2")])

      dict->Dict.forEachWithKey(
        (value, key) => {
          Console.log2(value, key)
        },
      )
    }
    ()
  })
})

describe("Dict.forEach", () => {
  test("Dict.forEach", () => {
    module Test = {
      let dict = Dict.fromArray([("key1", "value1"), ("key2", "value2")])

      dict->Dict.forEach(
        value => {
          Console.log(value)
        },
      )
    }
    ()
  })
})

describe("Dict.copy", () => {
  test("Dict.copy", () => {
    module Test = {
      let dict = Dict.fromArray([("key1", "value1"), ("key2", "value2")])
      let dict2 = dict->Dict.copy

      // Both log `["key1", "key2"]` here.
      Console.log2(dict->Dict.keysToArray, dict2->Dict.keysToArray)
    }
    ()
  })
})

describe("Dict.assign", () => {
  test("Dict.assign", () => {
    module Test = {
      let dict1 = Dict.make()
      dict1->Dict.set("firstKey", 1)
      Console.log(dict1->Dict.keysToArray) // Logs `["firstKey"]`

      let dict2 = Dict.make()
      dict2->Dict.set("someKey", 2)
      dict2->Dict.set("someKey2", 3)

      let dict1 = dict1->Dict.assign(dict2)

      Console.log(dict1->Dict.keysToArray) // Logs `["firstKey", "someKey", "someKey2"]`
    }
    ()
  })
})

describe("Dict.valuesToArray", () => {
  test("Dict.valuesToArray", () => {
    module Test = {
      let dict = Dict.make()
      dict->Dict.set("someKey", 1)
      dict->Dict.set("someKey2", 2)
      let values = dict->Dict.valuesToArray
      Console.log(values) // Logs `[1, 2]` to the console
    }
    ()
  })
})

describe("Dict.keysToArray", () => {
  test("Dict.keysToArray", () => {
    module Test = {
      let dict = Dict.make()
      dict->Dict.set("someKey", 1)
      dict->Dict.set("someKey2", 2)
      let keys = dict->Dict.keysToArray
      Console.log(keys) // Logs `["someKey", "someKey2"]` to the console
    }
    ()
  })
})

describe("Dict.toArray", () => {
  test("Dict.toArray", () => {
    module Test = {
      let dict = Dict.make()
      dict->Dict.set("someKey", 1)
      dict->Dict.set("someKey2", 2)
      let asArray = dict->Dict.toArray
      Console.log(asArray) // Logs `[["someKey", 1], ["someKey2", 2]]` to the console
    }
    ()
  })
})

describe("Dict.fromIterator", () => {
  test("Dict.fromIterator", () => {
    module Test = {
      let iterator: Iterator.t<(string, int)> = %raw(`
  (() => {
    var map1 = new Map();
    map1.set('first', 1);
    map1.set('second', 2);
    var iterator1 = map1[Symbol.iterator]();
    return iterator1;
  })()
`)
      iterator
      ->Dict.fromIterator
      ->Dict.valuesToArray
      ->assertEqual([1, 2])
    }
    ()
  })
})

describe("Dict.fromArray", () => {
  test("Dict.fromArray", () => {
    module Test = {
      let dict = Dict.fromArray([("key1", "value1"), ("key2", "value2")])
    }
    ()
  })
})

describe("Dict.make", () => {
  test("Dict.make", () => {
    module Test = {
      let dict1: dict<int> = Dict.make() // You can annotate the type of the values of your dict yourself if you want

      let dict2 = Dict.make() // Or you can let ReScript infer it via usage.
      dict2->Dict.set("someKey", 12)
    }
    ()
  })
})

describe("Dict.delete", () => {
  test("Dict.delete", () => {
    module Test = {
      let dict = Dict.fromArray([("someKey", "someValue")])

      dict->Dict.delete("someKey")
    }
    ()
  })
})

describe("Dict.set", () => {
  test("Dict.set", () => {
    module Test = {
      let dict = Dict.make()

      dict->Dict.set("someKey", "someValue")
    }
    ()
  })
})

describe("Dict.get", () => {
  test("Dict.get", () => {
    module Test = {
      let dict = Dict.fromArray([("someKey", "someValue")])

      switch dict->Dict.get("someKey") {
      | None => Console.log("Nope, didn't have the key.")
      | Some(value) => Console.log(value)
      }
    }
    ()
  })
})

describe("Dict.getUnsafe", () => {
  test("Dict.getUnsafe", () => {
    module Test = {
      let dict = Dict.fromArray([("key1", "value1"), ("key2", "value2")])
      let value = dict->Dict.getUnsafe("key1")
      Console.log(value) // value1
    }
    ()
  })
})

describe("Error.panic", () => {
  test("Error.panic", () => {
    module Test = {
      try {
        Error.panic("Uh oh. This was unexpected!")
      } catch {
      | Exn.Error(obj) =>
        switch Exn.message(obj) {
        | Some(m) => assert(m == "Panic! Uh oh. This was unexpected!")
        | None => assert(false)
        }
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Error.raise", () => {
  test("Error.raise", () => {
    module Test = {
      let error = Error.make("Everything is upside down.")

      if 5 > 10 {
        error->Error.raise
      } else {
        Console.log("Phew, sanity still rules.")
      }
    }
    ()
  })
})

describe("Error.make", () => {
  test("Error.make", () => {
    module Test = {
      let error = Error.make("Some message here")
      Console.log(error->Error.message) // Logs "Some message here" to the console
      Console.log(error->Error.name) // Logs "Error" to the console, because this is a regular error
    }
    ()
  })
})

describe("Error.name", () => {
  test("Error.name", () => {
    module Test = {
      let error = Error.SyntaxError.make("Some message here")
      Console.log(error->Error.name) // Logs "SyntaxError" to the console
    }
    ()
  })
})

describe("Error.message", () => {
  test("Error.message", () => {
    module Test = {
      let error = Error.SyntaxError.make("Some message here")
      Console.log(error->Error.message) // Logs "Some message here" to the console
    }
    ()
  })
})

describe("Error.stack", () => {
  test("Error.stack", () => {
    module Test = {
      let error = Error.make("error")
      Console.log(error->Error.stack) // Logs `stack` if it exists on `someError`
    }
    ()
  })
})

describe("Error.toException", () => {
  test("Error.toException", () => {
    module Test = {
      let error = Error.make("Something went wrong.")

      let asExn = error->Error.toException // `asExn` is now type `exn`
    }
    ()
  })
})

describe("Float.Constants.maxValue", () => {
  test("Float.Constants.maxValue", () => {
    module Test = {
      Float.Constants.minValue
    }
    ()
  })
})

describe("Float.Constants.minValue", () => {
  test("Float.Constants.minValue", () => {
    module Test = {
      Float.Constants.minValue
    }
    ()
  })
})

describe("Float.Constants.negativeInfinity", () => {
  test("Float.Constants.negativeInfinity", () => {
    module Test = {
      Float.Constants.negativeInfinity
    }
    ()
  })
})

describe("Float.Constants.positiveInfinity", () => {
  test("Float.Constants.positiveInfinity", () => {
    module Test = {
      Float.Constants.positiveInfinity
    }
    ()
  })
})

describe("Float.Constants.epsilon", () => {
  test("Float.Constants.epsilon", () => {
    module Test = {
      Float.Constants.epsilon
    }
    ()
  })
})

describe("Float.Constants.nan", () => {
  test("Float.Constants.nan", () => {
    module Test = {
      Float.Constants.nan
    }
    ()
  })
})

describe("Float.clamp", () => {
  test("Float.clamp", () => {
    module Test = {
      Float.clamp(4.2) == 4.2
      Float.clamp(4.2, ~min=4.3) == 4.3
      Float.clamp(4.2, ~max=4.1) == 4.1
      Float.clamp(4.2, ~min=4.3, ~max=4.1) == 4.3
    }
    ()
  })
})

describe("Float.mod", () => {
  test("Float.mod", () => {
    module Test = {
      Float.mod(7.0, 4.0) == 3.0
    }
    ()
  })
})

describe("Float.fromInt", () => {
  test("Float.fromInt", () => {
    module Test = {
      Float.fromInt(2) == 2.0
      Float.fromInt(1) == 1.0
    }
    ()
  })
})

describe("Float.toInt", () => {
  test("Float.toInt", () => {
    module Test = {
      Float.toInt(2.0) == 2
      Float.toInt(1.0) == 1
      Float.toInt(1.1) == 1
      Float.toInt(1.6) == 1
    }
    ()
  })
})

describe("Float.fromString", () => {
  test("Float.fromString", () => {
    module Test = {
      Float.fromString("0") == Some(0.0)
      Float.fromString("NaN") == None
      Float.fromString("6") == Some(6.0)
    }
    ()
  })
})

describe("Float.toLocaleString", () => {
  test("Float.toLocaleString", () => {
    module Test = {
      // If the application uses English as the default language
      Float.toLocaleString(1000.0) // "1,000"

      // If the application uses Portuguese Brazil as the default language
      Float.toLocaleString(1000.0) // "1.000"
    }
    ()
  })
})

describe("Float.toStringWithRadix", () => {
  test("Float.toStringWithRadix", () => {
    module Test = {
      Float.toStringWithRadix(6.0, ~radix=2) // "110"
      Float.toStringWithRadix(3735928559.0, ~radix=16) // "deadbeef"
      Float.toStringWithRadix(123456.0, ~radix=36) // "2n9c"
    }
    ()
  })
})

describe("Float.toString", () => {
  test("Float.toString", () => {
    module Test = {
      Float.toString(1000.0) // "1000"
      Float.toString(-1000.0) // "-1000"
    }
    ()
  })
})

describe("Float.toPrecisionWithPrecision", () => {
  test("Float.toPrecisionWithPrecision", () => {
    module Test = {
      Float.toPrecisionWithPrecision(100.0, ~digits=2) // "1.0e+2"
      Float.toPrecisionWithPrecision(1.0, ~digits=1) // "1"
    }
    ()
  })
})

describe("Float.toPrecision", () => {
  test("Float.toPrecision", () => {
    module Test = {
      Float.toPrecision(100.0) // "100"
      Float.toPrecision(1.0) // "1"
      Float.toPrecision(100.0, ~digits=2) // "1.0e+2"
      Float.toPrecision(1.0, ~digits=1) // "1"
    }
    ()
  })
})

describe("Float.toFixedWithPrecision", () => {
  test("Float.toFixedWithPrecision", () => {
    module Test = {
      Float.toFixedWithPrecision(300.0, ~digits=4) // "300.0000"
      Float.toFixedWithPrecision(300.0, ~digits=1) // "300.0"
    }
    ()
  })
})

describe("Float.toFixed", () => {
  test("Float.toFixed", () => {
    module Test = {
      Float.toFixed(123456.0) // "123456.00"
      Float.toFixed(10.0) // "10.00"
      Float.toFixed(300.0, ~digits=4) // "300.0000"
      Float.toFixed(300.0, ~digits=1) // "300.0"
    }
    ()
  })
})

describe("Float.toExponentialWithPrecision", () => {
  test("Float.toExponentialWithPrecision", () => {
    module Test = {
      Float.toExponentialWithPrecision(77.0, ~digits=2) // "7.70e+1"
      Float.toExponentialWithPrecision(5678.0, ~digits=2) // "5.68e+3"
    }
    ()
  })
})

describe("Float.toExponential", () => {
  test("Float.toExponential", () => {
    module Test = {
      Float.toExponential(1000.0) // "1e+3"
      Float.toExponential(-1000.0) // "-1e+3"
      Float.toExponential(77.0, ~digits=2) // "7.70e+1"
      Float.toExponential(5678.0, ~digits=2) // "5.68e+3"
    }
    ()
  })
})

describe("Float.parseIntWithRadix", () => {
  test("Float.parseIntWithRadix", () => {
    module Test = {
      Float.parseIntWithRadix("10.0", ~radix=2) // 2.0
      Float.parseIntWithRadix("15 * 3", ~radix=10) // 15.0
      Float.parseIntWithRadix("12", ~radix=13) // 15.0
      Float.parseIntWithRadix("17", ~radix=40)->Float.isNaN // true
    }
    ()
  })
})

describe("Float.parseInt", () => {
  test("Float.parseInt", () => {
    module Test = {
      Float.parseInt("1.0") // 1.0
      Float.parseInt("  3.14   ") // 3.0
      Float.parseInt(3) // 3.0
      Float.parseInt("3.14some non-digit characters") // 3.0
      Float.parseInt("error")->Float.isNaN // true
      Float.parseInt("10.0", ~radix=2) // 2.0
      Float.parseInt("15 * 3", ~radix=10) // 15.0
      Float.parseInt("12", ~radix=13) // 15.0
      Float.parseInt("17", ~radix=40)->Float.isNaN // true
    }
    ()
  })
})

describe("Float.parseFloat", () => {
  test("Float.parseFloat", () => {
    module Test = {
      Float.parseFloat("1.0") // 1.0
      Float.parseFloat("  3.14   ") // 3.14
      Float.parseFloat("3.0") // 3.0
      Float.parseFloat("3.14some non-digit characters") // 3.14
      Float.parseFloat("error")->Float.isNaN // true
    }
    ()
  })
})

describe("Float.isFinite", () => {
  test("Float.isFinite", () => {
    module Test = {
      Float.isFinite(1.0) // true
      Float.isFinite(Float.Constants.nan) // false
      Float.isFinite(Float.Constants.positiveInfinity) // false
    }
    ()
  })
})

describe("Float.isNaN", () => {
  test("Float.isNaN", () => {
    module Test = {
      Float.isNaN(3.0) // false
      Float.isNaN(Float.Constants.nan) // true
    }
    ()
  })
})

describe("Int.Bitwise.asr", () => {
  test("Int.Bitwise.asr", () => {
    module Test = {
      Int.Bitwise.asr(4, 1) == 2
    }
    ()
  })
})

describe("Int.Bitwise.lsr", () => {
  test("Int.Bitwise.lsr", () => {
    module Test = {
      Int.Bitwise.lsr(8, 1) == 4
    }
    ()
  })
})

describe("Int.Bitwise.lsl", () => {
  test("Int.Bitwise.lsl", () => {
    module Test = {
      Int.Bitwise.lsl(4, 1) == 8
    }
    ()
  })
})

describe("Int.Bitwise.lnot", () => {
  test("Int.Bitwise.lnot", () => {
    module Test = {
      Int.Bitwise.lnot(2) == -3
    }
    ()
  })
})

describe("Int.Bitwise.lxor", () => {
  test("Int.Bitwise.lxor", () => {
    module Test = {
      Int.Bitwise.lxor(7, 4) == 3
    }
    ()
  })
})

describe("Int.Bitwise.lor", () => {
  test("Int.Bitwise.lor", () => {
    module Test = {
      Int.Bitwise.lor(7, 4) == 7
    }
    ()
  })
})

describe("Int.Bitwise.land", () => {
  test("Int.Bitwise.land", () => {
    module Test = {
      Int.Bitwise.land(7, 4) == 4
    }
    ()
  })
})

describe("Int.Constants.maxValue", () => {
  test("Int.Constants.maxValue", () => {
    module Test = {
      Console.log(Int.Constants.maxValue)
    }
    ()
  })
})

describe("Int.Constants.minValue", () => {
  test("Int.Constants.minValue", () => {
    module Test = {
      Console.log(Int.Constants.minValue)
    }
    ()
  })
})

describe("Int.clamp", () => {
  test("Int.clamp", () => {
    module Test = {
      Int.clamp(42) == 42
      Int.clamp(42, ~min=50) == 50
      Int.clamp(42, ~max=40) == 40
      Int.clamp(42, ~min=50, ~max=40) == 50
    }
    ()
  })
})

describe("Int.rangeWithOptions", () => {
  test("Int.rangeWithOptions", () => {
    module Test = {
      Int.rangeWithOptions(3, 7, {step: 2}) == [3, 5]
      Int.rangeWithOptions(3, 7, {step: 2, inclusive: true}) == [3, 5, 7]
      Int.rangeWithOptions(3, 6, {step: -2}) // RangeError
    }
    ()
  })
})

describe("Int.range", () => {
  test("Int.range", () => {
    module Test = {
      Int.range(3, 6) == [3, 4, 5]
      Int.range(-3, -1) == [-3, -2]
      Int.range(3, 1) == [3, 2]
      Int.range(3, 7, ~options={step: 2}) == [3, 5]
      Int.range(3, 7, ~options={step: 2, inclusive: true}) == [3, 5, 7]
      Int.range(3, 6, ~options={step: -2}) // RangeError
    }
    ()
  })
})

describe("Int.mod", () => {
  test("Int.mod", () => {
    module Test = {
      Int.mod(7, 4) == 3
    }
    ()
  })
})

describe("Int.fromString", () => {
  test("Int.fromString", () => {
    module Test = {
      Int.fromString("0") == Some(0)
      Int.fromString("NaN") == None
      Int.fromString("6", ~radix=2) == None
    }
    ()
  })
})

describe("Int.fromFloat", () => {
  test("Int.fromFloat", () => {
    module Test = {
      Int.fromFloat(2.0) == 2
      Int.fromFloat(1.999) == 1
      Int.fromFloat(1.5) == 1
      Int.fromFloat(0.9999) == 0
    }
    ()
  })
})

describe("Int.toFloat", () => {
  test("Int.toFloat", () => {
    module Test = {
      Int.toFloat(100) == 100.0
      Int.toFloat(2) == 2.0
    }
    ()
  })
})

describe("Int.toLocaleString", () => {
  test("Int.toLocaleString", () => {
    module Test = {
      // If the application uses English as the default language
      Int.toLocaleString(1000) // "1,000"

      // If the application uses Portuguese Brazil as the default language
      Int.toLocaleString(1000) // "1.000"
    }
    ()
  })
})

describe("Int.toStringWithRadix", () => {
  test("Int.toStringWithRadix", () => {
    module Test = {
      Int.toStringWithRadix(6, ~radix=2) // "110"
      Int.toStringWithRadix(373592855, ~radix=16) // "16449317"
      Int.toStringWithRadix(123456, ~radix=36) // "2n9c"
    }
    ()
  })
})

describe("Int.toString", () => {
  test("Int.toString", () => {
    module Test = {
      Int.toString(1000) // "1000"
      Int.toString(-1000) // "-1000"
      Int.toString(6, ~radix=2) // "110"
      Int.toString(373592855, ~radix=16) // "16449317"
      Int.toString(123456, ~radix=36) // "2n9c"
    }
    ()
  })
})

describe("Int.toPrecisionWithPrecision", () => {
  test("Int.toPrecisionWithPrecision", () => {
    module Test = {
      Int.toPrecisionWithPrecision(100, ~digits=2) // "1.0e+2"
      Int.toPrecisionWithPrecision(1, ~digits=2) // "1.0"
    }
    ()
  })
})

describe("Int.toPrecision", () => {
  test("Int.toPrecision", () => {
    module Test = {
      Int.toPrecision(100) // "100"
      Int.toPrecision(1) // "1"
      Int.toPrecision(100, ~digits=2) // "1.0e+2"
      Int.toPrecision(1, ~digits=2) // "1.0"
    }
    ()
  })
})

describe("Int.toFixedWithPrecision", () => {
  test("Int.toFixedWithPrecision", () => {
    module Test = {
      Int.toFixedWithPrecision(300, ~digits=4) // "300.0000"
      Int.toFixedWithPrecision(300, ~digits=1) // "300.0"
    }
    ()
  })
})

describe("Int.toFixed", () => {
  test("Int.toFixed", () => {
    module Test = {
      Int.toFixed(123456) // "123456.00"
      Int.toFixed(10) // "10.00"
      Int.toFixed(300, ~digits=4) // "300.0000"
      Int.toFixed(300, ~digits=1) // "300.0"
    }
    ()
  })
})

describe("Int.toExponentialWithPrecision", () => {
  test("Int.toExponentialWithPrecision", () => {
    module Test = {
      Int.toExponentialWithPrecision(77, ~digits=2) // "7.70e+1"
      Int.toExponentialWithPrecision(5678, ~digits=2) // "5.68e+3"
    }
    ()
  })
})

describe("Int.toExponential", () => {
  test("Int.toExponential", () => {
    module Test = {
      Int.toExponential(1000) // "1e+3"
      Int.toExponential(-1000) // "-1e+3"
      Int.toExponential(77, ~digits=2) // "7.70e+1"
      Int.toExponential(5678, ~digits=2) // "5.68e+3"
    }
    ()
  })
})

describe("Iterator.forEach", () => {
  test("Iterator.forEach", () => {
    module Test = {
      let iterator: Iterator.t<string> = %raw(`
  (() => {
    var array1 = ['a', 'b', 'c'];
    var iterator1 = array1[Symbol.iterator]();
    return iterator1
  })()
`)
      iterator->Iterator.forEach(
        v => {
          switch v {
          | Some("a" | "b" | "c") => assert(true)
          | other =>
            other
            ->Option.isNone
            ->assertEqual(true)
          }
        },
      )
    }
    ()
  })
})

describe("Iterator.toArrayWithMapper", () => {
  test("Iterator.toArrayWithMapper", () => {
    module Test = {
      let map = Map.make()
      map->Map.set("someKey", "someValue")
      map->Map.set("someKey2", "someValue2")

      // `Map.keys` returns all keys of the map as an iterator.
      let mapKeysAsArray =
        map
        ->Map.keys
        ->Iterator.toArrayWithMapper(key => key->String.length)

      Console.log(mapKeysAsArray) // Logs [7, 8] to the console.
    }
    ()
  })
})

describe("Iterator.toArray", () => {
  test("Iterator.toArray", () => {
    module Test = {
      let map = Map.make()
      map->Map.set("someKey", "someValue")
      map->Map.set("someKey2", "someValue2")

      // `Map.keys` returns all keys of the map as an iterator.
      let mapKeysAsArray = map->Map.keys->Iterator.toArray

      Console.log(mapKeysAsArray) // Logs ["someKey", "someKey2"] to the console.
    }
    ()
  })
})

describe("Iterator.next", () => {
  test("Iterator.next", () => {
    module Test = {
      let iterator: Iterator.t<string> = %raw(`
  (() => {
    var array1 = ['a'];
    var iterator1 = array1[Symbol.iterator]();
    return iterator1
  })()
`)
      (iterator->Iterator.next).done->assertEqual(false)
      (iterator->Iterator.next).done->assertEqual(true)
    }
    ()
  })
})

describe("JSON.Decode.array", () => {
  test("JSON.Decode.array", () => {
    module Test = {
      JSON.parseExn(`["foo", "bar"]`)->JSON.Decode.array
      // Some([ 'foo', 'bar' ])

      JSON.parseExn(`"hello world"`)->JSON.Decode.array
      // None
    }
    ()
  })
})

describe("JSON.Decode.object", () => {
  test("JSON.Decode.object", () => {
    module Test = {
      JSON.parseExn(`{"foo":"bar"}`)->JSON.Decode.object
      // Some({ foo: 'bar' })

      JSON.parseExn(`"hello world"`)->JSON.Decode.object
      // None
    }
    ()
  })
})

describe("JSON.Decode.float", () => {
  test("JSON.Decode.float", () => {
    module Test = {
      JSON.parseExn(`42.0`)->JSON.Decode.float
      // Some(42.0)

      JSON.parseExn(`"hello world"`)->JSON.Decode.float
      // None
    }
    ()
  })
})

describe("JSON.Decode.string", () => {
  test("JSON.Decode.string", () => {
    module Test = {
      JSON.parseExn(`"hello world"`)->JSON.Decode.string
      // Some("hello world")

      JSON.parseExn(`42`)->JSON.Decode.string
      // None
    }
    ()
  })
})

describe("JSON.Decode.null", () => {
  test("JSON.Decode.null", () => {
    module Test = {
      JSON.parseExn(`null`)->JSON.Decode.null
      // Some(null)

      JSON.parseExn(`"hello world"`)->JSON.Decode.null
      // None
    }
    ()
  })
})

describe("JSON.Decode.bool", () => {
  test("JSON.Decode.bool", () => {
    module Test = {
      JSON.parseExn(`true`)->JSON.Decode.bool
      // Some(true)

      JSON.parseExn(`"hello world"`)->JSON.Decode.bool
      // None
    }
    ()
  })
})

describe("JSON.Encode.array", () => {
  test("JSON.Encode.array", () => {
    module Test = {
      let array = [JSON.Encode.string("hello world"), JSON.Encode.int(42)]

      JSON.Encode.array(array)
    }
    ()
  })
})

describe("JSON.Encode.object", () => {
  test("JSON.Encode.object", () => {
    module Test = {
      let dict = Dict.fromArray([
        ("foo", JSON.Encode.string("bar")),
        ("hello", JSON.Encode.string("world")),
      ])

      JSON.Encode.object(dict)
    }
    ()
  })
})

describe("JSON.Encode.float", () => {
  test("JSON.Encode.float", () => {
    module Test = {
      JSON.Encode.float(42.0)
    }
    ()
  })
})

describe("JSON.Encode.int", () => {
  test("JSON.Encode.int", () => {
    module Test = {
      JSON.Encode.int(42)
    }
    ()
  })
})

describe("JSON.Encode.string", () => {
  test("JSON.Encode.string", () => {
    module Test = {
      JSON.Encode.string("hello world")
    }
    ()
  })
})

describe("JSON.Encode.null", () => {
  test("JSON.Encode.null", () => {
    module Test = {
      JSON.Encode.null
    }
    ()
  })
})

describe("JSON.Encode.bool", () => {
  test("JSON.Encode.bool", () => {
    module Test = {
      JSON.Encode.bool(true)
    }
    ()
  })
})

describe("JSON.Classify.classify", () => {
  test("JSON.Classify.classify", () => {
    module Test = {
      JSON.Classify.classify("hello world")
      // String("hello world")

      JSON.Classify.classify(42)
      // Number(42)
    }
    ()
  })
})

describe("JSON.stringifyAnyWithFilterAndIndent", () => {
  test("JSON.stringifyAnyWithFilterAndIndent", () => {
    module Test = {
      let dict = Dict.fromArray([
        ("foo", JSON.Encode.string("bar")),
        ("hello", JSON.Encode.string("world")),
        ("someNumber", JSON.Encode.int(42)),
      ])

      dict
      ->JSON.stringifyAny
      ->Option.getUnsafe
      ->assertEqual(`{"foo":"bar","hello":"world","someNumber":42}`)

      dict
      ->JSON.stringifyAny(~space=2)
      ->Option.getUnsafe
      ->assertEqual(`{
  "foo": "bar",
  "hello": "world",
  "someNumber": 42
}`)

      dict
      ->JSON.stringifyAny(~replacer=Keys(["foo", "someNumber"]))
      ->Option.getUnsafe
      ->assertEqual(`{"foo":"bar","someNumber":42}`)

      JSON.stringifyAny(() => "hello world")->assertEqual(None)

      switch BigInt.fromInt(0)->JSON.stringifyAny {
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("JSON.stringifyAnyWithFilter", () => {
  test("JSON.stringifyAnyWithFilter", () => {
    module Test = {
      let dict = Dict.fromArray([
        ("foo", JSON.Encode.string("bar")),
        ("hello", JSON.Encode.string("world")),
        ("someNumber", JSON.Encode.int(42)),
      ])

      dict
      ->JSON.stringifyAnyWithFilter(["foo", "someNumber"])
      ->assertEqual(`{"foo":"bar","someNumber":42}`)

      JSON.stringifyAny(() => "hello world")->assertEqual(None)

      switch BigInt.fromInt(0)->JSON.stringifyAny {
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("JSON.stringifyAnyWithReplacerAndIndent", () => {
  test("JSON.stringifyAnyWithReplacerAndIndent", () => {
    module Test = {
      let dict = Dict.fromArray([
        ("foo", JSON.Encode.string("bar")),
        ("hello", JSON.Encode.string("world")),
        ("someNumber", JSON.Encode.int(42)),
      ])

      let replacer = (_, value) => {
        let decodedValue = value->JSON.Decode.string

        switch decodedValue {
        | Some(string) => string->String.toUpperCase->JSON.Encode.string
        | None => value
        }
      }

      dict
      ->JSON.stringifyAnyWithReplacer(replacer)
      ->Option.getUnsafe
      ->assertEqual(`{"foo":"BAR","hello":"WORLD","someNumber":42}`)

      JSON.stringifyAny(() => "hello world")->assertEqual(None)

      switch BigInt.fromInt(0)->JSON.stringifyAny {
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("JSON.stringifyAnyWithReplacer", () => {
  test("JSON.stringifyAnyWithReplacer", () => {
    module Test = {
      let dict = Dict.fromArray([
        ("foo", JSON.Encode.string("bar")),
        ("hello", JSON.Encode.string("world")),
        ("someNumber", JSON.Encode.int(42)),
      ])

      let replacer = (_, value) => {
        let decodedValue = value->JSON.Decode.string

        switch decodedValue {
        | Some(string) => string->String.toUpperCase->JSON.Encode.string
        | None => value
        }
      }

      dict
      ->JSON.stringifyAnyWithReplacer(replacer)
      ->Option.getUnsafe
      ->assertEqual(`{"foo":"BAR","hello":"WORLD","someNumber":42}`)

      JSON.stringifyAny(() => "hello world")->assertEqual(None)

      switch BigInt.fromInt(0)->JSON.stringifyAny {
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("JSON.stringifyAnyWithIndent", () => {
  test("JSON.stringifyAnyWithIndent", () => {
    module Test = {
      let dict = Dict.fromArray([
        ("foo", JSON.Encode.string("bar")),
        ("hello", JSON.Encode.string("world")),
        ("someNumber", JSON.Encode.int(42)),
      ])

      dict
      ->JSON.stringifyAnyWithIndent(2)
      ->Option.getUnsafe
      ->assertEqual(`{
  "foo": "bar",
  "hello": "world",
  "someNumber": 42
}`)

      JSON.stringifyAny(() => "hello world")->assertEqual(None)

      switch BigInt.fromInt(0)->JSON.stringifyAny {
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("JSON.stringifyAny", () => {
  test("JSON.stringifyAny", () => {
    module Test = {
      let dict = Dict.fromArray([
        ("foo", JSON.Encode.string("bar")),
        ("hello", JSON.Encode.string("world")),
        ("someNumber", JSON.Encode.int(42)),
      ])

      dict
      ->JSON.stringifyAny
      ->Option.getUnsafe
      ->assertEqual(`{"foo":"bar","hello":"world","someNumber":42}`)

      dict
      ->JSON.stringifyAny(~space=2)
      ->Option.getUnsafe
      ->assertEqual(`{
  "foo": "bar",
  "hello": "world",
  "someNumber": 42
}`)

      dict
      ->JSON.stringifyAny(~replacer=Keys(["foo", "someNumber"]))
      ->Option.getUnsafe
      ->assertEqual(`{"foo":"bar","someNumber":42}`)

      let replacer = JSON.Replacer(
        (_, value) => {
          let decodedValue = value->JSON.Decode.string

          switch decodedValue {
          | Some(string) => string->String.toUpperCase->JSON.Encode.string
          | None => value
          }
        },
      )

      dict
      ->JSON.stringifyAny(~replacer)
      ->Option.getUnsafe
      ->assertEqual(`{"foo":"BAR","hello":"WORLD","someNumber":42}`)

      JSON.stringifyAny(() => "hello world")->assertEqual(None)

      // Raise a exception
      switch BigInt.fromInt(0)->JSON.stringifyAny {
      | exception _ => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("JSON.stringifyWithFilterAndIndent", () => {
  test("JSON.stringifyWithFilterAndIndent", () => {
    module Test = {
      let json =
        Dict.fromArray([
          ("foo", JSON.Encode.string("bar")),
          ("hello", JSON.Encode.string("world")),
          ("someNumber", JSON.Encode.int(42)),
        ])->JSON.Encode.object

      JSON.stringifyWithFilterAndIndent(json, ["foo", "someNumber"], 2)
      // {
      //   "foo": "bar",
      //   "someNumber": 42
      // }
    }
    ()
  })
})

describe("JSON.stringifyWithFilter", () => {
  test("JSON.stringifyWithFilter", () => {
    module Test = {
      let json =
        Dict.fromArray([
          ("foo", JSON.Encode.string("bar")),
          ("hello", JSON.Encode.string("world")),
          ("someNumber", JSON.Encode.int(42)),
        ])->JSON.Encode.object

      JSON.stringifyWithFilter(json, ["foo", "someNumber"])
      // {"foo":"bar","someNumber":42}
    }
    ()
  })
})

describe("JSON.stringifyWithReplacerAndIndent", () => {
  test("JSON.stringifyWithReplacerAndIndent", () => {
    module Test = {
      let json =
        Dict.fromArray([
          ("foo", JSON.Encode.string("bar")),
          ("hello", JSON.Encode.string("world")),
          ("someNumber", JSON.Encode.int(42)),
        ])->JSON.Encode.object

      let replacer = (_, value) => {
        let decodedValue = value->JSON.Decode.string

        switch decodedValue {
        | Some(string) => string->String.toUpperCase->JSON.Encode.string
        | None => value
        }
      }

      JSON.stringifyWithReplacerAndIndent(json, replacer, 2)
      // {
      //   "foo": "BAR",
      //   "hello": "WORLD",
      //   "someNumber": 42
      // }
    }
    ()
  })
})

describe("JSON.stringifyWithReplacer", () => {
  test("JSON.stringifyWithReplacer", () => {
    module Test = {
      let json =
        Dict.fromArray([
          ("foo", JSON.Encode.string("bar")),
          ("hello", JSON.Encode.string("world")),
          ("someNumber", JSON.Encode.int(42)),
        ])->JSON.Encode.object

      let replacer = (_, value) => {
        let decodedValue = value->JSON.Decode.string

        switch decodedValue {
        | Some(string) => string->String.toUpperCase->JSON.Encode.string
        | None => value
        }
      }

      JSON.stringifyWithReplacer(json, replacer)
      // {"foo":"BAR","hello":"WORLD","someNumber":42}
    }
    ()
  })
})

describe("JSON.stringifyWithIndent", () => {
  test("JSON.stringifyWithIndent", () => {
    module Test = {
      let json =
        Dict.fromArray([
          ("foo", JSON.Encode.string("bar")),
          ("hello", JSON.Encode.string("world")),
          ("someNumber", JSON.Encode.int(42)),
        ])->JSON.Encode.object

      JSON.stringifyWithIndent(json, 2)
      // {
      //   "foo": "bar",
      //   "hello": "world",
      //   "someNumber": 42
      // }
    }
    ()
  })
})

describe("JSON.stringify", () => {
  test("JSON.stringify", () => {
    module Test = {
      let json =
        Dict.fromArray([
          ("foo", JSON.Encode.string("bar")),
          ("hello", JSON.Encode.string("world")),
          ("someNumber", JSON.Encode.int(42)),
        ])->JSON.Encode.object

      JSON.stringify(json)
      // {"foo":"bar","hello":"world","someNumber":42}

      JSON.stringify(json, ~space=2)
      // {
      //   "foo": "bar",
      //   "hello": "world",
      //   "someNumber": 42
      // }

      JSON.stringify(json, ~replacer=Keys(["foo", "someNumber"]))
      // {"foo":"bar","someNumber":42}

      let replacer = JSON.Replacer(
        (_, value) => {
          let decodedValue = value->JSON.Decode.string

          switch decodedValue {
          | Some(string) => string->String.toUpperCase->JSON.Encode.string
          | None => value
          }
        },
      )

      JSON.stringify(json, ~replacer)
      // {"foo":"BAR","hello":"WORLD","someNumber":42}
    }
    ()
  })
})

describe("JSON.parseExnWithReviver", () => {
  test("JSON.parseExnWithReviver", () => {
    module Test = {
      let reviver = (_, value: JSON.t) =>
        switch value {
        | String(string) => string->String.toUpperCase->JSON.Encode.string
        | Number(number) => (number *. 2.0)->JSON.Encode.float
        | _ => value
        }

      let jsonString = `{"hello":"world","someNumber":21}`

      try {
        JSON.parseExnWithReviver(jsonString, reviver)->Console.log
        // { hello: 'WORLD', someNumber: 42 }

        JSON.parseExnWithReviver("", reviver)->Console.log
        // error
      } catch {
      | Exn.Error(_) => Console.log("error")
      }
    }
    ()
  })
})

describe("JSON.parseExn", () => {
  test("JSON.parseExn", () => {
    module Test = {
      try {
        let _ = JSON.parseExn(`{"foo":"bar","hello":"world"}`)
        // { foo: 'bar', hello: 'world' }

        let _ = JSON.parseExn("")
        // error
      } catch {
      | Exn.Error(_) => Console.log("error")
      }

      let reviver = (_, value: JSON.t) =>
        switch value {
        | String(string) => string->String.toUpperCase->JSON.Encode.string
        | Number(number) => (number *. 2.0)->JSON.Encode.float
        | _ => value
        }

      let jsonString = `{"hello":"world","someNumber":21}`

      try {
        JSON.parseExn(jsonString, ~reviver)->Console.log
        // { hello: 'WORLD', someNumber: 42 }

        JSON.parseExn("", ~reviver)->Console.log
        // error
      } catch {
      | Exn.Error(_) => Console.log("error")
      }
    }
    ()
  })
})

describe("Map.entries", () => {
  test("Map.entries", () => {
    module Test = {
      let map = Map.make()
      map->Map.set("someKey", "someValue")
      map->Map.set("anotherKey", "anotherValue")

      let entries = map->Map.entries

      // Logs the first value
      Console.log(Iterator.next(entries).value)

      // You can also turn the iterator into an array.
      // Remember that an iterator consumes entries. We'll need a fresh entries iterator to get an array of all entries, since we consumed a value via `next` above already.
      Console.log(map->Map.entries->Iterator.toArray)
    }
    ()
  })
})

describe("Map.values", () => {
  test("Map.values", () => {
    module Test = {
      let map = Map.make()
      map->Map.set("someKey", "someValue")
      map->Map.set("anotherKey", "anotherValue")

      let values = map->Map.values

      // Logs the first value
      Console.log(Iterator.next(values).value)

      // You can also turn the iterator into an array.
      // Remember that an iterator consumes values. We'll need a fresh values iterator to get an array of all values, since we consumed a value via `next` above already.
      Console.log(map->Map.values->Iterator.toArray)
    }
    ()
  })
})

describe("Map.keys", () => {
  test("Map.keys", () => {
    module Test = {
      let map = Map.make()
      map->Map.set("someKey", "someValue")
      map->Map.set("anotherKey", "anotherValue")

      let keys = map->Map.keys

      // Logs the first key
      Console.log(Iterator.next(keys).value)

      // You can also turn the iterator into an array.
      // Remember that an iterator consumes values. We'll need a fresh keys iterator to get an array of all keys, since we consumed a value via `next` above already.
      Console.log(map->Map.keys->Iterator.toArray)
    }
    ()
  })
})

describe("Map.delete", () => {
  test("Map.delete", () => {
    module Test = {
      let map = Map.make()
      map->Map.set("someKey", "someValue")
      let didDeleteKey = map->Map.delete("someKey")
      Console.log(didDeleteKey) // Logs `true` to the console, becuase the map had the key, so it was successfully deleted

      let didDeleteKey = map->Map.delete("someNonExistantKey")
      Console.log(didDeleteKey) // Logs `false` to the console, becuase the key did not exist
    }
    ()
  })
})

describe("Map.set", () => {
  test("Map.set", () => {
    module Test = {
      let map = Map.make()
      map->Map.set("someKey", "someValue")
    }
    ()
  })
})

describe("Map.has", () => {
  test("Map.has", () => {
    module Test = {
      let map = Map.make()
      map->Map.set("someKey", "someValue")

      switch map->Map.has("someKey") {
      | false => Console.log("Nope, didn't have it.")
      | true => Console.log("Yay, we have the value!")
      }
    }
    ()
  })
})

describe("Map.get", () => {
  test("Map.get", () => {
    module Test = {
      let map = Map.make()
      map->Map.set("someKey", "someValue")

      switch map->Map.get("someKey") {
      | None => Console.log("Nope, didn't have it.")
      | Some(value) => Console.log2("Yay, had the value, and it's:", value)
      }
    }
    ()
  })
})

describe("Map.forEachWithKey", () => {
  test("Map.forEachWithKey", () => {
    module Test = {
      let map = Map.make()
      map->Map.set("someKey", "someValue")
      map->Map.set("someKey2", "someValue2")

      map->Map.forEachWithKey(
        (value, key) => {
          Console.log2(value, key)
        },
      )
    }
    ()
  })
})

describe("Map.forEach", () => {
  test("Map.forEach", () => {
    module Test = {
      let map = Map.make()
      map->Map.set("someKey", "someValue")
      map->Map.set("someKey2", "someValue2")

      map->Map.forEach(
        value => {
          Console.log(value)
        },
      )
    }
    ()
  })
})

describe("Map.clear", () => {
  test("Map.clear", () => {
    module Test = {
      let map = Map.make()

      map->Map.set("someKey", "someValue")
      map->Map.size // 1

      map->Map.clear
      map->Map.size // 0
    }
    ()
  })
})

describe("Map.size", () => {
  test("Map.size", () => {
    module Test = {
      let map = Map.make()

      map->Map.set("someKey", "someValue")

      let size = map->Map.size // 1
    }
    ()
  })
})

describe("Map.fromIterator", () => {
  test("Map.fromIterator", () => {
    module Test = {
      // Let's pretend we have an interator in the correct shape
      let iterator: Iterator.t<(string, string)> = %raw(`
  (() => {
    var map1 = new Map();

    map1.set('first', '1');
    map1.set('second', '2');

    var iterator1 = map1[Symbol.iterator]();
    return iterator1;
  })()
`)

      iterator
      ->Map.fromIterator
      ->Map.size
      ->assertEqual(2)
    }
    ()
  })
})

describe("Map.fromArray", () => {
  test("Map.fromArray", () => {
    module Test = {
      type languages = ReScript | JavaScript | TypeScript
      let languageRank = [(ReScript, 1), (JavaScript, 2), (TypeScript, 3)]

      let map = Map.fromArray(languageRank) // Map.t<languages, int>

      switch map->Map.get(ReScript) {
      | Some(1) => Console.log("Yay, ReScript is #1!")
      | _ => Console.log("Uh-oh, something is _terribly_ wrong with this program... abort.")
      }
    }
    ()
  })
})

describe("Map.make", () => {
  test("Map.make", () => {
    module Test = {
      `make()`
      // You can annotate the type of your map if you want to
      let myMap: Map.t<string, int> = Map.make()

      // Or you can let ReScript infer what's in your map
      let map = Map.make()
      map->Map.set("lang", "ReScript") // Inferred as Map.t<string, string>
    }
    ()
  })
})

describe("Null.flatMap", () => {
  test("Null.flatMap", () => {
    module Test = {
      let addIfAboveOne = value =>
        if value > 1 {
          Null.make(value + 1)
        } else {
          Null.null
        }

      Null.flatMap(Null.make(2), addIfAboveOne) // Null.make(3)
      Null.flatMap(Null.make(-4), addIfAboveOne) // null
      Null.flatMap(Null.null, addIfAboveOne) // null
    }
    ()
  })
})

describe("Null.mapOr", () => {
  test("Null.mapOr", () => {
    module Test = {
      let someValue = Null.make(3)
      someValue->Null.mapOr(0, x => x + 5) // 8

      let noneValue = Null.null
      noneValue->Null.mapOr(0, x => x + 5) // 0
    }
    ()
  })
})

describe("Null.map", () => {
  test("Null.map", () => {
    module Test = {
      Null.map(Null.make(3), x => x * x) // Null.make(9)
      Null.map(Null.null, x => x * x) // null
    }
    ()
  })
})

describe("Null.forEach", () => {
  test("Null.forEach", () => {
    module Test = {
      Null.forEach(Null.make("thing"), x => Console.log(x)) // logs "thing"
      Null.forEach(Null.null, x => Console.log(x)) // logs nothing
    }
    ()
  })
})

describe("Null.getUnsafe", () => {
  test("Null.getUnsafe", () => {
    module Test = {
      Null.getUnsafe(Null.make(3)) == 3
      Null.getUnsafe(Null.null) // Raises an error
    }
    ()
  })
})

describe("Null.getExn", () => {
  test("Null.getExn", () => {
    module Test = {
      Null.getExn(Null.make(3))->assertEqual(3)

      switch Null.getExn(%raw("'ReScript'")) {
      | exception Invalid_argument(_) => assert(false)
      | value => assertEqual(value, "ReScript")
      }

      switch Null.getExn(%raw("null")) {
      | exception Invalid_argument(_) => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Null.getOr", () => {
  test("Null.getOr", () => {
    module Test = {
      Null.getOr(Null.null, "Banana") // Banana
      Null.getOr(Null.make("Apple"), "Banana") // Apple

      let greet = (firstName: option<string>) =>
        "Greetings " ++ firstName->Option.getOr("Anonymous")

      Null.make("Jane")->Null.toOption->greet // "Greetings Jane"
      Null.null->Null.toOption->greet // "Greetings Anonymous"
    }
    ()
  })
})

describe("Null.fromOption", () => {
  test("Null.fromOption", () => {
    module Test = {
      let optString: option<string> = None
      let asNull = optString->Null.fromOption // Null.t<string>
      Console.log(asNull == Null.null) // Logs `true` to the console.
    }
    ()
  })
})

describe("Null.toOption", () => {
  test("Null.toOption", () => {
    module Test = {
      let nullStr = Null.make("Hello")

      switch nullStr->Null.toOption {
      | Some(str) => Console.log2("Got string:", str)
      | None => Console.log("Didn't have a value.")
      }
    }
    ()
  })
})

describe("Null.make", () => {
  test("Null.make", () => {
    module Test = {
      let myStr = "Hello"
      let asNullValue = myStr->Null.make // The compiler now thinks this can be `string` or `null`.
    }
    ()
  })
})

describe("Null.null", () => {
  test("Null.null", () => {
    module Test = {
      Console.log(null) // Logs `null` to the console.
    }
    ()
  })
})

describe("Null.asNullable", () => {
  test("Null.asNullable", () => {
    module Test = {
      let nullValue = Null.make("Hello")
      let asNullable = nullValue->Null.asNullable // Nullable.t<string>
    }
    ()
  })
})

describe("List.sort", () => {
  test("List.sort", () => {
    module Test = {
      List.sort(list{5, 4, 9, 3, 7}, Int.compare) // list{3, 4, 5, 7, 9}
    }
    ()
  })
})

describe("List.setAssoc", () => {
  test("List.setAssoc", () => {
    module Test = {
      list{(1, "a"), (2, "b"), (3, "c")}->List.setAssoc(2, "x", (a, b) => a == b) // list{(1, "a"), (2, "x"), (3, "c")}

      list{(1, "a"), (3, "c")}->List.setAssoc(2, "b", (a, b) => a == b) // list{(2, "b"), (1, "a"), (3, "c")}

      list{(9, "morning"), (3, "morning?!"), (22, "night")}->List.setAssoc(
        15,
        "afternoon",
        (a, b) => mod(a, 12) == mod(b, 12),
      )
      // list{(9, "morning"), (15, "afternoon"), (22, "night")}
    }
    ()
  })
})

describe("List.removeAssoc", () => {
  test("List.removeAssoc", () => {
    module Test = {
      list{(1, "a"), (2, "b"), (3, "c")}->List.removeAssoc(1, (a, b) => a == b) // list{(2, "b"), (3, "c")}

      list{(9, "morning"), (15, "afternoon"), (22, "night")}->List.removeAssoc(
        9,
        (k, item) => k /* 9 */ == item /* 9, 5, 22 */,
      )
      // list{(15, "afternoon"), (22, "night")}
    }
    ()
  })
})

describe("List.hasAssoc", () => {
  test("List.hasAssoc", () => {
    module Test = {
      list{(1, "a"), (2, "b"), (3, "c")}->List.hasAssoc(1, (a, b) => a == b) // true

      list{(9, "morning"), (15, "afternoon"), (22, "night")}->List.hasAssoc(
        25,
        (k, item) => k /* 25 */ == item /* 9, 5, 22 */,
      ) // false
    }
    ()
  })
})

describe("List.getAssoc", () => {
  test("List.getAssoc", () => {
    module Test = {
      list{(1, "a"), (2, "b"), (3, "c")}->List.getAssoc(3, (a, b) => a == b) // Some("c")

      list{(9, "morning"), (15, "afternoon"), (22, "night")}->List.getAssoc(
        15,
        (k, item) => k /* 15 */ == item /* 9, 5, 22 */,
      )
      // Some("afternoon")
    }
    ()
  })
})

describe("List.unzip", () => {
  test("List.unzip", () => {
    module Test = {
      List.unzip(list{(1, 2), (3, 4)}) // (list{1, 3}, list{2, 4})

      List.unzip(list{("H", "W"), ("e", "o"), ("l", "r"), ("l", "l"), ("o", "d"), (" ", "!")})
      // (list{"H", "e", "l", "l", "o", " "}, list{"W", "o", "r", "l", "d", "!"})
    }
    ()
  })
})

describe("List.partition", () => {
  test("List.partition", () => {
    module Test = {
      // (elementsThatSatisfies, elementsThatDoesNotSatisfy)

      List.partition(list{1, 2, 3, 4}, x => x > 2) // (list{3, 4}, list{1, 2})
    }
    ()
  })
})

describe("List.filterMap", () => {
  test("List.filterMap", () => {
    module Test = {
      let isEven = x => mod(x, 2) == 0

      list{1, 2, 3, 4}->List.filterMap(
        x =>
          if isEven(x) {
            Some(x)
          } else {
            None
          },
      ) // list{2, 4}

      list{Some(1), Some(2), None}->List.filterMap(x => x) // list{1, 2}
    }
    ()
  })
})

describe("List.filterWithIndex", () => {
  test("List.filterWithIndex", () => {
    module Test = {
      let isEven = x => mod(x, 2) == 0

      List.filterWithIndex(list{1, 2, 3, 4}, (_x, index) => isEven(index)) // list{1, 3}
    }
    ()
  })
})

describe("List.filter", () => {
  test("List.filter", () => {
    module Test = {
      let isEven = x => mod(x, 2) == 0

      List.filter(list{1, 2, 3, 4}, isEven) // list{2, 4}

      List.filter(list{None, Some(2), Some(3), None}, Option.isSome) // list{Some(2), Some(3)}
    }
    ()
  })
})

describe("List.find", () => {
  test("List.find", () => {
    module Test = {
      List.find(list{1, 4, 3, 2}, x => x > 3) // Some(4)

      List.find(list{1, 4, 3, 2}, x => x > 4) // None
    }
    ()
  })
})

describe("List.has", () => {
  test("List.has", () => {
    module Test = {
      list{1, 2, 3}->List.has(2, (a, b) => a == b) // true

      list{1, 2, 3}->List.has(4, (a, b) => a == b) // false

      list{-1, -2, -3}->List.has(2, (a, b) => abs(a) == abs(b)) // true
    }
    ()
  })
})

describe("List.equal", () => {
  test("List.equal", () => {
    module Test = {
      List.equal(list{1, 2, 3}, list{1, 2}, (a, b) => a == b) // false

      List.equal(list{1, 2}, list{1, 2}, (a, b) => a == b) // true

      List.equal(list{1, 2, 3}, list{-1, -2, -3}, (a, b) => abs(a) == abs(b)) // true
    }
    ()
  })
})

describe("List.compare", () => {
  test("List.compare", () => {
    module Test = {
      List.compare(list{3}, list{3, 7}, (a, b) => Int.compare(a, b)) // -1.
      List.compare(list{5, 3}, list{5}, (a, b) => Int.compare(a, b)) // 1.
      List.compare(list{1, 3, 5}, list{1, 4, 2}, (a, b) => Int.compare(a, b)) // -1.
      List.compare(list{1, 3, 5}, list{1, 2, 3}, (a, b) => Int.compare(a, b)) // 1.
      List.compare(list{1, 3, 5}, list{1, 3, 5}, (a, b) => Int.compare(a, b)) // 0.
    }
    ()
  })
})

describe("List.compareLength", () => {
  test("List.compareLength", () => {
    module Test = {
      List.compareLength(list{1, 2}, list{3, 4, 5, 6}) // -1.

      List.compareLength(list{1, 2, 3}, list{4, 5, 6}) // 0.

      List.compareLength(list{1, 2, 3, 4}, list{5, 6}) // 1.
    }
    ()
  })
})

describe("List.some2", () => {
  test("List.some2", () => {
    module Test = {
      List.some2(list{1, 2, 3}, list{0, 1}, (a, b) => a > b) // true

      List.some2(list{}, list{1}, (a, b) => a > b) // false

      List.some2(list{2, 3}, list{1}, (a, b) => a > b) // true

      List.some2(list{0, 1}, list{5, 0}, (a, b) => a > b) // true
    }
    ()
  })
})

describe("List.every2", () => {
  test("List.every2", () => {
    module Test = {
      List.every2(list{1, 2, 3}, list{0, 1}, (a, b) => a > b) // true

      List.every2(list{}, list{1}, (a, b) => a > b) // true

      List.every2(list{2, 3}, list{1}, (a, b) => a > b) // true

      List.every2(list{0, 1}, list{5, 0}, (a, b) => a > b) // false
    }
    ()
  })
})

describe("List.some", () => {
  test("List.some", () => {
    module Test = {
      let isAbove100 = value => value > 100

      list{101, 1, 2, 3}->List.some(isAbove100) // true

      list{1, 2, 3, 4}->List.some(isAbove100) // false
    }
    ()
  })
})

describe("List.every", () => {
  test("List.every", () => {
    module Test = {
      let isBelow10 = value => value < 10

      list{1, 9, 8, 2}->List.every(isBelow10) // true

      list{1, 99, 8, 2}->List.every(isBelow10) // false
    }
    ()
  })
})

describe("List.reduceReverse2", () => {
  test("List.reduceReverse2", () => {
    module Test = {
      List.reduceReverse2(list{1, 2, 3}, list{4, 5}, 0, (acc, x, y) => acc + x * x + y) //  + (1 * 1 + 4) + (2 * 2 + 5)
    }
    ()
  })
})

describe("List.reduce2", () => {
  test("List.reduce2", () => {
    module Test = {
      List.reduce2(list{1, 2, 3}, list{4, 5}, 0, (acc, x, y) => acc + x * x + y) // 0 + (1 * 1 + 4) + (2 * 2 + 5)
    }
    ()
  })
})

describe("List.forEach2", () => {
  test("List.forEach2", () => {
    module Test = {
      List.forEach2(list{"Z", "Y"}, list{"A", "B", "C"}, (x, y) => Console.log2(x, y))

      /*
  prints:
  "Z" "A"
  "Y" "B"
*/
    }
    ()
  })
})

describe("List.mapReverse2", () => {
  test("List.mapReverse2", () => {
    module Test = {
      List.mapReverse2(list{1, 2, 3}, list{1, 2}, (a, b) => a + b) // list{4, 2}
    }
    ()
  })
})

describe("List.reduceReverse", () => {
  test("List.reduceReverse", () => {
    module Test = {
      list{1, 2, 3, 4}->List.reduceReverse(0, (a, b) => a + b) // 10

      list{1, 2, 3, 4}->List.reduceReverse(10, (a, b) => a - b) // 0

      list{1, 2, 3, 4}->List.reduceReverse(list{}, List.add) // list{1, 2, 3, 4}
    }
    ()
  })
})

describe("List.reduceWithIndex", () => {
  test("List.reduceWithIndex", () => {
    module Test = {
      list{1, 2, 3, 4}->List.reduceWithIndex(0, (acc, item, index) => acc + item + index) // 16
    }
    ()
  })
})

describe("List.reduce", () => {
  test("List.reduce", () => {
    module Test = {
      list{1, 2, 3, 4}->List.reduce(0, (a, b) => a + b) // 10

      // same as

      list{1, 2, 3, 4}->List.reduce(0, (acc, item) => acc + item) // 10
    }
    ()
  })
})

describe("List.forEachWithIndex", () => {
  test("List.forEachWithIndex", () => {
    module Test = {
      List.forEachWithIndex(
        list{"a", "b", "c"},
        (x, index) => {
          Console.log("Item " ++ Int.toString(index) ++ " is " ++ x)
        },
      )
      /*
  prints:
  Item 0 is a
  Item 1 is b
  Item 2 is cc
*/
    }
    ()
  })
})

describe("List.forEach", () => {
  test("List.forEach", () => {
    module Test = {
      List.forEach(list{"a", "b", "c"}, x => Console.log("Item: " ++ x))
      /*
  prints:
  Item: a
  Item: b
  Item: c
*/
    }
    ()
  })
})

describe("List.mapReverse", () => {
  test("List.mapReverse", () => {
    module Test = {
      let f = x => x * x
      let l = list{3, 4, 5}

      let withMap = List.map(l, f)->List.reverse
      let withMapReverse = l->List.mapReverse(f)

      Console.log(withMap == withMapReverse) // true
    }
    ()
  })
})

describe("List.reverse", () => {
  test("List.reverse", () => {
    module Test = {
      List.reverse(list{1, 2, 3}) // list{3, 2, 1}
    }
    ()
  })
})

describe("List.toArray", () => {
  test("List.toArray", () => {
    module Test = {
      List.toArray(list{1, 2, 3}) // [1, 2, 3]
    }
    ()
  })
})

describe("List.fromArray", () => {
  test("List.fromArray", () => {
    module Test = {
      List.fromArray([1, 2, 3]) // list{1, 2, 3}
    }
    ()
  })
})

describe("List.mapWithIndex", () => {
  test("List.mapWithIndex", () => {
    module Test = {
      list{1, 2, 3}->List.mapWithIndex((x, index) => index + x) // list{1, 3, 5}
    }
    ()
  })
})

describe("List.zipBy", () => {
  test("List.zipBy", () => {
    module Test = {
      List.zipBy(list{1, 2, 3}, list{4, 5}, (a, b) => 2 * a + b) // list{6, 9}
    }
    ()
  })
})

describe("List.zip", () => {
  test("List.zip", () => {
    module Test = {
      List.zip(list{1, 2}, list{3, 4, 5}) // list{(1, 3), (2, 4)}
    }
    ()
  })
})

describe("List.map", () => {
  test("List.map", () => {
    module Test = {
      list{1, 2}->List.map(x => x + 1) // list{3, 4}
    }
    ()
  })
})

describe("List.flat", () => {
  test("List.flat", () => {
    module Test = {
      List.flat(list{list{1, 2, 3}, list{}, list{3}}) // list{1, 2, 3, 3}
    }
    ()
  })
})

describe("List.reverseConcat", () => {
  test("List.reverseConcat", () => {
    module Test = {
      List.reverseConcat(list{1, 2}, list{3, 4}) // list{2, 1, 3, 4}
    }
    ()
  })
})

describe("List.concatMany", () => {
  test("List.concatMany", () => {
    module Test = {
      List.concatMany([list{1, 2, 3}, list{}, list{3}]) // list{1, 2, 3, 3}
    }
    ()
  })
})

describe("List.concat", () => {
  test("List.concat", () => {
    module Test = {
      List.concat(list{1, 2, 3}, list{4, 5}) // list{1, 2, 3, 4, 5}
    }
    ()
  })
})

describe("List.splitAt", () => {
  test("List.splitAt", () => {
    module Test = {
      list{"Hello", "World"}->List.splitAt(1) // Some((list{"Hello"}, list{"World"}))

      list{0, 1, 2, 3, 4}->List.splitAt(2) // Some((list{0, 1}, list{2, 3, 4}))
    }
    ()
  })
})

describe("List.take", () => {
  test("List.take", () => {
    module Test = {
      list{1, 2, 3}->List.take(1) // Some(list{1})

      list{1, 2, 3}->List.take(2) // Some(list{1, 2})

      list{1, 2, 3}->List.take(4) // None
    }
    ()
  })
})

describe("List.drop", () => {
  test("List.drop", () => {
    module Test = {
      list{1, 2, 3}->List.drop(2) // Some(list{3})

      list{1, 2, 3}->List.drop(3) // Some(list{})

      list{1, 2, 3}->List.drop(4) // None
    }
    ()
  })
})

describe("List.toShuffled", () => {
  test("List.toShuffled", () => {
    module Test = {
      List.toShuffled(list{1, 2, 3}) // list{2, 1, 3}
    }
    ()
  })
})

describe("List.fromInitializer", () => {
  test("List.fromInitializer", () => {
    module Test = {
      List.fromInitializer(~length=5, i => i) // list{0, 1, 2, 3, 4}

      List.fromInitializer(~length=5, i => i * i) // list{0, 1, 4, 9, 16}
    }
    ()
  })
})

describe("List.make", () => {
  test("List.make", () => {
    module Test = {
      List.make(~length=3, 1) // list{1, 1, 1}
    }
    ()
  })
})

describe("List.getExn", () => {
  test("List.getExn", () => {
    module Test = {
      let abc = list{"A", "B", "C"}

      abc
      ->List.getExn(1)
      ->assertEqual("B")

      switch abc->List.getExn(4) {
      | exception Not_found => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("List.get", () => {
  test("List.get", () => {
    module Test = {
      let abc = list{"A", "B", "C"}

      abc->List.get(1) // Some("B")

      abc->List.get(4) // None
    }
    ()
  })
})

describe("List.add", () => {
  test("List.add", () => {
    module Test = {
      List.add(list{2, 3}, 1) // list{1, 2, 3}

      List.add(list{"World", "!"}, "Hello") // list{"Hello", "World", "!"}
    }
    ()
  })
})

describe("List.tailExn", () => {
  test("List.tailExn", () => {
    module Test = {
      List.tailExn(list{1, 2, 3})->assertEqual(list{2, 3})

      switch List.tailExn(list{}) {
      | exception Not_found => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("List.tail", () => {
  test("List.tail", () => {
    module Test = {
      List.tail(list{1, 2, 3}) // Some(list{2, 3})

      List.tail(list{}) // None
    }
    ()
  })
})

describe("List.headExn", () => {
  test("List.headExn", () => {
    module Test = {
      List.headExn(list{1, 2, 3})->assertEqual(1)

      switch List.headExn(list{}) {
      | exception Not_found => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("List.head", () => {
  test("List.head", () => {
    module Test = {
      List.head(list{}) // None
      List.head(list{1, 2, 3}) // Some(1)
    }
    ()
  })
})

describe("List.size", () => {
  test("List.size", () => {
    module Test = {
      List.size(list{1, 2, 3}) // 3
    }
    ()
  })
})

describe("List.length", () => {
  test("List.length", () => {
    module Test = {
      List.length(list{1, 2, 3}) // 3
    }
    ()
  })
})

describe("Math.Int.random", () => {
  test("Math.Int.random", () => {
    module Test = {
      Math.Int.random(2, 5) == 4
      Math.Int.random(505, 2000) == 1276
      Math.Int.random(-7, -2) == -4
    }
    ()
  })
})

describe("Math.Int.ceil", () => {
  test("Math.Int.ceil", () => {
    module Test = {
      Math.Int.ceil(3.7) == 4
      Math.Int.ceil(3.0) == 3
      Math.Int.ceil(-3.1) == -3
    }
    ()
  })
})

describe("Math.Int.floor", () => {
  test("Math.Int.floor", () => {
    module Test = {
      Math.Int.floor(3.7) == 3
      Math.Int.floor(3.0) == 3
      Math.Int.floor(-3.1) == -4
    }
    ()
  })
})

describe("Math.Int.sign", () => {
  test("Math.Int.sign", () => {
    module Test = {
      Math.Int.sign(3) // 1
      Math.Int.sign(-3) // -1
      Math.Int.sign(0) // 0
    }
    ()
  })
})

describe("Math.Int.pow", () => {
  test("Math.Int.pow", () => {
    module Test = {
      Math.Int.pow(2, ~exp=4) // 16
      Math.Int.pow(3, ~exp=4) // 81
    }
    ()
  })
})

describe("Math.Int.maxMany", () => {
  test("Math.Int.maxMany", () => {
    module Test = {
      Math.Int.maxMany([1, 2]) // 2
      Math.Int.maxMany([-1, -2]) // -1
      Math.Int.maxMany([])->Int.toFloat->Float.isFinite // false
    }
    ()
  })
})

describe("Math.Int.max", () => {
  test("Math.Int.max", () => {
    module Test = {
      Math.Int.max(1, 2) // 2
      Math.Int.max(-1, -2) // -1
    }
    ()
  })
})

describe("Math.Int.minMany", () => {
  test("Math.Int.minMany", () => {
    module Test = {
      Math.Int.minMany([1, 2]) // 1
      Math.Int.minMany([-1, -2]) // -2
      Math.Int.minMany([])->Int.toFloat->Float.isFinite // false
    }
    ()
  })
})

describe("Math.Int.min", () => {
  test("Math.Int.min", () => {
    module Test = {
      Math.Int.min(1, 2) // 1
      Math.Int.min(-1, -2) // -2
    }
    ()
  })
})

describe("Math.Int.imul", () => {
  test("Math.Int.imul", () => {
    module Test = {
      Math.Int.imul(3, 4) // 12
      Math.Int.imul(-5, 12) // 60
    }
    ()
  })
})

describe("Math.Int.clz32", () => {
  test("Math.Int.clz32", () => {
    module Test = {
      // 00000000000000000000000000000001
      Math.Int.clz32(1) // 31
      // 00000000000000000000000000000100
      Math.Int.clz32(4) // 29
    }
    ()
  })
})

describe("Math.Int.abs", () => {
  test("Math.Int.abs", () => {
    module Test = {
      Math.Int.abs(-2) // 2
      Math.Int.abs(3) // 3
    }
    ()
  })
})

describe("Math.Constants.sqrt2", () => {
  test("Math.Constants.sqrt2", () => {
    module Test = {
      Math.Constants.sqrt2
    }
    ()
  })
})

describe("Math.Constants.sqrt1_2", () => {
  test("Math.Constants.sqrt1_2", () => {
    module Test = {
      Math.Constants.sqrt1_2
    }
    ()
  })
})

describe("Math.Constants.pi", () => {
  test("Math.Constants.pi", () => {
    module Test = {
      Math.Constants.pi
    }
    ()
  })
})

describe("Math.Constants.log10e", () => {
  test("Math.Constants.log10e", () => {
    module Test = {
      Math.Constants.log10e
    }
    ()
  })
})

describe("Math.Constants.log2e", () => {
  test("Math.Constants.log2e", () => {
    module Test = {
      Math.Constants.log2e
    }
    ()
  })
})

describe("Math.Constants.ln10", () => {
  test("Math.Constants.ln10", () => {
    module Test = {
      Math.Constants.ln10
    }
    ()
  })
})

describe("Math.Constants.ln2", () => {
  test("Math.Constants.ln2", () => {
    module Test = {
      Math.Constants.ln2
    }
    ()
  })
})

describe("Math.Constants.e", () => {
  test("Math.Constants.e", () => {
    module Test = {
      Math.Constants.e
    }
    ()
  })
})

describe("Math.trunc", () => {
  test("Math.trunc", () => {
    module Test = {
      Math.trunc(0.123) // 0.0
      Math.trunc(1.999) // 1.0
      Math.trunc(13.37) // 13.0
      Math.trunc(42.84) // 42.0
    }
    ()
  })
})

describe("Math.tanh", () => {
  test("Math.tanh", () => {
    module Test = {
      Math.tanh(-0.0) // -0.0
      Math.tanh(0.0) // 0.0
      Math.tanh(1.0) // 0.7615941559557649
    }
    ()
  })
})

describe("Math.tan", () => {
  test("Math.tan", () => {
    module Test = {
      Math.tan(-0.0) // -0.0
      Math.tan(0.0) // 0.0
      Math.tan(1.0) // 1.5574077246549023
    }
    ()
  })
})

describe("Math.sqrt", () => {
  test("Math.sqrt", () => {
    module Test = {
      Math.sqrt(-1.0)->Float.isNaN // true
      Math.sqrt(-0.0) // -0.0
      Math.sqrt(0.0) // 0.0
      Math.sqrt(1.0) // 1.0
      Math.sqrt(9.0) // 3.0
    }
    ()
  })
})

describe("Math.sinh", () => {
  test("Math.sinh", () => {
    module Test = {
      Math.sinh(-0.0) // -0.0
      Math.sinh(0.0) // 0.0
      Math.sinh(1.0) // 1.1752011936438014
    }
    ()
  })
})

describe("Math.sin", () => {
  test("Math.sin", () => {
    module Test = {
      Math.sin(-0.0) // -0.0
      Math.sin(0.0) // 0.0
      Math.sin(1.0) // 0.8414709848078965
    }
    ()
  })
})

describe("Math.sign", () => {
  test("Math.sign", () => {
    module Test = {
      Math.sign(3.0) // 1.0
      Math.sign(-3.0) // 1.0
      Math.sign(0.0) // 0.0
    }
    ()
  })
})

describe("Math.round", () => {
  test("Math.round", () => {
    module Test = {
      Math.round(-20.5) // -20.0
      Math.round(-0.1) // -0.0
      Math.round(0.0) // 0.0
      Math.round(-0.0) // -0.0
    }
    ()
  })
})

describe("Math.random", () => {
  test("Math.random", () => {
    module Test = {
      Math.random()
    }
    ()
  })
})

describe("Math.pow", () => {
  test("Math.pow", () => {
    module Test = {
      Math.pow(2.0, ~exp=4.0) // 16.0
      Math.pow(3.0, ~exp=4.0) // 81.0
    }
    ()
  })
})

describe("Math.maxMany", () => {
  test("Math.maxMany", () => {
    module Test = {
      Math.maxMany([1.0, 2.0]) // 2.0
      Math.maxMany([-1.0, -2.0]) // -1.0
      Math.maxMany([])->Float.isFinite // false
    }
    ()
  })
})

describe("Math.max", () => {
  test("Math.max", () => {
    module Test = {
      Math.max(1.0, 2.0) // 2.0
      Math.max(-1.0, -2.0) // -1.0
    }
    ()
  })
})

describe("Math.minMany", () => {
  test("Math.minMany", () => {
    module Test = {
      Math.minMany([1.0, 2.0]) // 1.0
      Math.minMany([-1.0, -2.0]) // -2.0
      Math.minMany([])->Float.isFinite // false
    }
    ()
  })
})

describe("Math.min", () => {
  test("Math.min", () => {
    module Test = {
      Math.min(1.0, 2.0) // 1.0
      Math.min(-1.0, -2.0) // -2.0
    }
    ()
  })
})

describe("Math.log2", () => {
  test("Math.log2", () => {
    module Test = {
      Math.log2(-2.0)->Float.isNaN // true
      Math.log2(-0.0)->Float.isFinite // false
      Math.log2(0.0)->Float.isFinite // false
      Math.log2(1.0) // 0.0
    }
    ()
  })
})

describe("Math.log10", () => {
  test("Math.log10", () => {
    module Test = {
      Math.log10(-2.0)->Float.isNaN // true
      Math.log10(-0.0)->Float.isFinite // false
      Math.log10(0.0)->Float.isFinite // false
      Math.log10(1.0) // 0
    }
    ()
  })
})

describe("Math.log1p", () => {
  test("Math.log1p", () => {
    module Test = {
      Math.log1p(-2.0)->Float.isNaN // true
      Math.log1p(-1.0)->Float.isFinite // false
      Math.log1p(-0.0) // -0
    }
    ()
  })
})

describe("Math.log", () => {
  test("Math.log", () => {
    module Test = {
      Math.log(-1.0)->Float.isNaN // true
      Math.log(-0.0)->Float.isFinite // false
      Math.log(0.0)->Float.isFinite // false
      Math.log(1.0) // 0
    }
    ()
  })
})

describe("Math.hypotMany", () => {
  test("Math.hypotMany", () => {
    module Test = {
      Math.hypotMany([3.0, 4.0, 5.0]) // 7.0710678118654755
      Math.hypotMany([]) // 0.0
    }
    ()
  })
})

describe("Math.hypot", () => {
  test("Math.hypot", () => {
    module Test = {
      Math.hypot(3.0, 4.0) // 5.0
      Math.hypot(3.0, 5.0) // 5.8309518948453
    }
    ()
  })
})

describe("Math.fround", () => {
  test("Math.fround", () => {
    module Test = {
      Math.fround(5.5) == 5.5
      Math.fround(5.05) == 5.050000190734863
    }
    ()
  })
})

describe("Math.floor", () => {
  test("Math.floor", () => {
    module Test = {
      Math.floor(-45.95) // -46.0
      Math.floor(-45.05) // -46.0
      Math.floor(-0.0) // -0.0
    }
    ()
  })
})

describe("Math.expm1", () => {
  test("Math.expm1", () => {
    module Test = {
      Math.expm1(-1.0) // -0.6321205588285577
      Math.expm1(-0.0) // -0
    }
    ()
  })
})

describe("Math.exp", () => {
  test("Math.exp", () => {
    module Test = {
      Math.exp(-1.0) // 0.36787944117144233
      Math.exp(0.0) // 1.0
    }
    ()
  })
})

describe("Math.cosh", () => {
  test("Math.cosh", () => {
    module Test = {
      Math.cosh(-1.0) // 1.5430806348152437
      Math.cosh(-0.0) // 1.0
      Math.cosh(0.0) // 1.0
    }
    ()
  })
})

describe("Math.cos", () => {
  test("Math.cos", () => {
    module Test = {
      Math.cos(-0.0) // 1.0
      Math.cos(0.0) // 1.0
      Math.cos(1.0) // 0.5403023058681398
    }
    ()
  })
})

describe("Math.ceil", () => {
  test("Math.ceil", () => {
    module Test = {
      Math.ceil(3.1) == 4.0
      Math.ceil(3.0) == 3.0
      Math.ceil(-3.1) == -3.0
      Math.ceil(2_150_000_000.3) == 2_150_000_001.0
    }
    ()
  })
})

describe("Math.cbrt", () => {
  test("Math.cbrt", () => {
    module Test = {
      Math.cbrt(-1.0) // -1.0
      Math.cbrt(-0.0) // -0.0
      Math.cbrt(0.0) // 0.0
    }
    ()
  })
})

describe("Math.atan2", () => {
  test("Math.atan2", () => {
    module Test = {
      Math.atan2(~y=0.0, ~x=10.0) == 0.0
      Math.atan2(~x=5.0, ~y=5.0) == Math.Constants.pi /. 4.0
      Math.atan2(~x=90.0, ~y=15.0) // 1.4056476493802699
      Math.atan2(~x=15.0, ~y=90.0) // 0.16514867741462683
    }
    ()
  })
})

describe("Math.atanh", () => {
  test("Math.atanh", () => {
    module Test = {
      Math.atanh(-2.0)->Float.isNaN // true
      Math.atanh(-1.0)->Float.isFinite // false
      Math.atanh(-0.0) // -0.0
      Math.atanh(0.0) // 0.0
      Math.atanh(0.5) // 0.5493061443340548
    }
    ()
  })
})

describe("Math.atan", () => {
  test("Math.atan", () => {
    module Test = {
      Math.atan(-0.0) // -0.0
      Math.atan(0.0) // 0.0
      Math.atan(1.0) // 0.7853981633974483
    }
    ()
  })
})

describe("Math.asinh", () => {
  test("Math.asinh", () => {
    module Test = {
      Math.asinh(-1.0) // -0.881373587019543
      Math.asinh(-0.0) // -0.0
    }
    ()
  })
})

describe("Math.asin", () => {
  test("Math.asin", () => {
    module Test = {
      Math.asin(-1.0) // -1.5707963267948966
      Math.asin(-2.0)->Float.isNaN // true
    }
    ()
  })
})

describe("Math.acosh", () => {
  test("Math.acosh", () => {
    module Test = {
      Math.acosh(1.0) // 0.0
      Math.acosh(0.5)->Float.isNaN // true
    }
    ()
  })
})

describe("Math.acos", () => {
  test("Math.acos", () => {
    module Test = {
      Math.acos(-1.0) // 3.141592653589793
      Math.acos(-3.0)->Float.isNaN // true
    }
    ()
  })
})

describe("Math.abs", () => {
  test("Math.abs", () => {
    module Test = {
      Math.abs(-2.0) // 2.0
      Math.abs(3.0) // 3.0
    }
    ()
  })
})

describe("Nullable.flatMap", () => {
  test("Nullable.flatMap", () => {
    module Test = {
      let addIfAboveOne = value =>
        if value > 1 {
          Nullable.make(value + 1)
        } else {
          Nullable.null
        }

      Nullable.flatMap(Nullable.make(2), addIfAboveOne) // Nullable.make(3)
      Nullable.flatMap(Nullable.make(-4), addIfAboveOne) // undefined
      Nullable.flatMap(Nullable.null, addIfAboveOne) // undefined
    }
    ()
  })
})

describe("Nullable.mapOr", () => {
  test("Nullable.mapOr", () => {
    module Test = {
      let someValue = Nullable.make(3)
      someValue->Nullable.mapOr(0, x => x + 5) // 8

      let noneValue = Nullable.null
      noneValue->Nullable.mapOr(0, x => x + 5) // 0
    }
    ()
  })
})

describe("Nullable.map", () => {
  test("Nullable.map", () => {
    module Test = {
      Nullable.map(Nullable.make(3), x => x * x) // Nullable.make(9)
      Nullable.map(undefined, x => x * x) // undefined
    }
    ()
  })
})

describe("Nullable.forEach", () => {
  test("Nullable.forEach", () => {
    module Test = {
      Nullable.forEach(Nullable.make("thing"), x => Console.log(x)) // logs "thing"
      Nullable.forEach(Nullable.null, x => Console.log(x)) // returns ()
      Nullable.forEach(undefined, x => Console.log(x)) // returns ()
    }
    ()
  })
})

describe("Nullable.getUnsafe", () => {
  test("Nullable.getUnsafe", () => {
    module Test = {
      Nullable.getUnsafe(Nullable.make(3)) == 3
      Nullable.getUnsafe(Nullable.null) // Raises an error
    }
    ()
  })
})

describe("Nullable.getExn", () => {
  test("Nullable.getExn", () => {
    module Test = {
      switch Nullable.getExn(%raw("'Hello'")) {
      | exception Invalid_argument(_) => assert(false)
      | value => assertEqual(value, "Hello")
      }

      switch Nullable.getExn(%raw("null")) {
      | exception Invalid_argument(_) => assert(true)
      | _ => assert(false)
      }

      switch Nullable.getExn(%raw("undefined")) {
      | exception Invalid_argument(_) => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Nullable.getOr", () => {
  test("Nullable.getOr", () => {
    module Test = {
      Nullable.getOr(Nullable.null, "Banana") // Banana
      Nullable.getOr(Nullable.make("Apple"), "Banana") // Apple

      let greet = (firstName: option<string>) =>
        "Greetings " ++ firstName->Option.getOr("Anonymous")

      Nullable.make("Jane")->Nullable.toOption->greet // "Greetings Jane"
      Nullable.null->Nullable.toOption->greet // "Greetings Anonymous"
    }
    ()
  })
})

describe("Nullable.fromOption", () => {
  test("Nullable.fromOption", () => {
    module Test = {
      let optString = Some("Hello")
      let asNullable = optString->Nullable.fromOption // Nullable.t<string>
    }
    ()
  })
})

describe("Nullable.toOption", () => {
  test("Nullable.toOption", () => {
    module Test = {
      let nullableString = Nullable.make("Hello")

      switch nullableString->Nullable.toOption {
      | Some(str) => Console.log2("Got string:", str)
      | None => Console.log("Didn't have a value.")
      }
    }
    ()
  })
})

describe("Nullable.make", () => {
  test("Nullable.make", () => {
    module Test = {
      let myStr = "Hello"
      let asNullable = myStr->Nullable.make

      // Can't do the below because we're now forced to check for nullability
      // myStr == asNullable

      // Need to do this
      switch asNullable->Nullable.toOption {
      | Some(value) if value == myStr => Console.log("Yay, values matched!")
      | _ => Console.log("Values did not match.")
      }
    }
    ()
  })
})

describe("Nullable.isNullable", () => {
  test("Nullable.isNullable", () => {
    module Test = {
      let myStr = "Hello"
      let asNullable = myStr->Nullable.make

      // Can't do the below because we're now forced to check for nullability
      // myStr == asNullable

      // Check if asNullable is not null or undefined
      switch asNullable->Nullable.isNullable {
      | true => assert(false)
      | false => assert(true)
      }
    }
    ()
  })
})

describe("Nullable.undefined", () => {
  test("Nullable.undefined", () => {
    module Test = {
      Console.log(undefined) // Logs `undefined` to the console.
    }
    ()
  })
})

describe("Nullable.null", () => {
  test("Nullable.null", () => {
    module Test = {
      Console.log(Nullable.null) // Logs `null` to the console.
    }
    ()
  })
})

describe("Object.isExtensible", () => {
  test("Object.isExtensible", () => {
    module Test = {
      let obj = {"a": 1}
      obj->Object.isExtensible // true
      obj->Object.preventExtensions->ignore
      obj->Object.isExtensible // false
    }
    ()
  })
})

describe("Object.isFrozen", () => {
  test("Object.isFrozen", () => {
    module Test = {
      let point = {"x": 1, "y": 3}->Object.freeze
      let pointIsFrozen = point->Object.isFrozen // true
      let fruit = {"name": "Apple"}
      let fruitIsFrozen = fruit->Object.isFrozen // false
    }
    ()
  })
})

describe("Object.isSealed", () => {
  test("Object.isSealed", () => {
    module Test = {
      let point = {"x": 1, "y": 3}->Object.seal
      let pointIsSealed = point->Object.isSealed // true
      let fruit = {"name": "Apple"}
      let fruitIsSealed = fruit->Object.isSealed // false
    }
    ()
  })
})

describe("Object.freeze", () => {
  test("Object.freeze", () => {
    module Test = {
      let obj = {"a": 1}
      obj->Object.set("a", 2) // succeeds
      obj->Object.freeze->ignore

      try {
        obj->Object.set("a", 3) // fails
      } catch {
      | Exn.Error(_) => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Object.preventExtensions", () => {
  test("Object.preventExtensions", () => {
    module Test = {
      let obj = {"a": 1}
      obj->Object.set("b", 2) // succeeds
      obj->Object.preventExtensions->ignore
      try {
        obj->Object.set("c", 3) // fails
      } catch {
      | Exn.Error(_) => assert(true)
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Object.seal", () => {
  test("Object.seal", () => {
    module Test = {
      let point = {"x": 1, "y": 2}
      point->Object.set("x", -7) // succeeds
      point->Object.seal->ignore

      try {
        point->Object.set("z", 9) // fails
      } catch {
      | Exn.Error(_) => assert(true)
      | _ => assert(false)
      }

      point->Object.set("x", 13) // succeeds
    }
    ()
  })
})

describe("Object.hasOwnProperty", () => {
  test("Object.hasOwnProperty", () => {
    module Test = {
      let point = {"x": 1, "y": 2}
      {"a": 1}->Object.hasOwnProperty("a") // true
      {"a": 1}->Object.hasOwnProperty("b") // false
      {"a": 1}->Object.hasOwnProperty("toString") // false
    }
    ()
  })
})

describe("Object.keysToArray", () => {
  test("Object.keysToArray", () => {
    module Test = {
      {"a": 1, "b": 2}->Object.keysToArray // ["a", "b"]
      {"a": None}->Object.keysToArray // ["a"]
      Object.make()->Object.keysToArray // []
    }
    ()
  })
})

describe("Object.set", () => {
  test("Object.set", () => {
    module Test = {
      {"a": 1}->Object.set("a", 2) // {"a": 2}
      {"a": 1}->Object.set("a", None) // {"a": None}
      {"a": 1}->Object.set("b", 2) // {"a": 1, "b": 2}
    }
    ()
  })
})

describe("Object.getSymbol", () => {
  test("Object.getSymbol", () => {
    module Test = {
      let fruit = Symbol.make("fruit")
      let x = Object.make()
      x->Object.setSymbol(fruit, "banana")
      x->Object.getSymbol(fruit) // Some("banana")
    }
    ()
  })
})

describe("Object.get", () => {
  test("Object.get", () => {
    module Test = {
      {"a": 1}->Object.get("a") // Some(1)
      {"a": 1}->Object.get("b") // None
      {"a": undefined}->Object.get("a") // None
      {"a": null}->Object.get("a") // Some(null)
      {"a": 1}->Object.get("toString")->Option.isSome // true
    }
    ()
  })
})

describe("Object.assign", () => {
  test("Object.assign", () => {
    module Test = {
      Object.assign({"a": 1}, {"a": 2}) // {"a": 2}
      Object.assign({"a": 1, "b": 2}, {"a": 0}) // {"a": 0, "b": 2}
      Object.assign({"a": 1}, {"a": null}) // {"a": null}
    }
    ()
  })
})

describe("Object.create", () => {
  test("Object.create", () => {
    module Test = {
      let x = {"fruit": "banana"}
      let y = Object.create(x)
      y->Object.get("fruit") // Some("banana")
    }
    ()
  })
})

describe("Object.is", () => {
  test("Object.is", () => {
    module Test = {
      Object.is(25, 13) // false
      Object.is("abc", "abc") // true
      Object.is(undefined, undefined) // true
      Object.is(undefined, null) // false
      Object.is(-0.0, 0.0) // false
      Object.is(list{1, 2}, list{1, 2}) // false

      Object.is([1, 2, 3], [1, 2, 3]) // false
      [1, 2, 3] == [1, 2, 3] // true
      [1, 2, 3] === [1, 2, 3] // false

      let fruit = {"name": "Apple"}
      Object.is(fruit, fruit) // true
      Object.is(fruit, {"name": "Apple"}) // false
      fruit == {"name": "Apple"} // true
      fruit === {"name": "Apple"} // false
    }
    ()
  })
})

describe("Object.make", () => {
  test("Object.make", () => {
    module Test = {
      let x = Object.make()
      x->Object.keysToArray->Array.length // 0
      x->Object.get("toString")->Option.isSome // true
    }
    ()
  })
})

describe("Pervasives.assertEqual", () => {
  test("Pervasives.assertEqual", () => {
    module Test = {
      list{1, 2}
      ->List.tailExn
      ->assertEqual(list{2})
    }
    ()
  })
})

describe("Pervasives.import", () => {
  test("Pervasives.import", () => {
    module Test = {
      @send external indexOf: (array<'a>, 'a) => int = "indexOf"

      let indexOfOpt = (arr, item) =>
        switch arr->indexOf(item) {
        | -1 => None
        | index => Some(index)
        }

      let main = async () => {
        let indexOfOpt = await import(Array.indexOfOpt)
        let index = indexOfOpt([1, 2], 2)
        Console.log(index)
      }
    }
    ()
  })
})

describe("Pervasives.decodeURIComponent", () => {
  test("Pervasives.decodeURIComponent", () => {
    module Test = {
      Console.log(decodeURIComponent("array%3D%5BsomeValue%5D"))
      // Logs "array=[someValue]" to the console.
    }
    ()
  })
})

describe("Pervasives.encodeURIComponent", () => {
  test("Pervasives.encodeURIComponent", () => {
    module Test = {
      Console.log(encodeURIComponent("array=[someValue]"))
      // Logs "array%3D%5BsomeValue%5D" to the console.
    }
    ()
  })
})

describe("Pervasives.decodeURI", () => {
  test("Pervasives.decodeURI", () => {
    module Test = {
      Console.log(decodeURI("https://rescript-lang.org?array=%5BsomeValue%5D"))
      // Logs "https://rescript-lang.org?array=[someValue]" to the console.
    }
    ()
  })
})

describe("Pervasives.encodeURI", () => {
  test("Pervasives.encodeURI", () => {
    module Test = {
      Console.log(encodeURI("https://rescript-lang.org?array=[someValue]"))
      // Logs "https://rescript-lang.org?array=%5BsomeValue%5D" to the console.
    }
    ()
  })
})

describe("Pervasives.clearInterval", () => {
  test("Pervasives.clearInterval", () => {
    module Test = {
      let intervalId = setInterval(
        () => {
          Console.log("This prints in 100 ms")
        },
        100,
      )

      // Stop the interval after 500 ms
      let timeoutId = setTimeout(
        () => {
          clearInterval(intervalId)
        },
        500,
      )
    }
    ()
  })
})

describe("Pervasives.setIntervalFloat", () => {
  test("Pervasives.setIntervalFloat", () => {
    module Test = {
      // Log to the console ever 2 seconds (200 milliseconds).
      let intervalId = setIntervalFloat(
        () => {
          Console.log("This prints every 200 ms")
        },
        200.,
      )

      // Stop the interval after 500 ms
      let timeoutId = setTimeoutFloat(
        () => {
          clearInterval(intervalId)
        },
        500.0,
      )
    }
    ()
  })
})

describe("Pervasives.setInterval", () => {
  test("Pervasives.setInterval", () => {
    module Test = {
      // Log to the console ever 200 ms (200 milliseconds).
      let intervalId = setInterval(
        () => {
          Console.log("This prints every 200 ms.")
        },
        200,
      )

      let timeoutId = setTimeout(
        () => {
          clearInterval(intervalId)
        },
        500,
      )
    }
    ()
  })
})

describe("Pervasives.clearTimeout", () => {
  test("Pervasives.clearTimeout", () => {
    module Test = {
      let timeoutId = setTimeout(
        () => {
          Console.log("This prints in 2 seconds.")
        },
        2000,
      )

      // Clearing the timeout right away, before 2 seconds has passed, means that the above callback logging to the console will never run.
      clearTimeout(timeoutId)
    }
    ()
  })
})

describe("Pervasives.setTimeoutFloat", () => {
  test("Pervasives.setTimeoutFloat", () => {
    module Test = {
      // Log to the console after 200 milliseconds.
      let timeoutId = setTimeoutFloat(
        () => {
          Console.log("This prints in 200 ms.")
        },
        200.,
      )
    }
    ()
  })
})

describe("Pervasives.setTimeout", () => {
  test("Pervasives.setTimeout", () => {
    module Test = {
      // Log to the console after 200 milliseconds.
      let timeoutId = setTimeout(
        () => {
          Console.log("This prints in 200 ms.")
        },
        200,
      )
    }
    ()
  })
})

describe("Option.all", () => {
  test("Option.all", () => {
    module Test = {
      Option.all([Some(1), Some(2), Some(3)]) // Some([1, 2, 3])
      Option.all([Some(1), None]) // None
    }
    ()
  })
})

describe("Option.compare", () => {
  test("Option.compare", () => {
    module Test = {
      let clockCompare = (a, b) => Int.compare(mod(a, 12), mod(b, 12))

      Option.compare(Some(3), Some(15), clockCompare) // 0.
      Option.compare(Some(3), Some(14), clockCompare) // 1.
      Option.compare(Some(2), Some(15), clockCompare) // (-1.)
      Option.compare(None, Some(15), clockCompare) // (-1.)
      Option.compare(Some(14), None, clockCompare) // 1.
      Option.compare(None, None, clockCompare) // 0.
    }
    ()
  })
})

describe("Option.equal", () => {
  test("Option.equal", () => {
    module Test = {
      let clockEqual = (a, b) => mod(a, 12) == mod(b, 12)

      open Option

      equal(Some(3), Some(15), clockEqual) // true
      equal(Some(3), None, clockEqual) // false
      equal(None, Some(3), clockEqual) // false
      equal(None, None, clockEqual) // true
    }
    ()
  })
})

describe("Option.isNone", () => {
  test("Option.isNone", () => {
    module Test = {
      Option.isNone(None) // true
      Option.isNone(Some(1)) // false
    }
    ()
  })
})

describe("Option.isSome", () => {
  test("Option.isSome", () => {
    module Test = {
      Option.isSome(None) // false
      Option.isSome(Some(1)) // true
    }
    ()
  })
})

describe("Option.orElse", () => {
  test("Option.orElse", () => {
    module Test = {
      Option.orElse(Some(1812), Some(1066)) == Some(1812)
      Option.orElse(None, Some(1066)) == Some(1066)
      Option.orElse(None, None) == None
    }
    ()
  })
})

describe("Option.getOr", () => {
  test("Option.getOr", () => {
    module Test = {
      Option.getOr(None, "Banana") // Banana
      Option.getOr(Some("Apple"), "Banana") // Apple

      let greet = (firstName: option<string>) =>
        "Greetings " ++ firstName->Option.getOr("Anonymous")

      Some("Jane")->greet // "Greetings Jane"
      None->greet // "Greetings Anonymous"
    }
    ()
  })
})

describe("Option.flatMap", () => {
  test("Option.flatMap", () => {
    module Test = {
      let addIfAboveOne = value =>
        if value > 1 {
          Some(value + 1)
        } else {
          None
        }

      Option.flatMap(Some(2), addIfAboveOne) // Some(3)
      Option.flatMap(Some(-4), addIfAboveOne) // None
      Option.flatMap(None, addIfAboveOne) // None
    }
    ()
  })
})

describe("Option.map", () => {
  test("Option.map", () => {
    module Test = {
      Option.map(Some(3), x => x * x) // Some(9)
      Option.map(None, x => x * x) // None
    }
    ()
  })
})

describe("Option.mapOr", () => {
  test("Option.mapOr", () => {
    module Test = {
      let someValue = Some(3)
      someValue->Option.mapOr(0, x => x + 5) // 8

      let noneValue = None
      noneValue->Option.mapOr(0, x => x + 5) // 0
    }
    ()
  })
})

describe("Option.getUnsafe", () => {
  test("Option.getUnsafe", () => {
    module Test = {
      Option.getUnsafe(Some(3)) == 3
      Option.getUnsafe((None: option<int>)) // Returns `undefined`, which is not a valid `int`
    }
    ()
  })
})

describe("Option.getExn", () => {
  test("Option.getExn", () => {
    module Test = {
      Option.getExn(Some(3))->assertEqual(3)

      switch Option.getExn(None) {
      | exception _ => assert(true)
      | _ => assert(false)
      }

      switch Option.getExn(None, ~message="was None!") {
      | exception _ => assert(true) // Raises an Error with the message "was None!"
      | _ => assert(false)
      }
    }
    ()
  })
})

describe("Option.forEach", () => {
  test("Option.forEach", () => {
    module Test = {
      Option.forEach(Some("thing"), x => Console.log(x)) // logs "thing"
      Option.forEach(None, x => Console.log(x)) // returns ()
    }
    ()
  })
})

describe("Option.filter", () => {
  test("Option.filter", () => {
    module Test = {
      Option.filter(Some(10), x => x > 5) // Some(10)
      Option.filter(Some(4), x => x > 5) // None
      Option.filter(None, x => x > 5) // None
    }
    ()
  })
})

describe("RegExp.Result.input", () => {
  test("RegExp.Result.input", () => {
    module Test = {
      // Match the first two words separated by a space
      let regexp = RegExp.fromString("(\\w+) (\\w+)")

      // This below will log the full input string "ReScript is pretty cool, right?" to the console.
      switch regexp->RegExp.exec("ReScript is pretty cool, right?") {
      | None => Console.log("Nope, no match...")
      | Some(result) => Console.log(result->RegExp.Result.input)
      }
    }
    ()
  })
})

describe("RegExp.Result.matches", () => {
  test("RegExp.Result.matches", () => {
    module Test = {
      // Match the first two words separated by a space
      let regexp = RegExp.fromString("(\\w+) (\\w+)")

      // This below will log "ReScript" and "is" to the console.
      switch regexp->RegExp.exec("ReScript is pretty cool, right?") {
      | None => Console.log("Nope, no match...")
      | Some(result) =>
        switch result->RegExp.Result.matches {
        | [firstWord, secondWord] => Console.log2(firstWord, secondWord)
        | _ => Console.log("Didn't find exactly two words...")
        }
      }
    }
    ()
  })
})

describe("RegExp.Result.fullMatch", () => {
  test("RegExp.Result.fullMatch", () => {
    module Test = {
      // Match the first two words separated by a space
      let regexp = RegExp.fromString("(\\w+) (\\w+)")

      switch regexp->RegExp.exec("ReScript is pretty cool, right?") {
      | None => Console.log("Nope, no match...")
      | Some(result) => Console.log(result->RegExp.Result.fullMatch) // Prints the full string that matched, "ReScript is"
      }
    }
    ()
  })
})

describe("RegExp.unicode", () => {
  test("RegExp.unicode", () => {
    module Test = {
      let regexp1 = RegExp.fromStringWithFlags("\\w+", ~flags="g")
      Console.log(regexp1->RegExp.unicode) // Logs `false`, since `u` is not set

      let regexp2 = RegExp.fromStringWithFlags("\\w+", ~flags="mu")
      Console.log(regexp2->RegExp.unicode) // Logs `true`, since `u` is set
    }
    ()
  })
})

describe("RegExp.sticky", () => {
  test("RegExp.sticky", () => {
    module Test = {
      let regexp1 = RegExp.fromStringWithFlags("\\w+", ~flags="g")
      Console.log(regexp1->RegExp.unicode) // Logs `false`, since `y` is not set

      let regexp2 = RegExp.fromStringWithFlags("\\w+", ~flags="my")
      Console.log(regexp2->RegExp.unicode) // Logs `true`, since `y` is set
    }
    ()
  })
})

describe("RegExp.source", () => {
  test("RegExp.source", () => {
    module Test = {
      let regexp = RegExp.fromStringWithFlags("\\w+", ~flags="g")
      Console.log(regexp->RegExp.source) // Logs `\w+`, the source text of the `RegExp`
    }
    ()
  })
})

describe("RegExp.multiline", () => {
  test("RegExp.multiline", () => {
    module Test = {
      let regexp1 = RegExp.fromStringWithFlags("\\w+", ~flags="g")
      Console.log(regexp1->RegExp.multiline) // Logs `false`, since `m` is not set

      let regexp2 = RegExp.fromStringWithFlags("\\w+", ~flags="mi")
      Console.log(regexp2->RegExp.multiline) // Logs `true`, since `m` is set
    }
    ()
  })
})

describe("RegExp.global", () => {
  test("RegExp.global", () => {
    module Test = {
      let regexp1 = RegExp.fromStringWithFlags("\\w+", ~flags="g")
      Console.log(regexp1->RegExp.global) // Logs `true`, since `g` is set

      let regexp2 = RegExp.fromStringWithFlags("\\w+", ~flags="i")
      Console.log(regexp2->RegExp.global) // Logs `false`, since `g` is not set
    }
    ()
  })
})

describe("RegExp.ignoreCase", () => {
  test("RegExp.ignoreCase", () => {
    module Test = {
      let regexp1 = RegExp.fromStringWithFlags("\\w+", ~flags="g")
      Console.log(regexp1->RegExp.ignoreCase) // Logs `false`, since `i` is not set

      let regexp2 = RegExp.fromStringWithFlags("\\w+", ~flags="i")
      Console.log(regexp2->RegExp.ignoreCase) // Logs `true`, since `i` is set
    }
    ()
  })
})

describe("RegExp.setLastIndex", () => {
  test("RegExp.setLastIndex", () => {
    module Test = {
      // Match the first word in a sentence
      let regexp = RegExp.fromString("\\w+")
      let someStr = "Many words here."

      regexp->RegExp.setLastIndex(4)
      regexp->RegExp.exec(someStr)->ignore

      Console.log(regexp->RegExp.lastIndex) // Logs `10` to the console
    }
    ()
  })
})

describe("RegExp.lastIndex", () => {
  test("RegExp.lastIndex", () => {
    module Test = {
      // Match the first word in a sentence
      let regexp = RegExp.fromString("\\w+")
      let someStr = "Many words here."

      Console.log(regexp->RegExp.lastIndex) // Logs `0` to the console

      regexp->RegExp.exec(someStr)->ignore

      Console.log(regexp->RegExp.lastIndex) // Logs `4` to the console
    }
    ()
  })
})

describe("RegExp.exec", () => {
  test("RegExp.exec", () => {
    module Test = {
      // Match the first word in a sentence
      let regexp = RegExp.fromString("\\w+")

      switch regexp->RegExp.exec("ReScript is pretty cool, right?") {
      | None => Console.log("Nope, no match...")
      | Some(result) => Console.log(result->RegExp.Result.fullMatch) // Prints "ReScript"
      }
    }
    ()
  })
})

describe("RegExp.test", () => {
  test("RegExp.test", () => {
    module Test = {
      // Match the first word in a sentence
      let regexp = RegExp.fromString("\\w+")

      if regexp->RegExp.test("ReScript is cool!") {
        Console.log("Yay, there's a word in there.")
      }
    }
    ()
  })
})

describe("RegExp.fromStringWithFlags", () => {
  test("RegExp.fromStringWithFlags", () => {
    module Test = {
      // Match the first word in a sentence
      let regexp = RegExp.fromStringWithFlags("\\w+", ~flags="g")

      switch regexp->RegExp.exec("ReScript is pretty cool, right?") {
      | None => Console.log("Nope, no match...")
      | Some(result) => Console.log(result->RegExp.Result.fullMatch) // Prints "ReScript"
      }
    }
    ()
  })
})

describe("RegExp.fromString", () => {
  test("RegExp.fromString", () => {
    module Test = {
      // Match the first word in a sentence
      let regexp = RegExp.fromString("\\w+")

      switch regexp->RegExp.exec("ReScript is pretty cool, right?") {
      | None => Console.log("Nope, no match...")
      | Some(result) => Console.log(result->RegExp.Result.fullMatch) // Prints "ReScript"
      }
    }
    ()
  })
})

describe("Result.all", () => {
  test("Result.all", () => {
    module Test = {
      Result.all([Ok(1), Ok(2), Ok(3)]) // Ok([1, 2, 3])
      Result.all([Ok(1), Error(1)]) // Error(1)
    }
    ()
  })
})

describe("Result.mapError", () => {
  test("Result.mapError", () => {
    module Test = {
      let format = n => `Error code: ${n->Int.toString}`
      Result.mapError(Error(14), format) // Error("Error code: 14")
      Result.mapError(Ok("abc"), format) // Ok("abc")
    }
    ()
  })
})

describe("Result.forEach", () => {
  test("Result.forEach", () => {
    module Test = {
      Result.forEach(Ok(3), Console.log) // Logs "3", returns ()
      Result.forEach(Error("x"), Console.log) // Does nothing, returns ()
    }
    ()
  })
})

describe("Result.compare", () => {
  test("Result.compare", () => {
    module Test = {
      let good1 = Ok(59)

      let good2 = Ok(37)

      let bad1 = Error("invalid")

      let bad2 = Error("really invalid")

      let mod10cmp = (a, b) => Int.compare(mod(a, 10), mod(b, 10))

      Result.compare(Ok(39), Ok(57), mod10cmp) == 1.

      Result.compare(Ok(57), Ok(39), mod10cmp) == -1.

      Result.compare(Ok(39), Error("y"), mod10cmp) == 1.

      Result.compare(Error("x"), Ok(57), mod10cmp) == -1.

      Result.compare(Error("x"), Error("y"), mod10cmp) == 0.
    }
    ()
  })
})

describe("Result.equal", () => {
  test("Result.equal", () => {
    module Test = {
      let good1 = Ok(42)

      let good2 = Ok(32)

      let bad1 = Error("invalid")

      let bad2 = Error("really invalid")

      let mod10equal = (a, b) => mod(a, 10) === mod(b, 10)

      Result.equal(good1, good2, mod10equal) == true

      Result.equal(good1, bad1, mod10equal) == false

      Result.equal(bad2, good2, mod10equal) == false

      Result.equal(bad1, bad2, mod10equal) == true
    }
    ()
  })
})

describe("Result.getOr", () => {
  test("Result.getOr", () => {
    module Test = {
      Result.getOr(Ok(42), 0) == 42

      Result.getOr(Error("Invalid Data"), 0) == 0
    }
    ()
  })
})

describe("Result.flatMap", () => {
  test("Result.flatMap", () => {
    module Test = {
      let recip = x =>
        if x !== 0.0 {
          Ok(1.0 /. x)
        } else {
          Error("Divide by zero")
        }

      Result.flatMap(Ok(2.0), recip) == Ok(0.5)

      Result.flatMap(Ok(0.0), recip) == Error("Divide by zero")

      Result.flatMap(Error("Already bad"), recip) == Error("Already bad")
    }
    ()
  })
})

describe("Result.map", () => {
  test("Result.map", () => {
    module Test = {
      let f = x => sqrt(Int.toFloat(x))

      Result.map(Ok(64), f) == Ok(8.0)

      Result.map(Error("Invalid data"), f) == Error("Invalid data")
    }
    ()
  })
})

describe("Result.mapOr", () => {
  test("Result.mapOr", () => {
    module Test = {
      let ok = Ok(42)
      Result.mapOr(ok, 0, x => x / 2) == 21

      let error = Error("Invalid data")
      Result.mapOr(error, 0, x => x / 2) == 0
    }
    ()
  })
})

describe("Promise.allSettled", () => {
  test("Promise.allSettled", () => {
    module Test = {
      open Promise

      exception TestError(string)

      let promises = [resolve(1), resolve(2), reject(TestError("some rejected promise"))]

      allSettled(promises)
      ->then(
        results => {
          results->Array.forEach(
            result => {
              switch result {
              | Fulfilled({value: num}) => Console.log2("Number: ", num)
              | Rejected({reason}) => Console.log(reason)
              }
            },
          )

          resolve()
        },
      )
      ->ignore
    }
    ()
  })
})

describe("Promise.all", () => {
  test("Promise.all", () => {
    module Test = {
      open Promise
      let promises = [resolve(1), resolve(2), resolve(3)]

      all(promises)
      ->then(
        results => {
          results->Array.forEach(
            num => {
              Console.log2("Number: ", num)
            },
          )

          resolve()
        },
      )
      ->ignore
    }
    ()
  })
})

describe("Promise.any", () => {
  test("Promise.any", () => {
    module Test = {
      open Promise
      let racer = (ms, name) => {
        Promise.make(
          (resolve, _) => {
            setTimeout(
              () => {
                resolve(name)
              },
              ms,
            )->ignore
          },
        )
      }

      let promises = [racer(1000, "Turtle"), racer(500, "Hare"), racer(100, "Eagle")]

      any(promises)->then(
        winner => {
          Console.log("The winner is " ++ winner)
          resolve()
        },
      )
    }
    ()
  })
})

describe("Promise.race", () => {
  test("Promise.race", () => {
    module Test = {
      open Promise
      let racer = (ms, name) => {
        Promise.make(
          (resolve, _) => {
            setTimeout(
              () => {
                resolve(name)
              },
              ms,
            )->ignore
          },
        )
      }

      let promises = [racer(1000, "Turtle"), racer(500, "Hare"), racer(100, "Eagle")]

      race(promises)->then(
        winner => {
          Console.log("The winner is " ++ winner)
          resolve()
        },
      )
    }
    ()
  })
})

describe("Promise.finally", () => {
  test("Promise.finally", () => {
    module Test = {
      open Promise
      exception SomeError(string)
      let isDone = ref(false)

      resolve(5)
      ->then(
        _ => {
          reject(SomeError("test"))
        },
      )
      ->then(
        v => {
          Console.log2("final result", v)
          resolve()
        },
      )
      ->catch(
        _ => {
          Console.log("Error handled")
          resolve()
        },
      )
      ->finally(
        () => {
          Console.log("finally")
          isDone := true
        },
      )
      ->then(
        () => {
          Console.log2("isDone:", isDone.contents)
          resolve()
        },
      )
      ->ignore
    }
    ()
  })
})

describe("Promise.thenResolve", () => {
  test("Promise.thenResolve", () => {
    module Test = {
      open Promise
      resolve("Anna")
      ->thenResolve(
        str => {
          "Hello " ++ str
        },
      )
      ->thenResolve(
        str => {
          Console.log(str)
        },
      )
      ->ignore // Ignore needed for side-effects
    }
    ()
  })
})

describe("Promise.then", () => {
  test("Promise.then", () => {
    module Test = {
      open Promise
      resolve(5)
      ->then(
        num => {
          resolve(num + 5)
        },
      )
      ->then(
        num => {
          Console.log2("Your lucky number is: ", num)
          resolve()
        },
      )
      ->ignore
    }
    ()
  })
})

describe("Promise.catch", () => {
  test("Promise.catch", () => {
    module Test = {
      open Promise

      exception SomeError(string)

      reject(SomeError("this is an error"))
      ->then(
        _ => {
          Ok("This result will never be returned")->resolve
        },
      )
      ->catch(
        e => {
          let msg = switch e {
          | SomeError(msg) => "ReScript error occurred: " ++ msg
          | Exn.Error(obj) =>
            switch Exn.message(obj) {
            | Some(msg) => "JS exception occurred: " ++ msg
            | None => "Some other JS value has been thrown"
            }
          | _ => "Unexpected error occurred"
          }

          Error(msg)->resolve
        },
      )
      ->then(
        result => {
          switch result {
          | Ok(r) => Console.log2("Operation successful: ", r)
          | Error(msg) => Console.log2("Operation failed: ", msg)
          }->resolve
        },
      )
      ->ignore // Ignore needed for side-effects
    }
    ()
  })
})

describe("Promise.make", () => {
  test("Promise.make", () => {
    module Test = {
      open Promise

      let n = 4
      Promise.make(
        (resolve, reject) => {
          if n < 5 {
            resolve("success")
          } else {
            reject("failed")
          }
        },
      )
      ->then(
        str => {
          Console.log(str)->resolve
        },
      )
      ->catch(
        _ => {
          Console.log("Error occurred")
          resolve()
        },
      )
      ->ignore
    }
    ()
  })
})

describe("Promise.reject", () => {
  test("Promise.reject", () => {
    module Test = {
      exception TestError(string)

      TestError("some rejected value")
      ->Promise.reject
      ->Promise.catch(
        v => {
          switch v {
          | TestError(msg) => assertEqual(msg, "some rejected value")
          | _ => assert(false)
          }
          Promise.resolve()
        },
      )
      ->ignore
    }
    ()
  })
})

describe("Promise.resolve", () => {
  test("Promise.resolve", () => {
    module Test = {
      let p = Promise.resolve(5) // promise<int>
    }
    ()
  })
})

describe("Set.toArray", () => {
  test("Set.toArray", () => {
    module Test = {
      let set = Set.fromArray(["apple", "orange", "apple", "banana"])
      set->Set.toArray // ["apple", "orange", "banana"]
    }
    ()
  })
})

describe("Set.values", () => {
  test("Set.values", () => {
    module Test = {
      let set = Set.make()
      set->Set.add("someValue")
      set->Set.add("anotherValue")

      let values = set->Set.values

      // Logs the first value
      Console.log(Iterator.next(values).value)

      // You can also turn the iterator into an array.
      // Remember that an iterator consumes values. We'll need a fresh values iterator to get an array of all values, since we consumed a value via `next` above already.
      Console.log(set->Set.values->Iterator.toArray)
    }
    ()
  })
})

describe("Set.forEach", () => {
  test("Set.forEach", () => {
    module Test = {
      let set = Set.make()
      set->Set.add("someValue")
      set->Set.add("someValue2")

      set->Set.forEach(
        value => {
          Console.log(value)
        },
      )
    }
    ()
  })
})

describe("Set.has", () => {
  test("Set.has", () => {
    module Test = {
      let set = Set.make()
      set->Set.add("someValue")

      switch set->Set.has("someValue") {
      | false => Console.log("Nope, didn't have it.")
      | true => Console.log("Yay, we have the value!")
      }
    }
    ()
  })
})

describe("Set.delete", () => {
  test("Set.delete", () => {
    module Test = {
      let set = Set.make()
      set->Set.add("someValue")
      let didDeleteValue = set->Set.delete("someValue")
      Console.log(didDeleteValue) // Logs `true` to the console, becuase the set had the value, so it was successfully deleted

      let didDeleteValue = set->Set.delete("someNonExistantKey")
      Console.log(didDeleteValue) // Logs `false` to the console, becuase the value did not exist in the set
    }
    ()
  })
})

describe("Set.add", () => {
  test("Set.add", () => {
    module Test = {
      let set = Set.make()
      set->Set.add("someValue")
    }
    ()
  })
})

describe("Set.clear", () => {
  test("Set.clear", () => {
    module Test = {
      let set = Set.make()

      set->Set.add("someKey")
      set->Set.size // 1

      set->Set.clear
      set->Set.size // 0
    }
    ()
  })
})

describe("Set.size", () => {
  test("Set.size", () => {
    module Test = {
      let set = Set.make()

      set->Set.add("someValue")
      set->Set.add("someValue")
      set->Set.add("someValue2")

      let size = set->Set.size // 2
    }
    ()
  })
})

describe("Set.fromIterator", () => {
  test("Set.fromIterator", () => {
    module Test = {
      // Let's pretend we have an interator
      let iterator: Iterator.t<string> = %raw(`
  (() => {
    var array1 = ['a', 'b', 'c'];
    var iterator1 = array1[Symbol.iterator]();
    return iterator1
  })()
`)

      iterator
      ->Set.fromIterator
      ->Set.size
      ->assertEqual(3)
    }
    ()
  })
})

describe("Set.fromArray", () => {
  test("Set.fromArray", () => {
    module Test = {
      type languages = ReScript | JavaScript | TypeScript
      let languageRank = [ReScript, JavaScript, TypeScript]

      let set = Set.fromArray(languageRank) // Set.t<languages>

      switch set->Set.has(ReScript) {
      | true => Console.log("Yay, ReScript is in there!")
      | false => Console.log("Uh-oh, something is _terribly_ wrong with this program... abort.")
      }
    }
    ()
  })
})

describe("Set.make", () => {
  test("Set.make", () => {
    module Test = {
      // You can annotate the type of your set if you want to
      let mySet: Set.t<string> = Set.make()

      // Or you can let ReScript infer what's in your Set
      let set = Set.make()
      set->Set.add("Fine name") // Inferred as Set.t<string>
    }
    ()
  })
})

describe("String.localeCompare", () => {
  test("String.localeCompare", () => {
    module Test = {
      String.localeCompare("a", "c") < 0.0 == true
      String.localeCompare("a", "a") == 0.0
    }
    ()
  })
})

describe("String.padEnd", () => {
  test("String.padEnd", () => {
    module Test = {
      String.padEnd("Hello", 10, ".") == "Hello....."
      String.padEnd("abc", 1, "") == "abc"
    }
    ()
  })
})

describe("String.padStart", () => {
  test("String.padStart", () => {
    module Test = {
      String.padStart("abc", 5, " ") == "  abc"
      String.padStart("abc", 6, "123465") == "123abc"
    }
    ()
  })
})

describe("String.trimEnd", () => {
  test("String.trimEnd", () => {
    module Test = {
      String.trimEnd("   Hello world!   ") == "   Hello world!"
      String.trimEnd("   Hello   world!   ") == "   Hello   world!"
    }
    ()
  })
})

describe("String.trimStart", () => {
  test("String.trimStart", () => {
    module Test = {
      String.trimStart("   Hello world!   ") == "Hello world!   "
      String.trimStart("   Hello   world!   ") == "Hello   world!   "
    }
    ()
  })
})

describe("String.trim", () => {
  test("String.trim", () => {
    module Test = {
      String.trim("   abc def   ") == "abc def"
      String.trim("\n\r\t abc def \n\n\t\r ") == "abc def"
    }
    ()
  })
})

describe("String.toUpperCase", () => {
  test("String.toUpperCase", () => {
    module Test = {
      String.toUpperCase("abc") == "ABC"
      String.toUpperCase(`Straße`) == `STRASSE`
      String.toUpperCase(`πς`) == `ΠΣ`
    }
    ()
  })
})

describe("String.toLowerCase", () => {
  test("String.toLowerCase", () => {
    module Test = {
      String.toLowerCase("ABC") == "abc"
      String.toLowerCase(`ΣΠ`) == `σπ`
      String.toLowerCase(`ΠΣ`) == `πς`
    }
    ()
  })
})

describe("String.substringToEnd", () => {
  test("String.substringToEnd", () => {
    module Test = {
      String.substringToEnd("playground", ~start=4) == "ground"
      String.substringToEnd("playground", ~start=-3) == "playground"
      String.substringToEnd("playground", ~start=12) == ""
    }
    ()
  })
})

describe("String.substring", () => {
  test("String.substring", () => {
    module Test = {
      String.substring("playground", ~start=3, ~end=6) == "ygr"
      String.substring("playground", ~start=6, ~end=3) == "ygr"
      String.substring("playground", ~start=4, ~end=12) == "ground"
    }
    ()
  })
})

describe("String.startsWithFrom", () => {
  test("String.startsWithFrom", () => {
    module Test = {
      String.startsWithFrom("BuckleScript", "kle", 3) == true
      String.startsWithFrom("BuckleScript", "", 3) == true
      String.startsWithFrom("JavaScript", "Buckle", 2) == false
    }
    ()
  })
})

describe("String.startsWith", () => {
  test("String.startsWith", () => {
    module Test = {
      String.startsWith("BuckleScript", "Buckle") == true
      String.startsWith("BuckleScript", "") == true
      String.startsWith("JavaScript", "Buckle") == false
    }
    ()
  })
})

describe("String.splitByRegExpAtMost", () => {
  test("String.splitByRegExpAtMost", () => {
    module Test = {
      String.splitByRegExpAtMost("Hello World. How are you doing?", / /, ~limit=3) == [
          Some("Hello"),
          Some("World."),
          Some("How"),
        ]
    }
    ()
  })
})

describe("String.splitByRegExp", () => {
  test("String.splitByRegExp", () => {
    module Test = {
      String.splitByRegExp("Jan,Feb,Mar", /,/) == [Some("Jan"), Some("Feb"), Some("Mar")]
    }
    ()
  })
})

describe("String.splitAtMost", () => {
  test("String.splitAtMost", () => {
    module Test = {
      String.splitAtMost("ant/bee/cat/dog/elk", "/", ~limit=3) == ["ant", "bee", "cat"]
      String.splitAtMost("ant/bee/cat/dog/elk", "/", ~limit=0) == []
      String.splitAtMost("ant/bee/cat/dog/elk", "/", ~limit=9) == [
          "ant",
          "bee",
          "cat",
          "dog",
          "elk",
        ]
    }
    ()
  })
})

describe("String.split", () => {
  test("String.split", () => {
    module Test = {
      String.split("2018-01-02", "-") == ["2018", "01", "02"]
      String.split("a,b,,c", ",") == ["a", "b", "", "c"]
      String.split("good::bad as great::awful", "::") == ["good", "bad as great", "awful"]
      String.split("has-no-delimiter", ";") == ["has-no-delimiter"]
    }
    ()
  })
})

describe("String.sliceToEnd", () => {
  test("String.sliceToEnd", () => {
    module Test = {
      String.sliceToEnd("abcdefg", ~start=4) == "efg"
      String.sliceToEnd("abcdefg", ~start=-2) == "fg"
      String.sliceToEnd("abcdefg", ~start=7) == ""
    }
    ()
  })
})

describe("String.slice", () => {
  test("String.slice", () => {
    module Test = {
      String.slice("abcdefg", ~start=2, ~end=5) == "cde"
      String.slice("abcdefg", ~start=2, ~end=9) == "cdefg"
      String.slice("abcdefg", ~start=-4, ~end=-2) == "de"
      String.slice("abcdefg", ~start=5, ~end=1) == ""
    }
    ()
  })
})

describe("String.searchOpt", () => {
  test("String.searchOpt", () => {
    module Test = {
      String.searchOpt("testing 1 2 3", /\d+/) == Some(8)
      String.searchOpt("no numbers", /\d+/) == None
    }
    ()
  })
})

describe("String.search", () => {
  test("String.search", () => {
    module Test = {
      String.search("testing 1 2 3", /\d+/) == 8
      String.search("no numbers", /\d+/) == -1
    }
    ()
  })
})

describe("String.unsafeReplaceRegExpBy2", () => {
  test("String.unsafeReplaceRegExpBy2", () => {
    module Test = {
      let str = "7 times 6"
      let re = /(\d+) times (\d+)/
      let matchFn = (~match as _, ~group1, ~group2, ~offset as _, ~input as _) => {
        switch (Int.fromString(group1), Int.fromString(group2)) {
        | (Some(x), Some(y)) => Int.toString(x * y)
        | _ => "???"
        }
      }
      String.unsafeReplaceRegExpBy2(str, re, matchFn) == "42"
    }
    ()
  })
})

describe("String.unsafeReplaceRegExpBy1", () => {
  test("String.unsafeReplaceRegExpBy1", () => {
    module Test = {
      let str = "Jony is 40"
      let re = /(Jony is )\d+/g
      let matchFn = (~match as _, ~group1, ~offset as _, ~input as _) => {
        group1 ++ "41"
      }
      String.unsafeReplaceRegExpBy1(str, re, matchFn) == "Jony is 41"
    }
    ()
  })
})

describe("String.unsafeReplaceRegExpBy0", () => {
  test("String.unsafeReplaceRegExpBy0", () => {
    module Test = {
      let str = "beautiful vowels"
      let re = /[aeiou]/g
      let matchFn = (~match, ~offset as _, ~input as _) => String.toUpperCase(match)
      String.unsafeReplaceRegExpBy0(str, re, matchFn) == "bEAUtIfUl vOwEls"
    }
    ()
  })
})

describe("String.replaceAllRegExp", () => {
  test("String.replaceAllRegExp", () => {
    module Test = {
      String.replaceAllRegExp("vowels be gone", /[aeiou]/g, "x") == "vxwxls bx gxnx"
      String.replaceAllRegExp("aabbcc", /b/g, ".") == "aa..cc"
    }
    ()
  })
})

describe("String.replaceAll", () => {
  test("String.replaceAll", () => {
    module Test = {
      String.replaceAll("old old string", "old", "new") == "new new string"
      String.replaceAll("the cat and the dog", "the", "this") == "this cat and this dog"
    }
    ()
  })
})

describe("String.replaceRegExp", () => {
  test("String.replaceRegExp", () => {
    module Test = {
      String.replaceRegExp("vowels be gone", /[aeiou]/g, "x") == "vxwxls bx gxnx"
      String.replaceRegExp("Juan Fulano", /(\w+) (\w+)/, "$2, $1") == "Fulano, Juan"
    }
    ()
  })
})

describe("String.replace", () => {
  test("String.replace", () => {
    module Test = {
      String.replace("old string", "old", "new") == "new string"
      String.replace("the cat and the dog", "the", "this") == "this cat and the dog"
    }
    ()
  })
})

describe("String.repeat", () => {
  test("String.repeat", () => {
    module Test = {
      String.repeat("ha", 3) == "hahaha"
      String.repeat("empty", 0) == ""
    }
    ()
  })
})

describe("String.normalizeForm", () => {
  test("String.normalizeForm", () => {
    module Test = {
      let string1 = "\uFB00"
      let string2 = "\u0066\u0066"
      Console.log(string1 == string2) // false

      let normalizeString1 = String.normalizeByForm(string1, #NFKD)
      let normalizeString2 = String.normalizeByForm(string2, #NFKD)
      Console.log(normalizeString1 == normalizeString2) // true
    }
    ()
  })
})

describe("String.normalize", () => {
  test("String.normalize", () => {
    module Test = {
      let string1 = "\u00F1"
      let string2 = "\u006E\u0303"

      assert(string1 != string2) // true
      assertEqual(String.normalize(string1), String.normalize(string2))
    }
    ()
  })
})

describe("String.match", () => {
  test("String.match", () => {
    module Test = {
      String.match("The better bats", /b[aeiou]t/) == Some([Some("bet")])
      String.match("The better bats", /b[aeiou]t/g) == Some([Some("bet"), Some("bat")])
      String.match("Today is 2018-04-05.", /(\d+)-(\d+)-(\d+)/) ==
        Some([Some("2018-04-05"), Some("2018"), Some("04"), Some("05")])
      String.match("The optional example", /(foo)?(example)/) ==
        Some([Some("example"), None, Some("example")])
      String.match("The large container.", /b[aeiou]g/) == None
    }
    ()
  })
})

describe("String.lastIndexOfFrom", () => {
  test("String.lastIndexOfFrom", () => {
    module Test = {
      String.lastIndexOfFrom("bookseller", "ok", 6) == 2
      String.lastIndexOfFrom("beekeeper", "ee", 8) == 4
      String.lastIndexOfFrom("beekeeper", "ee", 3) == 1
      String.lastIndexOfFrom("abcdefg", "xyz", 4) == -1
    }
    ()
  })
})

describe("String.lastIndexOfOpt", () => {
  test("String.lastIndexOfOpt", () => {
    module Test = {
      String.lastIndexOfOpt("bookseller", "ok") == Some(2)
      String.lastIndexOfOpt("beekeeper", "ee") == Some(4)
      String.lastIndexOfOpt("abcdefg", "xyz") == None
    }
    ()
  })
})

describe("String.lastIndexOf", () => {
  test("String.lastIndexOf", () => {
    module Test = {
      String.lastIndexOf("bookseller", "ok") == 2
      String.lastIndexOf("beekeeper", "ee") == 4
      String.lastIndexOf("abcdefg", "xyz") == -1
    }
    ()
  })
})

describe("String.indexOfFrom", () => {
  test("String.indexOfFrom", () => {
    module Test = {
      String.indexOfFrom("bookseller", "ok", 1) == 2
      String.indexOfFrom("bookseller", "sell", 2) == 4
      String.indexOfFrom("bookseller", "sell", 5) == -1
    }
    ()
  })
})

describe("String.indexOfOpt", () => {
  test("String.indexOfOpt", () => {
    module Test = {
      String.indexOfOpt("bookseller", "ok") == Some(2)
      String.indexOfOpt("bookseller", "xyz") == None
    }
    ()
  })
})

describe("String.indexOf", () => {
  test("String.indexOf", () => {
    module Test = {
      String.indexOf("bookseller", "ok") == 2
      String.indexOf("bookseller", "sell") == 4
      String.indexOf("beekeeper", "ee") == 1
      String.indexOf("bookseller", "xyz") == -1
    }
    ()
  })
})

describe("String.includesFrom", () => {
  test("String.includesFrom", () => {
    module Test = {
      String.includesFrom("programmer", "gram", 1) == true
      String.includesFrom("programmer", "gram", 4) == false
      String.includesFrom(`대한민국`, `한`, 1) == true
    }
    ()
  })
})

describe("String.includes", () => {
  test("String.includes", () => {
    module Test = {
      String.includes("programmer", "gram") == true
      String.includes("programmer", "er") == true
      String.includes("programmer", "pro") == true
      String.includes("programmer.dat", "xyz") == false
    }
    ()
  })
})

describe("String.endsWithFrom", () => {
  test("String.endsWithFrom", () => {
    module Test = {
      String.endsWithFrom("abcd", "cd", 4) == true
      String.endsWithFrom("abcde", "cd", 3) == false
      String.endsWithFrom("abcde", "cde", 99) == true
      String.endsWithFrom("example.dat", "ple", 7) == true
    }
    ()
  })
})

describe("String.endsWith", () => {
  test("String.endsWith", () => {
    module Test = {
      String.endsWith("BuckleScript", "Script") == true
      String.endsWith("BuckleShoes", "Script") == false
    }
    ()
  })
})

describe("String.concatMany", () => {
  test("String.concatMany", () => {
    module Test = {
      String.concatMany("1st", ["2nd", "3rd", "4th"]) == "1st2nd3rd4th"
    }
    ()
  })
})

describe("String.concat", () => {
  test("String.concat", () => {
    module Test = {
      String.concat("cow", "bell") == "cowbell"
      String.concat("Re", "Script") == "ReScript"
    }
    ()
  })
})

describe("String.codePointAt", () => {
  test("String.codePointAt", () => {
    module Test = {
      String.codePointAt(`¿😺?`, 1) == Some(0x1f63a)
      String.codePointAt("abc", 5) == None
    }
    ()
  })
})

describe("String.charCodeAt", () => {
  test("String.charCodeAt", () => {
    module Test = {
      String.charCodeAt(`😺`, 0) == 0xd83d->Int.toFloat
      String.codePointAt(`😺`, 0) == Some(0x1f63a)
    }
    ()
  })
})

describe("String.charAt", () => {
  test("String.charAt", () => {
    module Test = {
      String.charAt("ReScript", 0) == "R"
      String.charAt("Hello", 12) == ""
      String.charAt(`JS`, 5) == ""
    }
    ()
  })
})

describe("String.getUnsafe", () => {
  test("String.getUnsafe", () => {
    module Test = {
      String.getUnsafe("ReScript", 0) == "R"
      String.getUnsafe("Hello", 4) == "o"
    }
    ()
  })
})

describe("String.get", () => {
  test("String.get", () => {
    module Test = {
      String.get("ReScript", 0) == Some("R")
      String.get("Hello", 4) == Some("o")
      String.get(`JS`, 4) == None
    }
    ()
  })
})

describe("String.length", () => {
  test("String.length", () => {
    module Test = {
      String.length("abcd") == 4
    }
    ()
  })
})

describe("String.fromCodePointMany", () => {
  test("String.fromCodePointMany", () => {
    module Test = {
      String.fromCodePointMany([0xd55c, 0xae00, 0x1f63a]) == `한글😺`
    }
    ()
  })
})

describe("String.fromCodePoint", () => {
  test("String.fromCodePoint", () => {
    module Test = {
      String.fromCodePoint(65) == "A"
      String.fromCodePoint(0x3c8) == `ψ`
      String.fromCodePoint(0xd55c) == `한`
      String.fromCodePoint(0x1f63a) == `😺`
    }
    ()
  })
})

describe("String.fromCharCodeMany", () => {
  test("String.fromCharCodeMany", () => {
    module Test = {
      String.fromCharCodeMany([189, 43, 190, 61]) == "½+¾="
      String.fromCharCodeMany([65, 66, 67]) == "ABC"
    }
    ()
  })
})

describe("String.fromCharCode", () => {
  test("String.fromCharCode", () => {
    module Test = {
      String.fromCharCode(65) == "A"
      String.fromCharCode(0x3c8) == `ψ`
      String.fromCharCode(0xd55c) == `한`
      String.fromCharCode(-64568) == `ψ`
    }
    ()
  })
})

describe("String.make", () => {
  test("String.make", () => {
    module Test = {
      String.make(3.5) == "3.5"
      String.make([1, 2, 3]) == "1,2,3"
    }
    ()
  })
})

describe("Type.Classify.classify", () => {
  test("Type.Classify.classify", () => {
    module Test = {
      switch %raw(`null`)->Type.Classify.classify {
      | Null => Console.log("Yup, that's null.")
      | _ => Console.log("This doesn't actually appear to be null...")
      }
    }
    ()
  })
})

describe("Type.typeof", () => {
  test("Type.typeof", () => {
    module Test = {
      Console.log(Type.typeof("Hello")) // Logs "string" to the console.

      let someVariable = true

      switch someVariable->Type.typeof {
      | #boolean => Console.log("This is a bool, yay!")
      | _ => Console.log("Oh, not a bool sadly...")
      }
    }
    ()
  })
})
