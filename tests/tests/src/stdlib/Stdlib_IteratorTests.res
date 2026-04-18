let eq = (a, b) => a == b

let supportsIteratorHelpers: bool = %raw(`typeof ((function*(){ yield 1; })()).map === "function"`)

let _checkIteratorHelperTypes = () => {
  let iteratorObject: IteratorObject.t<int, string, string> = %raw(`(function* () {
    yield 1
    return "done"
  })()`)

  let _mapped: IteratorObject.t<string, unit, unknown> =
    iteratorObject->IteratorObject.map(value => value->Int.toString)
  let _filtered: IteratorObject.t<int, unit, unknown> =
    iteratorObject->IteratorObject.filter(value => value > 0)
  let _dropped: IteratorObject.t<int, unit, unknown> = iteratorObject->IteratorObject.drop(1)
  let _taken: IteratorObject.t<int, unit, unknown> = iteratorObject->IteratorObject.take(1)
  let _flatMapped: IteratorObject.t<int, unit, unknown> =
    iteratorObject->IteratorObject.flatMap(value => [value, value + 1]->Array.asIterable)
}

if supportsIteratorHelpers {
  let makeIteratorObject = (): IteratorObject.t<int, unit, unit> =>
    %raw(`(function* () {
    yield 1
    yield 2
    yield 3
  })()`)

  Test.run(
    __POS_OF__("IteratorObject.toArrayWithMapper"),
    makeIteratorObject()->IteratorObject.toArrayWithMapper(value => value + 10),
    eq,
    [11, 12, 13],
  )

  Test.run(
    __POS_OF__("IteratorObject.map"),
    makeIteratorObject()->IteratorObject.map(value => value + 1)->IteratorObject.toArray,
    eq,
    [2, 3, 4],
  )

  Test.run(
    __POS_OF__("IteratorObject.filter"),
    makeIteratorObject()
    ->IteratorObject.filter(value => mod(value, 2) == 1)
    ->IteratorObject.toArray,
    eq,
    [1, 3],
  )

  Test.run(
    __POS_OF__("IteratorObject.drop"),
    makeIteratorObject()->IteratorObject.drop(1)->IteratorObject.toArray,
    eq,
    [2, 3],
  )

  Test.run(
    __POS_OF__("IteratorObject.take"),
    makeIteratorObject()->IteratorObject.take(2)->IteratorObject.toArray,
    eq,
    [1, 2],
  )

  Test.run(
    __POS_OF__("IteratorObject.flatMap"),
    makeIteratorObject()
    ->IteratorObject.flatMap(value => [value, value + 10]->Array.asIterable)
    ->IteratorObject.toArray,
    eq,
    [1, 11, 2, 12, 3, 13],
  )

  Test.run(
    __POS_OF__("IteratorObject.reduce"),
    makeIteratorObject()->IteratorObject.reduce((sum, value) => sum + value, ~initialValue=0),
    eq,
    6,
  )

  Test.run(
    __POS_OF__("IteratorObject.every"),
    makeIteratorObject()->IteratorObject.every(value => value > 0),
    eq,
    true,
  )

  Test.run(
    __POS_OF__("IteratorObject.some"),
    makeIteratorObject()->IteratorObject.some(value => value == 2),
    eq,
    true,
  )

  Test.run(
    __POS_OF__("IteratorObject.find"),
    makeIteratorObject()->IteratorObject.find(value => value == 3),
    eq,
    Some(3),
  )
}

Test.run(
  __POS_OF__("IteratorObject.toArrayWithMapper (built-in array iterator)"),
  [1, 2, 3]->Array.values->IteratorObject.toArrayWithMapper(value => value + 20),
  eq,
  [21, 22, 23],
)

let iterator: IterableIterator.t<string, unit, unit> = %raw(`
  (() => {
    var array1 = ['a', 'b', 'c'];
    var iterator1 = array1[Symbol.iterator]();
    return iterator1
  })()
`)

let syncResult = ref(None)

switch iterator->IterableIterator.next {
| Yield({value: "a"}) => ()
| _ => ()
}

switch iterator->IterableIterator.next {
| Yield({value: "b"}) => syncResult.contents = Some("b")
| _ => ()
}

Test.run(__POS_OF__("Sync forEach"), syncResult.contents, eq, Some("b"))

let iteratorProtocol: Iterator.t<string, unit, unit> = %raw(`
  (() => {
    var array1 = ['x', 'y'];
    var iterator1 = array1[Symbol.iterator]();
    return iterator1
  })()
`)

let protocolResult = ref(None)

switch iteratorProtocol->Iterator.next {
| Yield({value: "x"}) => protocolResult.contents = Some("x")
| _ => ()
}

Test.run(__POS_OF__("Iterator next"), protocolResult.contents, eq, Some("x"))

let omittedDoneIterator: Iterator.t<int, string, unit> = %raw(`({
  step: 0,
  next() {
    if (this.step === 0) {
      this.step = 1
      return {value: 1}
    }
    return {done: true, value: "done"}
  }
})`)

let omittedDoneResult = ref(None)

switch omittedDoneIterator->Iterator.next {
| Yield({value: 1}) => omittedDoneResult.contents = Some("yield")
| _ => ()
}

Test.run(
  __POS_OF__("Iterator next with omitted done"),
  omittedDoneResult.contents,
  eq,
  Some("yield"),
)

let createdIterator = {
  let current = ref(0)
  Iterator.make(() => {
    let value = current.contents
    current := value + 1

    if value >= 2 {
      Iterator.doneWithValue("done")
    } else {
      Iterator.value(value)
    }
  })
}

let createdProtocolResult = ref(None)

switch createdIterator->Iterator.next {
| Yield({value: 0}) => createdProtocolResult.contents = Some("protocol")
| _ => ()
}

Test.run(
  __POS_OF__("Creating your own iterator"),
  createdProtocolResult.contents,
  eq,
  Some("protocol"),
)

let createdIterableIterator = {
  let current = ref(0)
  IterableIterator.make(() => {
    let value = current.contents
    current := value + 1

    if value >= 2 {
      Iterator.doneWithValue("done")
    } else {
      Iterator.value(value)
    }
  })
}

let createdIterableResult = ref(None)

switch createdIterableIterator->IterableIterator.next {
| Yield({value: 0}) => ()
| _ => ()
}

switch createdIterableIterator->IterableIterator.next {
| Yield({value: 1}) => createdIterableResult.contents = Some("iterable")
| _ => ()
}

Test.run(
  __POS_OF__("Creating your own iterable iterator"),
  createdIterableResult.contents,
  eq,
  Some("iterable"),
)

let generatorViaIterator: Generator.t<int, string, option<int>> = %raw(`(function* () {
  let sent = yield 1
  return String((sent ?? 0) + 1)
})()`)

let iteratorNextValueResult = ref(None)

switch generatorViaIterator->Generator.asIterator->Iterator.nextValue(None) {
| Yield({value: 1}) => ()
| _ => ()
}

switch generatorViaIterator->Generator.asIterator->Iterator.nextValue(Some(4)) {
| Return({value: "5"}) => iteratorNextValueResult.contents = Some("nextValue")
| _ => ()
}

Test.run(__POS_OF__("Iterator nextValue"), iteratorNextValueResult.contents, eq, Some("nextValue"))

Test.run(
  __POS_OF__("Generator.asIterable"),
  (
    %raw(`(function* () {
      yield 1
      yield 2
    })()`): Generator.t<int, unit, unit>
  )
  ->Generator.asIterable
  ->Array.fromIterable,
  eq,
  [1, 2],
)

Test.run(
  __POS_OF__("Generator.asIteratorObject"),
  (
    %raw(`(function* () {
      yield 1
      yield 2
    })()`): Generator.t<int, unit, unit>
  )
  ->Generator.asIteratorObject
  ->IteratorObject.toArrayWithMapper(value => value * 2),
  eq,
  [2, 4],
)

Test.run(
  __POS_OF__("Generator.asIteratorObject nextValue"),
  {
    let generatorWithNext: Generator.t<int, unit, int> = %raw(`(function* () {
      let injected = yield 1
      yield injected + 1
    })()`)

    switch generatorWithNext->Generator.next {
    | Yield({value: 1}) =>
      switch generatorWithNext->Generator.asIteratorObject->IteratorObject.nextValue(41) {
      | Yield({value: 42}) => Some("nextValue")
      | _ => None
      }
    | _ => None
    }
  },
  eq,
  Some("nextValue"),
)

let generatorReturnValueResult = ref(None)
let generatorReturnValue: Generator.t<int, string, unit> = %raw(`(function* () {
  yield 1
  return "done"
})()`)

switch generatorReturnValue->Generator.next {
| Yield({value: 1}) => ()
| _ => ()
}

switch generatorReturnValue->Generator.returnValue("stopped") {
| Return({value: "stopped"}) => generatorReturnValueResult.contents = Some("returnValue")
| _ => ()
}

Test.run(
  __POS_OF__("Generator.returnValue"),
  generatorReturnValueResult.contents,
  eq,
  Some("returnValue"),
)

let generatorThrowErrorResult = ref(None)
let generatorThrowError: Generator.t<int, string, unit> = %raw(`(function* () {
  try {
    yield 1
    return "done"
  } catch (error) {
    return error.message
  }
})()`)

switch generatorThrowError->Generator.next {
| Yield({value: 1}) => ()
| _ => ()
}

switch generatorThrowError->Generator.throwError(JsExn.anyToExnInternal(JsError.make("boom"))) {
| Return(_) => generatorThrowErrorResult.contents = Some("throwError")
| _ => ()
}

Test.run(
  __POS_OF__("Generator.throwError"),
  generatorThrowErrorResult.contents,
  eq,
  Some("throwError"),
)

let asyncIterableIterator: AsyncIterableIterator.t<(string, string), unit, unit> = %raw(`
  (async function* () {
    yield ['first', '1']
    yield ['second', '2']
  })()
`)

let asyncResult = ref(None)

switch await asyncIterableIterator->AsyncIterableIterator.next {
| Yield({value: ("first", _value)}) => ()
| _ => ()
}

await asyncIterableIterator->AsyncIterableIterator.forEach(((key, _value)) => {
  if key == "second" {
    asyncResult.contents = Some("second")
  }
})

Test.run(__POS_OF__("Async forEach"), asyncResult.contents, eq, Some("second"))

%%private(
  let asyncResult = ref(None)
  let count = ref(0)
  let asyncIterableIteratorResult = ref(None)
  let asyncIterableIteratorCount = ref(0)
)

let asyncIterator = AsyncIterator.make(async () => {
  let currentCount = count.contents
  count := currentCount + 1

  if currentCount === 3 {
    AsyncIterator.doneWithValue(currentCount)
  } else {
    AsyncIterator.value(currentCount)
  }
})

await asyncIterator->AsyncIterator.forEach(value => {
  if value == 2 {
    asyncResult.contents = Some("done")
  }
})

Test.run(__POS_OF__("Creating your own async iterator"), asyncResult.contents, eq, Some("done"))

let asyncOmittedDoneValues = ref([])
let asyncOmittedDoneIterator: AsyncIterator.t<int, string, unit> = %raw(`({
  step: 0,
  next() {
    if (this.step === 0) {
      this.step = 1
      return Promise.resolve({value: 1})
    }
    return Promise.resolve({done: true, value: "done"})
  }
})`)

await asyncOmittedDoneIterator->AsyncIterator.forEach(value => {
  asyncOmittedDoneValues := [...asyncOmittedDoneValues.contents, value]
})

Test.run(
  __POS_OF__("AsyncIterator.forEach with omitted done"),
  asyncOmittedDoneValues.contents,
  eq,
  [1],
)

let asyncGeneratorNextValue: AsyncGenerator.t<
  int,
  string,
  option<int>,
> = %raw(`(async function* () {
  let sent = yield 1
  return String((sent ?? 0) + 1)
})()`)

let asyncGeneratorNextValueResult = ref(None)

switch await asyncGeneratorNextValue
->AsyncGenerator.asAsyncIterator
->AsyncIterator.nextValue(None) {
| Yield({value: 1}) => ()
| _ => ()
}

switch await asyncGeneratorNextValue->AsyncGenerator.nextValue(Some(4)) {
| Return({value: "5"}) => asyncGeneratorNextValueResult.contents = Some("nextValue")
| _ => ()
}

Test.run(
  __POS_OF__("AsyncGenerator.nextValue"),
  asyncGeneratorNextValueResult.contents,
  eq,
  Some("nextValue"),
)

let asyncGeneratorIterableValues = ref([])
let asyncGeneratorIterable: AsyncGenerator.t<int, unit, unit> = %raw(`(async function* () {
  yield 2
  yield 3
})()`)

await asyncGeneratorIterable
->AsyncGenerator.asAsyncIterableIterator
->AsyncIterableIterator.forEach(value =>
  asyncGeneratorIterableValues := [...asyncGeneratorIterableValues.contents, value]
)

Test.run(
  __POS_OF__("AsyncGenerator.asAsyncIterableIterator"),
  asyncGeneratorIterableValues.contents,
  eq,
  [2, 3],
)

let _asyncIterableView: AsyncIterable.t<int> = (
  %raw(`(async function* () {
      yield 1
    })()`): AsyncGenerator.t<int, unit, unit>
)->AsyncGenerator.asAsyncIterable

let asyncGeneratorReturnValueResult = ref(None)
let asyncGeneratorReturnValue: AsyncGenerator.t<int, string, unit> = %raw(`(async function* () {
  yield 1
  return "done"
})()`)

switch await asyncGeneratorReturnValue->AsyncGenerator.next {
| Yield({value: 1}) => ()
| _ => ()
}

switch await asyncGeneratorReturnValue->AsyncGenerator.returnValue("stopped") {
| Return({value: "stopped"}) => asyncGeneratorReturnValueResult.contents = Some("returnValue")
| _ => ()
}

Test.run(
  __POS_OF__("AsyncGenerator.returnValue"),
  asyncGeneratorReturnValueResult.contents,
  eq,
  Some("returnValue"),
)

let asyncGeneratorThrowErrorResult = ref(None)
let asyncGeneratorThrowError: AsyncGenerator.t<int, string, unit> = %raw(`(async function* () {
  try {
    yield 1
    return "done"
  } catch (error) {
    return error.message
  }
})()`)

switch await asyncGeneratorThrowError->AsyncGenerator.next {
| Yield({value: 1}) => ()
| _ => ()
}

switch await asyncGeneratorThrowError->AsyncGenerator.throwError(
  JsExn.anyToExnInternal(JsError.make("boom")),
) {
| Return(_) => asyncGeneratorThrowErrorResult.contents = Some("throwError")
| _ => ()
}

Test.run(
  __POS_OF__("AsyncGenerator.throwError"),
  asyncGeneratorThrowErrorResult.contents,
  eq,
  Some("throwError"),
)

let createdAsyncIterableIterator = AsyncIterableIterator.make(async () => {
  let currentCount = asyncIterableIteratorCount.contents
  asyncIterableIteratorCount := currentCount + 1

  if currentCount === 2 {
    AsyncIterator.done()
  } else {
    AsyncIterator.value(currentCount)
  }
})

await createdAsyncIterableIterator->AsyncIterableIterator.forEach(value => {
  if value == 1 {
    asyncIterableIteratorResult.contents = Some("iterable")
  }
})

Test.run(
  __POS_OF__("Creating your own async iterable iterator"),
  asyncIterableIteratorResult.contents,
  eq,
  Some("iterable"),
)
