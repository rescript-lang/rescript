let eq = (a, b) => a == b

let iterator: Iterator.t<string> = %raw(`
  (() => {
    var array1 = ['a', 'b', 'c'];
    var iterator1 = array1[Symbol.iterator]();
    return iterator1
  })()
`)

let syncResult = ref(None)

%%raw(`
if (!Iterator.prototype.forEach) {
  Iterator.prototype.forEach = function forEach(callback, thisArg) {
    if (typeof callback !== 'function') {
      throw new TypeError(callback + ' is not a function');
    }

    let index = 0;
    let result = this.next();

    while (!result.done) {
      callback.call(thisArg, result.value, index, this);
      result = this.next();
      index++;
    }
  };
}
`)

iterator->Iterator.forEach(v => {
  if v == "b" {
    syncResult.contents = Some("b")
  }
})

Test.run(__POS_OF__("Sync forEach"), syncResult.contents, eq, Some("b"))

let asyncIterator: AsyncIterator.t<(string, string)> = %raw(`
  (() => {
    var map1 = new Map();

    map1.set('first', '1');
    map1.set('second', '2');

    var iterator1 = map1[Symbol.iterator]();
    return iterator1;
  })()
`)

let asyncResult = ref(None)

await asyncIterator->AsyncIterator.forEach(v => {
  switch v {
  | Some(("second", _value)) => asyncResult.contents = Some("second")
  | _ => ()
  }
})

Test.run(__POS_OF__("Async forEach"), asyncResult.contents, eq, Some("second"))

%%private(
  let asyncResult = ref(None)
  let count = ref(0)
)

let asyncIterator = AsyncIterator.make(async () => {
  let currentCount = count.contents
  count := currentCount + 1

  if currentCount === 3 {
    AsyncIterator.done(~finalValue=currentCount)
  } else {
    AsyncIterator.value(currentCount)
  }
})

await asyncIterator->AsyncIterator.forEach(v => {
  switch v {
  | Some(3) => asyncResult.contents = Some("done")
  | _ => Console.log("next..")
  }
})

Test.run(__POS_OF__("Creating your own async iterator"), asyncResult.contents, eq, Some("done"))
