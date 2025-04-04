exception TestError(string)

let fail = msg => {
  Exn.raiseError(msg)
}

let equal = (a, b) => {
  a == b
}

module Creation = {
  let resolveTest = () => {
    open Promise

    Promise.resolve("test")
    ->then(str => {
      Test.run(__POS_OF__("Should resolve test"), str, equal, "test")
      resolve()
    })
    ->ignore
  }

  let runTests = () => {
    resolveTest()
  }
}

module ThenChaining = {
  // A promise should be able to return a nested
  // Promise and also flatten it for another then call
  // to the actual value
  let testThen = () => {
    open Promise
    resolve(1)
    ->then(first => {
      resolve(first + 1)
    })
    ->then(value => {
      Test.run(__POS_OF__("Should be 2"), value, equal, 2)
      resolve()
    })
  }

  // It's not allowed to return a promise<promise<'a>> value
  // within a then. This operation will throw an error
  let testInvalidThen = () => {
    open Promise
    resolve(1)
    ->then(first => {
      resolve(resolve(first + 1))
    })
    ->then(p => {
      let isPromise = Type.typeof((p: promise<_>)) == #object
      Test.run(__POS_OF__("Should not be a promise"), isPromise, equal, false)
      resolve()
    })
  }

  let testThenResolve = () => {
    open Promise

    resolve(1)
    ->thenResolve(num => {
      num + 1
    })
    ->thenResolve(ret => {
      Test.run(__POS_OF__("Should be 2"), ret, equal, 2)
    })
  }

  let testInvalidThenResolve = () => {
    open Promise

    resolve(1)
    ->thenResolve(num => {
      // This is against the law
      resolve(num)
    })
    ->then(p => {
      let isPromise = Type.typeof((p: promise<_>)) == #object
      Test.run(__POS_OF__("Should not be a promise"), isPromise, equal, false)
      resolve()
    })
  }

  let runTests = () => {
    testThen()->ignore
    testInvalidThen()->ignore
    testThenResolve()->ignore
    testInvalidThenResolve()->ignore
  }
}

module Rejection = {
  // Should gracefully handle a exn passed via reject()
  let testExnRejection = () => {
    let cond = "Expect rejection to contain a TestError"
    open Promise

    TestError("oops")
    ->reject
    ->catch(e => {
      Test.run(__POS_OF__(cond), e, equal, TestError("oops"))
      resolve()
    })
    ->ignore
  }

  let runTests = () => {
    testExnRejection()->ignore
  }
}

module Catching = {
  let asyncParseFail: unit => promise<string> = %raw(`
  function() {
    return new Promise((resolve) => {
      var result = JSON.parse("{..");
      return resolve(result);
    })
  }
  `)

  // Should correctly capture an JS error thrown within
  // a Promise `then` function
  let testExternalPromiseThrow = () => {
    open Promise

    asyncParseFail()
    ->then(_ => resolve()) // Since our asyncParse will fail anyways, we convert to promise<unit> for our catch later
    ->catch(e => {
      let success = switch e {
      | Exn.Error(err) => Exn.name(err) == Some("SyntaxError")
      | _ => false
      }

      Test.run(__POS_OF__("Should be a parser error with Unexpected token ."), success, equal, true)
      resolve()
    })
  }

  // Should correctly capture an exn thrown in a Promise
  // `then` function
  let testExnThrow = () => {
    open Promise

    resolve()
    ->then(_ => {
      raise(TestError("Thrown exn"))
    })
    ->catch(e => {
      let isTestErr = switch e {
      | TestError("Thrown exn") => true
      | _ => false
      }
      Test.run(__POS_OF__("Should be a TestError"), isTestErr, equal, true)
      resolve()
    })
  }

  // Should correctly capture a JS error raised with Exn.raiseError
  // within a Promise then function
  let testRaiseErrorThrow = () => {
    open Promise

    let causeErr = () => {
      Exn.raiseError("Some JS error")
    }

    resolve()
    ->then(_ => {
      causeErr()
    })
    ->catch(e => {
      let isTestErr = switch e {
      | Exn.Error(err) => Exn.message(err) == Some("Some JS error")
      | _ => false
      }
      Test.run(__POS_OF__("Should be some JS error"), isTestErr, equal, true)
      resolve()
    })
  }

  // Should recover a rejection and use then to
  // access the value
  let thenAfterCatch = () => {
    open Promise
    resolve()
    ->then(_ => {
      // NOTE: if then is used, there will be an uncaught
      // error
      reject(TestError("some rejected value"))
    })
    ->catch(e => {
      let s = switch e {
      | TestError("some rejected value") => "success"
      | _ => "not a test error"
      }
      resolve(s)
    })
    ->then(msg => {
      Test.run(__POS_OF__("Should be success"), msg, equal, "success")
      resolve()
    })
  }

  let testCatchFinally = () => {
    open Promise
    let wasCalled = ref(false)
    resolve(5)
    ->then(_ => {
      reject(TestError("test"))
    })
    ->then(v => {
      v->resolve
    })
    ->catch(_ => {
      resolve()
    })
    ->finally(() => {
      wasCalled := true
    })
    ->then(v => {
      Test.run(__POS_OF__("value should be unit"), v, equal, ())
      Test.run(__POS_OF__("finally should have been called"), wasCalled.contents, equal, true)
      resolve()
    })
    ->ignore
  }

  let testResolveFinally = () => {
    open Promise
    let wasCalled = ref(false)
    resolve(5)
    ->then(v => {
      resolve(v + 5)
    })
    ->finally(() => {
      wasCalled := true
    })
    ->then(v => {
      Test.run(__POS_OF__("value should be 5"), v, equal, 10)
      Test.run(__POS_OF__("finally should have been called"), wasCalled.contents, equal, true)
      resolve()
    })
    ->ignore
  }

  let runTests = () => {
    testExternalPromiseThrow()->ignore
    testExnThrow()->ignore
    testRaiseErrorThrow()->ignore
    thenAfterCatch()->ignore
    testCatchFinally()->ignore
    testResolveFinally()->ignore
  }
}

module Concurrently = {
  let testParallel = () => {
    open Promise

    let place = ref(0)

    let delayedMsg = (ms, msg) => {
      Promise.make((resolve, _) => {
        setTimeout(() => {
          place := place.contents + 1
          resolve((place.contents, msg))
        }, ms)->TimeoutId.ignore
      })
    }

    let p1 = delayedMsg(1000, "is Anna")
    let p2 = delayedMsg(500, "myName")
    let p3 = delayedMsg(100, "Hi")

    all([p1, p2, p3])->then(arr => {
      let exp = [(3, "is Anna"), (2, "myName"), (1, "Hi")]
      Test.run(__POS_OF__("Should have correct placing"), arr, equal, exp)
      resolve()
    })
  }

  let testRace = () => {
    open Promise

    let racer = (ms, name) => {
      Promise.make((resolve, _) => {
        setTimeout(() => {
          resolve(name)
        }, ms)->TimeoutId.ignore
      })
    }

    let promises = [racer(1000, "Turtle"), racer(500, "Hare"), racer(100, "Eagle")]

    race(promises)->then(winner => {
      Test.run(__POS_OF__("Eagle should win"), winner, equal, "Eagle")
      resolve()
    })
  }

  let testParallel2 = () => {
    open Promise

    let place = ref(0)

    let delayedMsg = (ms, msg) => {
      Promise.make((resolve, _) => {
        setTimeout(() => {
          place := place.contents + 1
          resolve((place.contents, msg))
        }, ms)->TimeoutId.ignore
      })
    }

    let p1 = delayedMsg(1000, "is Anna")
    let p2 = delayedMsg(500, "myName")

    all2((p1, p2))->then(arr => {
      let exp = ((2, "is Anna"), (1, "myName"))
      Test.run(__POS_OF__("Should have correct placing"), arr, equal, exp)
      resolve()
    })
  }

  let testParallel3 = () => {
    open Promise

    let place = ref(0)

    let delayedMsg = (ms, msg) => {
      Promise.make((resolve, _) => {
        setTimeout(() => {
          place := place.contents + 1
          resolve((place.contents, msg))
        }, ms)->TimeoutId.ignore
      })
    }

    let p1 = delayedMsg(1000, "is Anna")
    let p2 = delayedMsg(500, "myName")
    let p3 = delayedMsg(100, "Hi")

    all3((p1, p2, p3))->then(arr => {
      let exp = ((3, "is Anna"), (2, "myName"), (1, "Hi"))
      Test.run(__POS_OF__("Should have correct placing"), arr, equal, exp)
      resolve()
    })
  }

  let testParallel4 = () => {
    open Promise

    let place = ref(0)

    let delayedMsg = (ms, msg) => {
      Promise.make((resolve, _) => {
        setTimeout(() => {
          place := place.contents + 1
          resolve((place.contents, msg))
        }, ms)->TimeoutId.ignore
      })
    }

    let p1 = delayedMsg(1500, "Anna")
    let p2 = delayedMsg(1000, "is")
    let p3 = delayedMsg(500, "my name")
    let p4 = delayedMsg(100, "Hi")

    all4((p1, p2, p3, p4))->then(arr => {
      let exp = ((4, "Anna"), (3, "is"), (2, "my name"), (1, "Hi"))
      Test.run(__POS_OF__("Should have correct placing"), arr, equal, exp)
      resolve()
    })
  }

  let testParallel5 = () => {
    open Promise

    let place = ref(0)

    let delayedMsg = (ms, msg) => {
      Promise.make((resolve, _) => {
        setTimeout(() => {
          place := place.contents + 1
          resolve((place.contents, msg))
        }, ms)->TimeoutId.ignore
      })
    }

    let p1 = delayedMsg(1500, "Anna")
    let p2 = delayedMsg(1000, "is")
    let p3 = delayedMsg(500, "name")
    let p4 = delayedMsg(100, "my")
    let p5 = delayedMsg(50, "Hi")

    all5((p1, p2, p3, p4, p5))->then(arr => {
      let exp = ((5, "Anna"), (4, "is"), (3, "name"), (2, "my"), (1, "Hi"))
      Test.run(__POS_OF__("Should have correct placing"), arr, equal, exp)
      resolve()
    })
  }

  let testParallel6 = () => {
    open Promise

    let place = ref(0)

    let delayedMsg = (ms, msg) => {
      Promise.make((resolve, _) => {
        setTimeout(() => {
          place := place.contents + 1
          resolve((place.contents, msg))
        }, ms)->TimeoutId.ignore
      })
    }

    let p1 = delayedMsg(1500, "Anna")
    let p2 = delayedMsg(1000, "is")
    let p3 = delayedMsg(500, "name")
    let p4 = delayedMsg(100, "my")
    let p5 = delayedMsg(50, ", ")
    let p6 = delayedMsg(10, "Hi")

    all6((p1, p2, p3, p4, p5, p6))->then(arr => {
      let exp = ((6, "Anna"), (5, "is"), (4, "name"), (3, "my"), (2, ", "), (1, "Hi"))
      Test.run(__POS_OF__("Should have correct placing"), arr, equal, exp)
      resolve()
    })
  }

  let runTests = () => {
    testParallel()->ignore
    testRace()->ignore
    testParallel2()->ignore
    testParallel3()->ignore
    testParallel4()->ignore
    testParallel5()->ignore
    testParallel6()->ignore
  }
}

Creation.runTests()
ThenChaining.runTests()
Rejection.runTests()
Catching.runTests()
Concurrently.runTests()
