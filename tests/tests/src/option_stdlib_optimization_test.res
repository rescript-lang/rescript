open Mocha
open Test_utils

// Test Option.forEach/Option.map optimizations generate optimal JavaScript
// These tests verify that the optimized calls are transformed to efficient
// switch statements that match handwritten switch patterns

let globalValue = 89

module PipeChain = {
  // Modeled after some real world code that chains a lot of
  // Option.flatMap/Option.map calls.
  type incident = {incidentId: string, categoryId: option<string>}
  type category = {categoryId: string, name: string}

  let getIncidentCategoryName = (
    incidents: Belt.Map.String.t<incident>,
    categories: Belt.Map.String.t<category>,
    ~incidentId,
  ) =>
    incidentId
    ->Option.flatMap(incidentId => incidents->Belt.Map.String.get(incidentId))
    ->Option.flatMap(incident => incident.categoryId)
    ->Option.flatMap(categoryId => categories->Belt.Map.String.get(categoryId))
    ->Option.map(category => category.name)
}

module ForEach = {
  let testPrimitive = () => {
    let opt = Some(42)
    Option.forEach(opt, value => Console.log(value))
  }

  let testPrimitivePipe = () => {
    let opt = Some(42)
    opt->Option.forEach(value => Console.log(value))
  }

  let testPrimitiveRef = () => {
    let opt = Some(42)
    switch opt {
    | Some(value) => Console.log(value)
    | None => ()
    }
  }

  let testNone = () => {
    let opt = None
    Option.forEach(opt, value => Console.log(globalValue + value))
  }

  let testNonePipe = () => {
    let opt = None
    opt->Option.forEach(value => Console.log(globalValue + value))
  }

  let testNoneRef = () => {
    let opt = None
    switch opt {
    | Some(value) => Console.log(globalValue + value)
    | None => ()
    }
  }

  let testQualified = () => {
    let opt = Some("hello")
    Stdlib_Option.forEach(opt, value => Console.log(value))
  }

  let testQualifiedPipe = () => {
    let opt = Some("hello")
    opt->Stdlib_Option.forEach(value => Console.log(value))
  }

  let testQualifiedRef = () => {
    let opt = Some("hello")
    switch opt {
    | Some(value) => Console.log(value)
    | None => ()
    }
  }

  let testComplexExpr = () => {
    let opt = Some(true)
    Option.forEach(opt, value =>
      if value {
        Console.log("true")
      } else {
        Console.log("false")
      }
    )
  }

  let testComplexExprPipe = () => {
    let opt = Some(true)
    opt->Option.forEach(value =>
      if value {
        Console.log("true")
      } else {
        Console.log("false")
      }
    )
  }

  let testComplexExprRef = () => {
    let opt = Some(true)
    switch opt {
    | Some(value) =>
      if value {
        Console.log("true")
      } else {
        Console.log("false")
      }
    | None => ()
    }
  }

  let testNestedCalls = () => {
    let opt = Some([1, 2, 3])
    Option.forEach(opt, value => Array.forEach(value, item => Console.log(item)))
  }

  let testNestedCallsPipe = () => {
    let opt = Some([1, 2, 3])
    opt->Option.forEach(value => Array.forEach(value, item => Console.log(item)))
  }

  let testNestedCallsRef = () => {
    let opt = Some([1, 2, 3])
    switch opt {
    | Some(value) => Array.forEach(value, item => Console.log(item))
    | None => ()
    }
  }

  let testNamedCallback = () => {
    let opt = Some(42)
    let log = value => Console.log(value)
    Option.forEach(opt, log)
  }

  let testNamedCallbackPipe = () => {
    let opt = Some(42)
    let log = value => Console.log(value)
    opt->Option.forEach(log)
  }

  let testNamedCallbackRef = () => {
    let opt = Some(42)
    let log = value => Console.log(value)
    switch opt {
    | Some(value) => log(value)
    | None => ()
    }
  }

  let testMultiple = () => {
    let opt1 = Some("first")
    let opt2 = Some("second")
    let opt3 = None

    Option.forEach(opt1, value => Console.log(value))
    Option.forEach(opt2, value => Console.log(value))
    Option.forEach(opt3, value => Console.log(value))
  }

  let testMultiplePipe = () => {
    let opt1 = Some("first")
    let opt2 = Some("second")
    let opt3 = None

    opt1->Option.forEach(value => Console.log(value))
    opt2->Option.forEach(value => Console.log(value))
    opt3->Option.forEach(value => Console.log(value))
  }

  let testMultipleRef = () => {
    let opt1 = Some("first")
    let opt2 = Some("second")
    let opt3 = None

    switch opt1 {
    | Some(value) => Console.log(value)
    | None => ()
    }
    switch opt2 {
    | Some(value) => Console.log(value)
    | None => ()
    }
    switch opt3 {
    | Some(value) => Console.log(value)
    | None => ()
    }
  }

  let testNonPrimitive = () => {
    let opt = Some("hello")
    Option.forEach(opt, value => Console.log(value))
  }

  let testNonPrimitivePipe = () => {
    let opt = Some("hello")
    opt->Option.forEach(value => Console.log(value))
  }

  let testNonPrimitiveRef = () => {
    let opt = Some("hello")
    switch opt {
    | Some(value) => Console.log(value)
    | None => ()
    }
  }
}

module Map = {
  let testPrimitive = () => {
    let opt = Some(42)
    let result = Option.map(opt, value => value + 1)
    Console.log(result)
  }

  let testPrimitivePipe = () => {
    let opt = Some(42)
    let result = opt->Option.map(value => value + 1)
    Console.log(result)
  }

  let testPrimitiveRef = () => {
    let opt = Some(42)
    let result = switch opt {
    | Some(value) => Some(value + 1)
    | None => None
    }
    Console.log(result)
  }

  let testNone = () => {
    let opt = None
    let result = Option.map(opt, value => globalValue + value)
    Console.log(result)
  }

  let testNonePipe = () => {
    let opt = None
    let result = opt->Option.map(value => globalValue + value)
    Console.log(result)
  }

  let testNoneRef = () => {
    let opt = None
    let result = switch opt {
    | Some(value) => Some(globalValue + value)
    | None => None
    }
    Console.log(result)
  }

  let testQualified = () => {
    let opt = Some("hello")
    let result = Stdlib_Option.map(opt, value => value ++ " world")
    Console.log(result)
  }

  let testQualifiedPipe = () => {
    let opt = Some("hello")
    let result = opt->Stdlib_Option.map(value => value ++ " world")
    Console.log(result)
  }

  let testQualifiedRef = () => {
    let opt = Some("hello")
    let result = switch opt {
    | Some(value) => Some(value ++ " world")
    | None => None
    }
    Console.log(result)
  }

  let testComplex = () => {
    let opt = Some(true)
    let result = Option.map(opt, value =>
      if value {
        "true"
      } else {
        "false"
      }
    )
    Console.log(result)
  }

  let testComplexPipe = () => {
    let opt = Some(true)
    let result = opt->Option.map(value =>
      if value {
        "true"
      } else {
        "false"
      }
    )
    Console.log(result)
  }

  let testComplexRef = () => {
    let opt = Some(true)
    let result = switch opt {
    | Some(value) =>
      Some(
        if value {
          "true"
        } else {
          "false"
        },
      )
    | None => None
    }
    Console.log(result)
  }

  let testNamedCallback = () => {
    let opt = Some(42)
    let add = value => value + 1
    let result = Option.map(opt, add)
    Console.log(result)
  }

  let testNamedCallbackPipe = () => {
    let opt = Some(42)
    let add = value => value + 1
    let result = opt->Option.map(add)
    Console.log(result)
  }

  let testNamedCallbackRef = () => {
    let opt = Some(42)
    let add = value => value + 1
    let result = switch opt {
    | Some(value) => Some(add(value))
    | None => None
    }
    Console.log(result)
  }
}

module FlatMap = {
  let testPrimitive = () => {
    let opt = Some(42)
    let result = Option.flatMap(opt, value => Some(value + 1))
    Console.log(result)
  }

  let testPrimitivePipe = () => {
    let opt = Some(42)
    let result = opt->Option.flatMap(value => Some(value + 1))
    Console.log(result)
  }

  let testPrimitiveRef = () => {
    let opt = Some(42)
    let result = switch opt {
    | Some(value) => Some(value + 1)
    | None => None
    }
    Console.log(result)
  }

  let testNone = () => {
    let opt = None
    let result = Option.flatMap(opt, value => Some(globalValue + value))
    Console.log(result)
  }

  let testNonePipe = () => {
    let opt = None
    let result = opt->Option.flatMap(value => Some(globalValue + value))
    Console.log(result)
  }

  let testNoneRef = () => {
    let opt = None
    let result = switch opt {
    | Some(value) => Some(globalValue + value)
    | None => None
    }
    Console.log(result)
  }

  let testQualified = () => {
    let opt = Some("hello")
    let result = Stdlib_Option.flatMap(opt, value => Some(value ++ " world"))
    Console.log(result)
  }

  let testQualifiedPipe = () => {
    let opt = Some("hello")
    let result = opt->Stdlib_Option.flatMap(value => Some(value ++ " world"))
    Console.log(result)
  }

  let testQualifiedRef = () => {
    let opt = Some("hello")
    let result = switch opt {
    | Some(value) => Some(value ++ " world")
    | None => None
    }
    Console.log(result)
  }

  let testComplex = () => {
    let opt = Some(true)
    let result = Option.flatMap(opt, value =>
      if value {
        Some("true")
      } else {
        None
      }
    )
    Console.log(result)
  }

  let testComplexPipe = () => {
    let opt = Some(true)
    let result = opt->Option.flatMap(value =>
      if value {
        Some("true")
      } else {
        None
      }
    )
    Console.log(result)
  }

  let testComplexRef = () => {
    let opt = Some(true)
    let result = switch opt {
    | Some(value) =>
      if value {
        Some("true")
      } else {
        None
      }
    | None => None
    }
    Console.log(result)
  }

  let testNamedCallback = () => {
    let opt = Some(42)
    let add = value => Some(value + 1)
    let result = Option.flatMap(opt, add)
    Console.log(result)
  }

  let testNamedCallbackPipe = () => {
    let opt = Some(42)
    let add = value => Some(value + 1)
    let result = opt->Option.flatMap(add)
    Console.log(result)
  }

  let testNamedCallbackRef = () => {
    let opt = Some(42)
    let add = value => Some(value + 1)
    let result = switch opt {
    | Some(value) => add(value)
    | None => None
    }
    Console.log(result)
  }
}

describe("Scope preservation in Option optimizations", () => {
  test("Option.forEach evaluates callback argument even when option is None", () => {
    let invocations = ref(0)
    let makeCallback = () => {
      invocations.contents = invocations.contents + 1
      (_value: string) => ()
    }

    Option.forEach(None, makeCallback())

    eq(__LOC__, invocations.contents, 1)
  })

  test("Option.forEach does not shadow surrounding bindings", () => {
    let result = ref(None)

    Option.forEach(
      Some(1),
      value => {
        result.contents = Some(globalValue + value)
      },
    )

    eq(__LOC__, result.contents, Some(globalValue + 1))
  })

  test("Option.map evaluates callback argument even when option is None", () => {
    let invocations = ref(0)
    let makeCallback = () => {
      invocations.contents = invocations.contents + 1
      value => value
    }

    Option.map(None, makeCallback())->ignore

    eq(__LOC__, invocations.contents, 1)
  })

  test("Option.map does not shadow surrounding bindings", () => {
    let result = Option.map(Some(1), value => globalValue + value)

    eq(__LOC__, result, Some(globalValue + 1))
  })

  test("Option.flatMap evaluates callback argument even when option is None", () => {
    let invocations = ref(0)
    let makeCallback = () => {
      invocations.contents = invocations.contents + 1
      value => Some(value)
    }

    Option.flatMap(None, makeCallback())->ignore

    eq(__LOC__, invocations.contents, 1)
  })

  test("Option.flatMap does not shadow surrounding bindings", () => {
    let result = Option.flatMap(Some(1), value => Some(globalValue + value))

    eq(__LOC__, result, Some(globalValue + 1))
  })
})
