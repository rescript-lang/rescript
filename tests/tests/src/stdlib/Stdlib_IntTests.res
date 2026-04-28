let eq = (a, b) => a == b

let catch = f =>
  try {
    let _ = f()
    failwith("no exception raised")
  } catch {
  | JsExn(err) => err
  }

Test.run(__POS_OF__("range - positive, increasing"), Int.range(3, 6), eq, [3, 4, 5])
Test.run(__POS_OF__("range - negative, increasing"), Int.range(-3, -1), eq, [-3, -2])
Test.run(__POS_OF__("range - cross-zero, increasing"), Int.range(-1, 2), eq, [-1, 0, 1])
Test.run(__POS_OF__("range - start == end"), Int.range(3, 3), eq, [])
Test.run(__POS_OF__("range - positive, decreasing"), Int.range(3, 1), eq, [3, 2])
Test.run(__POS_OF__("range - negative, decreasing"), Int.range(-1, -3), eq, [-1, -2])

Test.run(
  __POS_OF__("range - positive, increasing, step 2"),
  Int.range(3, 6, ~options={step: 2}),
  eq,
  [3, 5],
)
Test.run(
  __POS_OF__("range + positive, increasing, step 2"),
  Int.range(3, 7, ~options={step: 2}),
  eq,
  [3, 5],
)
Test.run(
  __POS_OF__("range + positive, increasing, step 2"),
  Int.range(3, 8, ~options={step: 2}),
  eq,
  [3, 5, 7],
)
Test.run(
  __POS_OF__("range - negative, increasing, step 2"),
  Int.range(-6, -3, ~options={step: 2}),
  eq,
  [-6, -4],
)
Test.run(
  __POS_OF__("range - positive, increasing, step 0"),
  catch(() => Int.range(3, 6, ~options={step: 0})),
  eq,
  JsError.RangeError.make("Incorrect range arguments")->JsError.toJsExn,
)
Test.run(__POS_OF__("range - start == end, step 0"), Int.range(3, 3, ~options={step: 0}), eq, [])
Test.run(
  __POS_OF__("range + positive, increasing, step -1"),
  Int.range(3, 6, ~options={step: -1}),
  eq,
  [],
)
Test.run(
  __POS_OF__("range + positive, decreasing, step 1"),
  Int.range(6, 3, ~options={step: 1}),
  eq,
  [],
)
Test.run(
  __POS_OF__("range + positive, decreasing, step -2"),
  Int.range(6, 3, ~options={step: -2}),
  eq,
  [6, 4],
)
Test.run(
  __POS_OF__("range + positive, increasing, step -2"),
  Int.range(6, 2, ~options={step: -2}),
  eq,
  [6, 4],
)
Test.run(
  __POS_OF__("range + positive, increasing, step -2"),
  Int.range(6, 1, ~options={step: -2}),
  eq,
  [6, 4, 2],
)
Test.run(
  __POS_OF__("range + negative, decreasing, step -2"),
  Int.range(-3, -6, ~options={step: -2}),
  eq,
  [-3, -5],
)
Test.run(
  __POS_OF__("range - positive, increasing, step 2, inclusive"),
  Int.range(3, 6, ~options={step: 2, inclusive: true}),
  eq,
  [3, 5],
)
Test.run(
  __POS_OF__("range + positive, increasing, step 2, inclusive"),
  Int.range(3, 7, ~options={step: 2, inclusive: true}),
  eq,
  [3, 5, 7],
)
Test.run(
  __POS_OF__("range + positive, increasing, step 2, inclusive"),
  Int.range(3, 8, ~options={step: 2, inclusive: true}),
  eq,
  [3, 5, 7],
)
Test.run(
  __POS_OF__("range - negative, increasing, step 2, inclusive"),
  Int.range(-6, -3, ~options={step: 2, inclusive: true}),
  eq,
  [-6, -4],
)
Test.run(
  __POS_OF__("range - positive, increasing, step 0, inclusive"),
  catch(() => Int.range(3, 6, ~options={step: 0, inclusive: true})),
  eq,
  JsError.RangeError.make("Incorrect range arguments")->JsError.toJsExn,
)
Test.run(
  __POS_OF__("range - start == end, step 0, inclusive"),
  Int.range(3, 3, ~options={step: 0, inclusive: true}),
  eq,
  [3],
)
Test.run(
  __POS_OF__("range + positive, increasing, step -1, inclusive"),
  Int.range(3, 6, ~options={step: -1, inclusive: true}),
  eq,
  [],
)
Test.run(
  __POS_OF__("range + positive, decreasing, step 1, inclusive"),
  Int.range(6, 3, ~options={step: 1, inclusive: true}),
  eq,
  [],
)
Test.run(
  __POS_OF__("range + positive, decreasing, step -2, inclusive"),
  Int.range(6, 3, ~options={step: -2, inclusive: true}),
  eq,
  [6, 4],
)
Test.run(
  __POS_OF__("range + positive, increasing, step -2, inclusive"),
  Int.range(6, 2, ~options={step: -2, inclusive: true}),
  eq,
  [6, 4, 2],
)
Test.run(
  __POS_OF__("range + positive, increasing, step -2, inclusive"),
  Int.range(6, 1, ~options={step: -2, inclusive: true}),
  eq,
  [6, 4, 2],
)
Test.run(
  __POS_OF__("range + negative, decreasing, step -2, inclusive"),
  Int.range(-3, -6, ~options={step: -2, inclusive: true}),
  eq,
  [-3, -5],
)

Test.run(__POS_OF__("clamp"), Int.clamp(42), eq, 42)
Test.run(__POS_OF__("clamp - < min"), Int.clamp(~min=50, 42), eq, 50)
Test.run(__POS_OF__("clamp - > min"), Int.clamp(~min=40, 42), eq, 42)
Test.run(__POS_OF__("clamp - < max"), Int.clamp(~max=50, 42), eq, 42)
Test.run(__POS_OF__("clamp - > max"), Int.clamp(~max=40, 42), eq, 40)
Test.run(__POS_OF__("clamp - < min, < max"), Int.clamp(~min=50, ~max=60, 42), eq, 50)
Test.run(__POS_OF__("clamp - < min, > max"), Int.clamp(~min=50, ~max=40, 42), eq, 50) // min wins
Test.run(__POS_OF__("clamp - > min, < max"), Int.clamp(~min=40, ~max=60, 42), eq, 42)
Test.run(__POS_OF__("clamp - > min, > max"), Int.clamp(~min=40, ~max=40, 42), eq, 40)

Test.run(__POS_OF__("Int.equal optimization"), Int.equal(1, 3), eq, false)
