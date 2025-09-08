let eq = (a, b) => a == b

// ===== is =====

// == Removed when argument types were changed to be the same ==
// Test.run(__POS_OF__("is: different types"), Object.is("abc", false), eq, false)
// Test.run(__POS_OF__("is: null and undefined"), Object.is(null, None), eq, false)
// Test.run(__POS_OF__("is: undefined and None"), Object.is(undefined, None), eq, true)

Test.run(__POS_OF__("is: ints"), Object.is(25, 25), eq, true)

Test.run(__POS_OF__("is: strings"), Object.is("abc", "abc"), eq, true)
Test.run(__POS_OF__("is: strings"), Object.is("abc", "ABC"), eq, false)

Test.run(__POS_OF__("is: null and undefined"), Object.is(null, undefined), eq, false)
Test.run(__POS_OF__("is: null and undefined"), Object.is(undefined, undefined), eq, true)
Test.run(__POS_OF__("is: null and undefined"), Object.is(null, null), eq, true)

let nums = [1, 2, 3]
Test.run(__POS_OF__("is: arrays"), Object.is([1, 2, 3], [1, 2, 3]), eq, false)
Test.run(__POS_OF__("is: arrays"), Object.is(nums, nums), eq, true)
Test.run(__POS_OF__("is: arrays"), [1, 2, 3] == [1, 2, 3], eq, true)
Test.run(__POS_OF__("is: arrays"), [1, 2, 3] === [1, 2, 3], eq, false)

Test.run(__POS_OF__("is: list"), Object.is(list{1, 2, 3}, list{1, 2, 3}), eq, false)
Test.run(__POS_OF__("is: list"), list{1, 2, 3} == list{1, 2, 3}, eq, true)
Test.run(__POS_OF__("is: list"), list{1, 2, 3} === list{1, 2, 3}, eq, false)

let d = Date.makeWithYM(~year=2000, ~month=1)
Test.run(
  __POS_OF__("is: date"),
  Object.is(Date.makeWithYM(~year=2000, ~month=1), Date.makeWithYM(~year=2000, ~month=1)),
  eq,
  false,
)
Test.run(__POS_OF__("is: date"), Object.is(d, d), eq, true)

let x = {"a": 1}
Test.run(__POS_OF__("is: objects"), Object.is(x, x), eq, true)
Test.run(__POS_OF__("is: objects"), Object.is({"a": 1}, {"a": 1}), eq, false)
Test.run(__POS_OF__("is: objects"), Object.is(Object.make(), Object.make()), eq, false) // hmm...
Test.run(__POS_OF__("is: === and == operator"), x === x, eq, true)
Test.run(__POS_OF__("is: === and == operator"), x == x, eq, true)
Test.run(__POS_OF__("is: === and == operator"), {"a": 1} == {"a": 1}, eq, true)

Test.run(__POS_OF__("is: zeros"), Object.is(-0, -0), eq, true)
Test.run(__POS_OF__("is: zeros"), Object.is(-0.0, -0.0), eq, true)
Test.run(__POS_OF__("is: zeros"), Object.is(0.0, -0.0), eq, false)

let mkBig = s => BigInt.fromString(s)
Test.run(__POS_OF__("is: bigint"), Object.is(mkBig("123456789"), mkBig("123456789")), eq, true)
Test.run(__POS_OF__("is: bigint"), Object.is(mkBig("123489"), mkBig("123456789")), eq, false)
Test.run(__POS_OF__("is: bigint"), Object.is(mkBig("000000000"), mkBig("0")), eq, true)
Test.run(__POS_OF__("is: bigint"), mkBig("123") == mkBig("123"), eq, true)
Test.run(__POS_OF__("is: bigint"), mkBig("123") === mkBig("123"), eq, true)

// ====== assign ======

Test.run(
  __POS_OF__("assign copies from source to target"),
  Object.assign({"a": 1, "b": 2}, {"b": 3, "c": 0}),
  eq,
  {"a": 1, "b": 3, "c": 0},
)

let assignOverwritesTarget = (~title, ~source) => {
  let sourceObj = {"a": source}
  Test.run(__POS_OF__(`assign ${title}`), Object.assign({"a": 1}, sourceObj), eq, sourceObj)
  Test.run(__POS_OF__(`assign ${title}`), Object.assign({"a": undefined}, sourceObj), eq, sourceObj)
  Test.run(__POS_OF__(`assign ${title}`), Object.assign({"a": null}, sourceObj), eq, sourceObj)
}

assignOverwritesTarget(~title="when source is undefined", ~source=undefined)
assignOverwritesTarget(~title="when source is null", ~source=null)
assignOverwritesTarget(~title="when source is a number", ~source=1)
assignOverwritesTarget(~title="when source is a string", ~source="abc")

// ===== get =====

type getTestData<'obj, 'res, 'expected> = {
  title: string,
  source: unit => 'obj,
  get: 'obj => 'res,
  expected: 'expected,
}

let runGetTest = i =>
  Test.run(__POS_OF__(`Object.get: ${i.title}`), i.source()->i.get, eq, i.expected)

{
  title: "prop exists, return Some",
  source: () => {"a": 1},
  get: Object.get(_, "a"),
  expected: Some(1),
}->runGetTest

{
  title: "prop NOT exist, return None",
  source: () => {"a": 1},
  get: i => i->Object.get("banana"),
  expected: None,
}->runGetTest

{
  title: "prop like toString, return Some",
  source: () => {"a": 1},
  get: i => i->Object.get("toString")->Option.isSome,
  expected: true,
}->runGetTest

{
  title: "prop exist but explicitly undefined, return None",
  source: () => {"a": undefined},
  get: i => i->Object.get("a"),
  expected: None,
}->runGetTest

{
  title: "prop exist but explicitly null, return None",
  source: () => {"a": null},
  get: i => i->Object.get("a"),
  expected: Some(null),
}->runGetTest

{
  title: "prop exists and is an array, can get it",
  source: () => {"a": [1, 2, 3]},
  get: i => i->Object.get("a")->Option.map(i => i->Array.concat([4, 5]))->Option.getOr([]),
  expected: [1, 2, 3, 4, 5],
}->runGetTest

// This throws an exception
// {
//   title: "prop exists but casted wrong on get",
//   source: () => {"a": 34},
//   get: i => i->Object.get("a")->Option.map(i => i->Array.concat([4, 5]))->Option.getWithDefault([]),
//   expected: [],
// }->runGetTest

// ===== getSymbol =====

let getSymbolTestWhenExists = () => {
  let obj = Object.make()
  let fruit = Symbol.make("fruit")
  obj->Object.setSymbol(fruit, "banana")
  let retrieved = obj->Object.getSymbol(fruit)
  Test.run(
    __POS_OF__(`Object.getSymbol when exists return it as Some`),
    retrieved,
    eq,
    Some("banana"),
  )
}
getSymbolTestWhenExists()

Test.run(
  __POS_OF__(`Object.getSymbol when not exists return it as None`),
  Object.make()->Object.getSymbol(Symbol.make("fruit")),
  eq,
  None,
)

// ===== create =====

Test.run(
  __POS_OF__(`Object.create clones properties`),
  {"a": 1}->Object.create->Object.get("a"),
  eq,
  Some(1),
)

Test.run(
  __POS_OF__(`Object.create clones properties`),
  {"a": 1}->Object.create->Object.get("b"),
  eq,
  None,
)
