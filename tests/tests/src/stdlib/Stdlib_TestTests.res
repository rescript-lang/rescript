let eq = (a, b) => a == b

let bign = BigInt.fromFloat(Float.Constants.maxValue)->Option.getOr(0n)
let bign = BigInt.add(bign, bign)

Test.run(__POS_OF__("print null"), Test.print(null), eq, "null")
Test.run(__POS_OF__("print undefined"), Test.print(undefined), eq, "undefined")
Test.run(__POS_OF__("print NaN"), Test.print(nan), eq, "NaN")
Test.run(__POS_OF__("print infinity"), Test.print(infinity), eq, "Infinity")
Test.run(__POS_OF__("print 0"), Test.print(0), eq, "0")
Test.run(__POS_OF__("print int"), Test.print(42), eq, "42")
Test.run(__POS_OF__("print float"), Test.print(4.2), eq, "4.2")
Test.run(__POS_OF__("print string"), Test.print("foo"), eq, `"foo"`)
Test.run(__POS_OF__("print bool"), Test.print(true), eq, "true")
Test.run(__POS_OF__("print object"), Test.print({"x": 42}), eq, `{ x: 42 }`)
Test.run(__POS_OF__("print array"), Test.print([1, 2, 3]), eq, "[ 1, 2, 3 ]")
Test.run(__POS_OF__("print symbol"), Test.print(Symbol.make("foo")), eq, "Symbol(foo)")
Test.run(__POS_OF__("print function"), Test.print(() => 42), eq, "() => 42")
Test.run(__POS_OF__("print es6 function"), Test.print(%raw("() => 42")), eq, "() => 42")
Test.run(
  __POS_OF__("print bigint"),
  Test.print(bign),
  eq,
  "359538626972463141629054847463408713596141135051689993197834953606314521560057077521179117265533756343080917907028764928468642653778928365536935093407075033972099821153102564152490980180778657888151737016910267884609166473806445896331617118664246696549595652408289446337476354361838599762500808052368249716736n",
)
Test.run(__POS_OF__("print set"), Test.print(Set.fromArray([1, 2, 2, 3])), eq, "Set(3) { 1, 2, 3 }")
