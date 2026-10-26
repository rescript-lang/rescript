let eq = (a, b) => a == b

// ===== BigInt64Array Tests =====

Test.run(__POS_OF__("bytes per element is 8"), BigInt64Array.Constants.bytesPerElement, eq, 8)

let num1 = BigInt.fromStringOrThrow("123456789")
let num2 = BigInt.fromStringOrThrow("987654321")
let num3 = BigInt.fromStringOrThrow("555555555")

let assertTrue = (message, predicate) => {
  try {
    if !predicate() {
      message->Error.make->Error.raise
    }
  } catch {
  | _ => message->Error.make->Error.raise
  }
}

let assertWillThrow = (message, f) => {
  let didThrow = ref(false)
  try {
    f()
  } catch {
  | _ => didThrow := true
  }
  if didThrow.contents == false {
    message->Error.make->Error.raise
  }
}

let areSame = (x: bigint, y: bigint) => BigInt.toString(x) == BigInt.toString(y)

// What's going on here?
// assertTrue("big ints if different then not equal", () => num1 != num2)
// assertTrue("big ints if same then equal", () => num1 == num1)

assertTrue("fromArray", () =>
  [num1, num2]->BigInt64Array.fromArray->BigInt64Array.get(1)->Option.getExn->areSame(num2)
)

assertTrue("fromBuffer", () => {
  let x = ArrayBuffer.make(16)->BigInt64Array.fromBuffer
  x->BigInt64Array.set(1, num2)
  x->BigInt64Array.get(1)->Option.getExn->areSame(num2)
})

assertWillThrow("fromBuffer when too short can throw when used", () => {
  let x = ArrayBuffer.make(1)->BigInt64Array.fromBuffer
  x->BigInt64Array.set(0, num1)
  x->BigInt64Array.get(0)->Option.getExn->areSame(num1)->ignore
})

assertTrue("fromBufferWithRange", () => {
  let x = ArrayBuffer.make(16)->BigInt64Array.fromBufferWithRange(~byteOffset=0, ~length=1)
  x->BigInt64Array.set(0, num1)
  x->BigInt64Array.get(0)->Option.getExn->areSame(num1)
})

// assertWillThrow("testing", () => {"a"->Error.make->Error.raise})
// assertWillThrow("testing", () => {Console.log("f")})

assertWillThrow("fromBufferWithRange is unsafe, out of range", () => {
  let x = ArrayBuffer.make(16)->BigInt64Array.fromBufferWithRange(~byteOffset=13, ~length=1)
  x->BigInt64Array.set(0, num1)
  x->BigInt64Array.get(0)->Option.getExn->areSame(num1)->ignore
})

assertTrue("fromLength is NOT in bytes", () => {
  let x = BigInt64Array.fromLength(1)
  BigInt64Array.byteLength(x) == 8
})

assertTrue("a typed array is a TypedArray.t through a coercion", () => {
  let x = BigInt64Array.fromLength(2)
  TypedArray.length(x :> TypedArray.t<bigint>) == 2
})
let o = BigInt64Array.fromArrayLikeOrIterableWithMap
