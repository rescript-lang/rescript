let eq = (a, b) => a == b

// ===== BigInt64Array Tests =====

Test.run(__POS_OF__("bytes per element is 8"), BigInt64Array.Constants.bytesPerElement, eq, 8)

let num1 = BigInt.fromStringOrThrow("123456789")
let num2 = BigInt.fromStringOrThrow("987654321")
let num3 = BigInt.fromStringOrThrow("555555555")

let assertTrue = (message, predicate) => {
  try {
    if !predicate() {
      message->JsError.make->JsError.throw
    }
  } catch {
  | _ => message->JsError.make->JsError.throw
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
    message->JsError.make->JsError.throw
  }
}

let areSame = (x: bigint, y: bigint) => BigInt.toString(x) == BigInt.toString(y)

// What's going on here?
// assertTrue("big ints if different then not equal", () => num1 != num2)
// assertTrue("big ints if same then equal", () => num1 == num1)

assertTrue("fromArray", () =>
  [num1, num2]->BigInt64Array.fromArray->BigInt64Array.get(1)->Option.getOrThrow->areSame(num2)
)

assertTrue("fromBuffer", () => {
  let x = ArrayBuffer.make(16)->BigInt64Array.fromBuffer
  x->BigInt64Array.set(1, num2)
  x->BigInt64Array.get(1)->Option.getOrThrow->areSame(num2)
})

assertWillThrow("fromBuffer when too short can throw when used", () => {
  let x = ArrayBuffer.make(1)->BigInt64Array.fromBuffer
  x->BigInt64Array.set(0, num1)
  x->BigInt64Array.get(0)->Option.getOrThrow->areSame(num1)->ignore
})

assertTrue("fromBufferWithRange", () => {
  let x = ArrayBuffer.make(16)->BigInt64Array.fromBuffer(~byteOffset=0, ~length=1)
  x->BigInt64Array.set(0, num1)
  x->BigInt64Array.get(0)->Option.getOrThrow->areSame(num1)
})

// assertWillThrow("testing", () => {"a"->Error.make->Error.raise})
// assertWillThrow("testing", () => {Console.log("f")})

assertWillThrow("fromBufferWithRange is unsafe, out of range", () => {
  let x = ArrayBuffer.make(16)->BigInt64Array.fromBuffer(~byteOffset=13, ~length=1)
  x->BigInt64Array.set(0, num1)
  x->BigInt64Array.get(0)->Option.getOrThrow->areSame(num1)->ignore
})

assertTrue("fromLength is NOT in bytes", () => {
  let x = BigInt64Array.fromLength(1)
  BigInt64Array.byteLength(x) == 8
})

assertTrue("a typed array is a TypedArray.t through a coercion", () => {
  let x = BigInt64Array.fromLength(2)
  TypedArray.length(x :> TypedArray.t<bigint>) == 2
})

let o = (x, map) => BigInt64Array.fromArrayLikeOrIterable(x, ~map)

// ===== copyWithin and map keep the typed array =====

assertTrue("copyAllWithin returns the typed array it mutated", () => {
  let x = Int32Array.fromArray([1, 2, 3])
  let r: Int32Array.t = x->Int32Array.copyAllWithin(~target=1)
  r === x && Int32Array.toString(r) == "1,1,2"
})

assertTrue("copyWithin returns the typed array it mutated", () => {
  let x = Int32Array.fromArray([1, 2, 3, 4])
  let r: Int32Array.t = x->Int32Array.copyWithin(~target=1, ~start=2, ~end=4)
  r === x && Int32Array.toString(r) == "1,3,4,4"
})

assertTrue("TypedArray.copyAllWithin returns the typed array it mutated", () => {
  let x = (Int32Array.fromArray([1, 2, 3]) :> TypedArray.t<int>)
  let r: TypedArray.t<int> = x->TypedArray.copyAllWithin(~target=1)
  r === x && TypedArray.toString(r) == "1,1,2"
})

assertTrue("TypedArray.copyWithin returns the typed array it mutated", () => {
  let x = (Int32Array.fromArray([1, 2, 3, 4]) :> TypedArray.t<int>)
  let r: TypedArray.t<int> = x->TypedArray.copyWithin(~target=1, ~start=2, ~end=4)
  r === x && TypedArray.toString(r) == "1,3,4,4"
})

assertTrue("TypedArray.map keeps the element type", () => {
  let x = (Int32Array.fromArray([1, 2, 3]) :> TypedArray.t<int>)
  let r: TypedArray.t<int> = x->TypedArray.map(v => v * 2)
  let s: TypedArray.t<int> = x->TypedArray.mapWithIndex((v, i) => v + i)
  TypedArray.toString(r) == "2,4,6" && TypedArray.toString(s) == "1,3,5"
})
