// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Benchmark: Code generation quality
// Measures runtime performance of generated JavaScript patterns to verify
// codegen produces efficient output.

let eq = (a, b) => a == b

@val external performanceNow: unit => float = "performance.now"

let bench = (name, iterations, f) => {
  let start = performanceNow()
  for _ in 0 to iterations - 1 {
    ignore(f())
  }
  let elapsed = performanceNow() -. start
  let perIter = elapsed /. Int.toFloat(iterations)
  Console.log(
    `[BENCH] ${name}: ${Float.toFixed(elapsed, ~digits=2)}ms total, ${Float.toFixed(perIter, ~digits=4)}ms/iter (${Int.toString(iterations)} iterations)`,
  )
  elapsed
}

// ─── Array operation efficiency ─────────────────────────────────────
let _ = bench("codegen: array map+filter+reduce", 100, () => {
  Array.fromInitializer(~length=5000, i => i)
  ->Array.map(x => x * 2)
  ->Array.filter(x => x mod 3 == 0)
  ->Array.reduce(0, (a, b) => a + b)
})
Test.run(__POS_OF__("bench codegen: array ops"), true, eq, true)

// ─── Record operation efficiency ────────────────────────────────────
type vec3 = {x: float, y: float, z: float}

let vecAdd = (a: vec3, b: vec3): vec3 => {
  x: a.x +. b.x,
  y: a.y +. b.y,
  z: a.z +. b.z,
}

let vecScale = (v: vec3, s: float): vec3 => {
  x: v.x *. s,
  y: v.y *. s,
  z: v.z *. s,
}

let vecDot = (a: vec3, b: vec3) => a.x *. b.x +. a.y *. b.y +. a.z *. b.z

let vecLength = v => Math.sqrt(vecDot(v, v))

let _ = bench("codegen: vec3 operations", 10000, () => {
  let v1 = {x: 1.0, y: 2.0, z: 3.0}
  let v2 = {x: 4.0, y: 5.0, z: 6.0}
  let sum = vecAdd(v1, v2)
  let scaled = vecScale(sum, 0.5)
  vecLength(scaled)
})
Test.run(
  __POS_OF__("bench codegen: vec3"),
  vecAdd({x: 1.0, y: 0.0, z: 0.0}, {x: 0.0, y: 1.0, z: 0.0}).z < 0.001,
  eq,
  true,
)

// ─── Variant dispatch efficiency ────────────────────────────────────
type expr =
  | Const(float)
  | Add4(expr, expr)
  | Mul4(expr, expr)
  | Neg4(expr)

let rec evalExpr = e =>
  switch e {
  | Const(n) => n
  | Add4(a, b) => evalExpr(a) +. evalExpr(b)
  | Mul4(a, b) => evalExpr(a) *. evalExpr(b)
  | Neg4(a) => -.(evalExpr(a))
  }

let complexExpr =
  Add4(
    Mul4(Const(2.0), Add4(Const(3.0), Const(4.0))),
    Neg4(Mul4(Const(5.0), Const(6.0))),
  )

let _ = bench("codegen: variant dispatch", 10000, () => {
  evalExpr(complexExpr)
})
Test.run(
  __POS_OF__("bench codegen: expr eval"),
  evalExpr(complexExpr) > -16.1 && evalExpr(complexExpr) < -15.9,
  eq,
  true,
)

// ─── Closure call efficiency ────────────────────────────────────────
let _ = bench("codegen: closure calls", 1000, () => {
  let fns = Array.fromInitializer(~length=100, i => {
    let factor = i + 1
    x => x * factor
  })
  fns->Array.map(f => f(10))->Array.reduce(0, (a, b) => a + b)
})
Test.run(__POS_OF__("bench codegen: closures"), true, eq, true)

// ─── String building efficiency ─────────────────────────────────────
let _ = bench("codegen: string building", 100, () => {
  let parts = Array.fromInitializer(~length=1000, i => Int.toString(i))
  parts->Array.join(",")
})
Test.run(__POS_OF__("bench codegen: strings"), true, eq, true)

// ─── Pattern match with many branches ───────────────────────────────
type color =
  | Red3 | Orange3 | Yellow3 | Green3 | Blue3 | Indigo3 | Violet3
  | White3 | Black3 | Gray3 | Brown3 | Pink3 | Cyan3 | Magenta3

let colorToHex = c =>
  switch c {
  | Red3 => "#FF0000"
  | Orange3 => "#FFA500"
  | Yellow3 => "#FFFF00"
  | Green3 => "#00FF00"
  | Blue3 => "#0000FF"
  | Indigo3 => "#4B0082"
  | Violet3 => "#EE82EE"
  | White3 => "#FFFFFF"
  | Black3 => "#000000"
  | Gray3 => "#808080"
  | Brown3 => "#A52A2A"
  | Pink3 => "#FFC0CB"
  | Cyan3 => "#00FFFF"
  | Magenta3 => "#FF00FF"
  }

let colors = [Red3, Green3, Blue3, White3, Black3, Cyan3, Magenta3]
let _ = bench("codegen: 14-variant match", 10000, () => {
  colors->Array.map(colorToHex)
})
Test.run(__POS_OF__("bench codegen: color match"), colorToHex(Red3), eq, "#FF0000")

// ─── Exception handling efficiency ──────────────────────────────────
exception BenchExn(int)

let _ = bench("codegen: try/catch (no throw)", 10000, () => {
  try {
    42
  } catch {
  | BenchExn(_) => 0
  }
})

let _ = bench("codegen: try/catch (with throw)", 1000, () => {
  try {
    raise(BenchExn(42))
  } catch {
  | BenchExn(n) => n
  | _ => 0
  }
})
Test.run(__POS_OF__("bench codegen: exception"), true, eq, true)

// ─── Dict lookup efficiency ─────────────────────────────────────────
let lookupDict = Dict.fromArray(
  Array.fromInitializer(~length=1000, i => (Int.toString(i), i * i)),
)

let _ = bench("codegen: dict lookup 1000", 1000, () => {
  let sum = ref(0)
  for i in 0 to 999 {
    switch Dict.get(lookupDict, Int.toString(i)) {
    | Some(v) => sum := sum.contents + v
    | None => ()
    }
  }
  sum.contents
})
Test.run(__POS_OF__("bench codegen: dict"), Dict.get(lookupDict, "10"), eq, Some(100))

// ─── Summary ────────────────────────────────────────────────────────
Console.log("[BENCH] All codegen benchmarks completed")
Test.run(__POS_OF__("bench codegen: all completed"), true, eq, true)
