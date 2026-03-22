// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Benchmark: Compilation patterns
// Measures runtime performance of complex code patterns that stress the
// compiler's optimization pipeline.

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

// ─── Large switch statement ─────────────────────────────────────────
type opcode =
  | Add3 | Sub3 | Mul3 | Div3 | Mod3 | Neg3
  | And3 | Or3 | Not3 | Xor3
  | Shl3 | Shr3
  | Eq3 | Ne3 | Lt3 | Le3 | Gt3 | Ge3
  | Load3 | Store3 | Push3 | Pop3
  | Jump3 | JumpIf3 | Call3 | Ret3
  | Nop3 | Halt3
  | Inc3 | Dec3 | Dup3 | Swap3
  | PrintI3 | PrintS3 | PrintF3
  | Alloc3 | Free3 | Copy3
  | Enter3 | Leave3

let opcodeNum = op =>
  switch op {
  | Add3 => 0 | Sub3 => 1 | Mul3 => 2 | Div3 => 3 | Mod3 => 4 | Neg3 => 5
  | And3 => 6 | Or3 => 7 | Not3 => 8 | Xor3 => 9
  | Shl3 => 10 | Shr3 => 11
  | Eq3 => 12 | Ne3 => 13 | Lt3 => 14 | Le3 => 15 | Gt3 => 16 | Ge3 => 17
  | Load3 => 18 | Store3 => 19 | Push3 => 20 | Pop3 => 21
  | Jump3 => 22 | JumpIf3 => 23 | Call3 => 24 | Ret3 => 25
  | Nop3 => 26 | Halt3 => 27
  | Inc3 => 28 | Dec3 => 29 | Dup3 => 30 | Swap3 => 31
  | PrintI3 => 32 | PrintS3 => 33 | PrintF3 => 34
  | Alloc3 => 35 | Free3 => 36 | Copy3 => 37
  | Enter3 => 38 | Leave3 => 39
  }

let opcodes = [Add3, Sub3, Mul3, Load3, Store3, Push3, Call3, Ret3, Nop3, Halt3]
let _ = bench("compile: 40-variant switch", 10000, () => {
  opcodes->Array.map(opcodeNum)->Array.reduce(0, (a, b) => a + b)
})
Test.run(__POS_OF__("bench compile: switch"), opcodeNum(Halt3), eq, 27)

// ─── Deep function composition ──────────────────────────────────────
let compose2 = (f, g) => x => g(f(x))

let pipeline =
  compose2(x => x + 1, compose2(x => x * 2, compose2(x => x - 3, compose2(x => x + 5, x => x * 3))))

let _ = bench("compile: composition chain", 10000, () => {
  pipeline(10)
})
Test.run(__POS_OF__("bench compile: compose"), pipeline(10), eq, ((10 * 3 + 5 - 3) * 2 + 1))

// ─── Complex module hierarchy ───────────────────────────────────────
module Utils = {
  let square = x => x * x
  let clamp = (x, ~min, ~max) =>
    if x < min {
      min
    } else if x > max {
      max
    } else {
      x
    }
}

module Transform = {
  let apply = (arr, fns) =>
    fns->Array.reduce(arr, (acc, f) => acc->Array.map(f))
}

let _ = bench("compile: module hierarchy", 1000, () => {
  let data = Array.fromInitializer(~length=100, i => i)
  Transform.apply(data, [
    x => x + 1,
    Utils.square,
    x => Utils.clamp(x, ~min=0, ~max=10000),
  ])
})
Test.run(__POS_OF__("bench compile: module"), Utils.square(5), eq, 25)

// ─── Heavy stdlib usage ────────────────────────────────────────────
let _ = bench("compile: stdlib-heavy pipeline", 100, () => {
  Array.fromInitializer(~length=1000, i => i)
  ->Array.filter(x => x mod 2 == 0)
  ->Array.map(x => x * x)
  ->Array.filter(x => x > 100)
  ->Array.map(Int.toString)
  ->Array.join(", ")
  ->String.length
})
Test.run(__POS_OF__("bench compile: stdlib"), true, eq, true)

// ─── Complex async/await-like pattern ───────────────────────────────
// Simulated with Result chains
type computeError = ParseErr(string) | CalcErr(string) | FormatErr(string)

let parseNum = s =>
  switch Int.fromString(s) {
  | Some(n) => Ok(n)
  | None => Error(ParseErr(s))
  }

let calculate = n =>
  if n == 0 {
    Error(CalcErr("division by zero"))
  } else {
    Ok(1000 / n)
  }

let format = n =>
  if n < 0 {
    Error(FormatErr("negative"))
  } else {
    Ok(`Result: ${Int.toString(n)}`)
  }

let pipeline2 = s =>
  parseNum(s)->Result.flatMap(calculate)->Result.flatMap(format)

let _ = bench("compile: result pipeline", 10000, () => {
  let _ = pipeline2("42")
  let _ = pipeline2("0")
  let _ = pipeline2("abc")
  pipeline2("10")
})
Test.run(__POS_OF__("bench compile: result"), pipeline2("10"), eq, Ok("Result: 100"))

// ─── Large record operations ────────────────────────────────────────
type config = {
  host: string,
  port: int,
  debug: bool,
  maxRetries: int,
  timeout: float,
  prefix: string,
  suffix: string,
  logLevel: string,
}

let defaultConfig = {
  host: "localhost",
  port: 8080,
  debug: false,
  maxRetries: 3,
  timeout: 30.0,
  prefix: "",
  suffix: "",
  logLevel: "info",
}

let _ = bench("compile: record spread", 10000, () => {
  let c1 = {...defaultConfig, host: "example.com"}
  let c2 = {...c1, port: 3000, debug: true}
  let c3 = {...c2, logLevel: "debug", maxRetries: 5}
  c3.port + c3.maxRetries
})
Test.run(
  __POS_OF__("bench compile: record"),
  {...defaultConfig, port: 3000}.port,
  eq,
  3000,
)

// ─── Closure-heavy code ─────────────────────────────────────────────
let _ = bench("compile: closure creation", 1000, () => {
  let makers = Array.fromInitializer(~length=100, i => {
    let captured = i
    () => captured * captured
  })
  makers->Array.map(f => f())->Array.reduce(0, (a, b) => a + b)
})
Test.run(__POS_OF__("bench compile: closures"), true, eq, true)

// ─── Summary ────────────────────────────────────────────────────────
Console.log("[BENCH] All compile benchmarks completed")
Test.run(__POS_OF__("bench compile: all completed"), true, eq, true)
