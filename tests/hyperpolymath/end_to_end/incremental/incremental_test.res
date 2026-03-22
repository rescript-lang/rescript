// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// End-to-end test: Incremental compilation patterns
// Tests that values computed across module boundaries and compile-time
// constants are correctly propagated.

let eq = (a, b) => a == b

// ─── Cross-module constant propagation ──────────────────────────────
module Constants = {
  let maxSize = 1024
  let defaultName = "untitled"
  let version = (1, 2, 3)
  let pi = 3.14159265358979
}

// Constants should be correctly propagated
Test.run(__POS_OF__("incr: const int"), Constants.maxSize, eq, 1024)
Test.run(__POS_OF__("incr: const string"), Constants.defaultName, eq, "untitled")
let (major, minor, patch) = Constants.version
Test.run(__POS_OF__("incr: const tuple"), major * 100 + minor * 10 + patch, eq, 123)

// ─── Module dependency tracking ─────────────────────────────────────
module Base2 = {
  type t = {id: int, name: string}
  let make = (id, name) => {id, name}
  let toString = t => `${Int.toString(t.id)}: ${t.name}`
}

module Extended2 = {
  // Depends on Base2
  let create = name => Base2.make(0, name)
  let withId = (t: Base2.t, id) => Base2.make(id, t.name)
  let describe = t => `Extended(${Base2.toString(t)})`
}

module Consumer = {
  // Depends on both Base2 and Extended2
  let test = () => {
    let item = Extended2.create("test")
    let updated = Extended2.withId(item, 42)
    Extended2.describe(updated)
  }
}

Test.run(
  __POS_OF__("incr: module dep chain"),
  Consumer.test(),
  eq,
  "Extended(42: test)",
)

// ─── Type changes propagation ──────────────────────────────────────
// Verify that type definitions are correctly used by dependents
type status = Active | Inactive | Pending

module StatusUtils = {
  let toString = s =>
    switch s {
    | Active => "active"
    | Inactive => "inactive"
    | Pending => "pending"
    }

  let fromString = s =>
    switch s {
    | "active" => Some(Active)
    | "inactive" => Some(Inactive)
    | "pending" => Some(Pending)
    | _ => None
    }

  let isActive = s => s == Active
}

Test.run(__POS_OF__("incr: type dep toString"), StatusUtils.toString(Active), eq, "active")
Test.run(
  __POS_OF__("incr: type dep fromString"),
  StatusUtils.fromString("pending"),
  eq,
  Some(Pending),
)
Test.run(
  __POS_OF__("incr: type dep fromString none"),
  StatusUtils.fromString("unknown"),
  eq,
  None,
)
Test.run(__POS_OF__("incr: type dep isActive"), StatusUtils.isActive(Active), eq, true)
Test.run(__POS_OF__("incr: type dep not active"), StatusUtils.isActive(Inactive), eq, false)

// ─── Interface boundary testing ─────────────────────────────────────
module type Cacheable = {
  type key
  type value
  let get: key => option<value>
  let set: (key, value) => unit
}

module StringCache: Cacheable with type key = string and type value = string = {
  type key = string
  type value = string
  let store = Dict.make()
  let get = key => Dict.get(store, key)
  let set = (key, value) => Dict.set(store, key, value)
}

StringCache.set("hello", "world")
Test.run(__POS_OF__("incr: interface get"), StringCache.get("hello"), eq, Some("world"))
Test.run(__POS_OF__("incr: interface miss"), StringCache.get("missing"), eq, None)

// ─── Compile-time assertions via type system ────────────────────────
// If these compile, the types are correct
module TypeChecks = {
  // Identity preserves type
  let id: 'a => 'a = x => x

  // Map preserves container, changes element
  let mapOpt: (option<'a>, 'a => 'b) => option<'b> = Option.map

  // Composition type
  let compose: ('b => 'c, 'a => 'b) => 'a => 'c = (f, g) => x => f(g(x))
}

Test.run(__POS_OF__("incr: type check id"), TypeChecks.id(42), eq, 42)
Test.run(
  __POS_OF__("incr: type check mapOpt"),
  TypeChecks.mapOpt(Some(5), x => x * 2),
  eq,
  Some(10),
)
Test.run(
  __POS_OF__("incr: type check compose"),
  TypeChecks.compose(x => x + 1, x => x * 2)(3),
  eq,
  7,
)

// ─── Re-export patterns ─────────────────────────────────────────────
module ReExporter = {
  // Re-export from nested module
  let arrayMap = Array.map
  let arrayFilter = Array.filter
  let arrayReduce = Array.reduce
  let stringLength = String.length
}

Test.run(
  __POS_OF__("incr: re-export map"),
  ReExporter.arrayMap([1, 2, 3], x => x + 1),
  eq,
  [2, 3, 4],
)
Test.run(
  __POS_OF__("incr: re-export filter"),
  ReExporter.arrayFilter([1, 2, 3, 4], x => x > 2),
  eq,
  [3, 4],
)
Test.run(
  __POS_OF__("incr: re-export strlen"),
  ReExporter.stringLength("hello"),
  eq,
  5,
)

// ─── Shared state across modules ────────────────────────────────────
module SharedState = {
  let counter = ref(0)
  let increment = () => {
    counter := counter.contents + 1
    counter.contents
  }
  let reset = () => counter := 0
  let get = () => counter.contents
}

module User1 = {
  let doWork = () => {
    SharedState.increment()
    SharedState.increment()
  }
}

module User2 = {
  let doWork = () => {
    SharedState.increment()
  }
}

SharedState.reset()
User1.doWork()
User2.doWork()
Test.run(__POS_OF__("incr: shared state"), SharedState.get(), eq, 3)
