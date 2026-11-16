// Only a global must be an identifier that is not a keyword: the other
// properties of @scope are read with a computed access when they need one.

type t

// Properties after the global
@val @scope(("lib", "foo-bar", "class")) external afterGlobal: int = "afterGlobal"

// A global property that no identifier can name
@val @scope(("globalThis", "foo-bar")) external onGlobalThis: int = "onGlobalThis"

// `import.meta` starts with a keyword, but is no global
@val @scope(("import", "meta")) external url: string = "url"

// Properties of a module
@module("lib") @scope(("foo-bar", "class")) external ofModule: int = "ofModule"
@module("lib") @new @scope("foo-bar") external make: unit => t = "Klass"

// Properties of the object an external is called on
@send @scope(("class", "foo-bar")) external send: (t, int) => unit = "send"
@get @scope("foo-bar") external get: t => int = "get"
@set @scope("class") external set: (t, int) => unit = "set"
@get_index @scope("foo-bar") external getIndex: (t, int) => int = ""

let values = () => (afterGlobal, onGlobalThis, url, ofModule)

let calls = t => {
  send(make(), get(t))
  set(t, getIndex(t, 0))
}
