let add = %raw("(a, b) => a + b")
let template = %raw(`function (a) {
  return "x" + a
}`)
let escaped = %raw(`"a\`b\${c}"`)
let fn = %ffi("(a) => a + 1")
let applied = %ffi(`(a, b) => a * b`)(2, 3)
let attributed = @foo %raw("1")
let leftAsExtension = %raw({"x"})
let bracedEscapes = %raw({"\"a\tb\""})

%%raw("var top = 1")

@attr
%%raw(`
  var multi = 2
`)

/** Doc comment */
%%raw("var documented = 3")
