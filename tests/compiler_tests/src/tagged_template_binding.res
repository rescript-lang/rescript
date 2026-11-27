// Cross-module provider: a `taggedTemplate` value exported from this module and
// consumed (with backtick syntax) in `Tagged_template_test`. The whole point of
// the first-class type is that the consuming module sees only the *type* of
// `sql` and still emits real tagged-template syntax at the call site.

@module("./tagged_template_lib.js")
external makeSql: string => taggedTemplate<string, string> = "makeSql"

let sql = makeSql("X: ")
