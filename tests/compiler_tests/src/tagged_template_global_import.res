// Codegen-only fixture (not a `*_test` file, so it is compiled but never run).
// It pins down the generated JS for a tag bound to a *bare package specifier*
// (`@module("postgres")`) — as opposed to a relative `./file.js` path — proving
// the import statement and the real tagged-template call site are emitted
// correctly. The committed `.mjs` snapshot is the assertion.

type queryResult = {rows: array<string>}

// The `postgres` default export is a factory whose return value *is* the tag.
@module("postgres")
external postgres: string => taggedTemplate<string, promise<queryResult>> = "default"

// Construct the tag from the bare-imported factory.
let makeSql = url => postgres(url)

// Use it with backtick syntax — emits a real `sql`...`` tagged template against
// the bare-package import.
let findUser = (sql: taggedTemplate<string, promise<queryResult>>, id) =>
  sql`SELECT * FROM users WHERE id = ${id}`
