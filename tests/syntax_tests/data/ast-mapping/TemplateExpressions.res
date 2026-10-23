// Round-trip coverage for ordinary and tagged templates through the
// Parsetree0 bridge (ast_mapper_to0 / ast_mapper_from0).

let value = "world"
let ordinary = `hello ${value}\n`
let tagged = tag`raw \unicode ${value}\x61`
