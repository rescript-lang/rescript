@module("tag")
external tag: taggedTemplate<string, string> = "default"

let value = "x"
let template = `value: ${value}`
let tagged = tag`value: ${value}`

//^hin
