let myFunction = (~name: option<string>=?) => {
  ignore(name)
}
let name = Some("John")
myFunction(~name?)

/* === AVAILABLE ACTIONS:
- RewriteArgType(Optional) - Make argument optional
*/
