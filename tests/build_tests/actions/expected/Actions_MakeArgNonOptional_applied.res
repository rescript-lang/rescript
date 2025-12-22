let myFunction = (~name: string) => {
  ignore(name)
}
let name = "John"
myFunction(~name)

/* === AVAILABLE ACTIONS:
- RewriteArgType(Labelled) - Make argument optional
*/
