let myFunction = (~name: option<string>=?) => {
  ignore(name)
}
let name = Some("John")
myFunction(~name)
