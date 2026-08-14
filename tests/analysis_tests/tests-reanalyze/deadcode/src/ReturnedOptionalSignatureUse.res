let useInterfaceDeclaration = () =>
  ReturnedOptionalSignature.returnedOptional("value")(~inner="used")

module InlineSignature: {
  let returnedOptional: (~outer: string=?, string) => (~inner: string=?) => string
} = {
  let returnedOptional = (~outer=?, value) => (~inner=?) => value
}

let useInlineSignature = () =>
  InlineSignature.returnedOptional("value")(~inner="used")

let _ = useInterfaceDeclaration()
let _ = useInlineSignature()
