module InnerModule: {
  type t = private string
  @genType
  let make: t => string
} = {
  type t = string
  let make = t => t ++ "..."
}
