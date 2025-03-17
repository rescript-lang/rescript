module CatchResult = {
  @tag("ok")
  type t<'value> = | @as(true) Ok({value: 'value}) | @as(false) Error({errors: array<string>})
}
