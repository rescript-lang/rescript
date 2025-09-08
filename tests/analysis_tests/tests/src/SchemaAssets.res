@live
type rec input_ByAddress = {city: string}
@tag("__$inputUnion")
and input_Location =
  | @as("byAddress") ByAddress(input_ByAddress)
  | @as("byId") ById(string)
