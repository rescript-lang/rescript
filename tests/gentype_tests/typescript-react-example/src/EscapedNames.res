@genType
type variant = | @as("Illegal\"Name") IllegalName

@genType
type polymorphicVariant = [#"Illegal\"Name"]

@genType
type object_ = {"normalField": int, "escape\"me": int}

@genType
type record = {
  normalField: variant,
  @as("Renamed'Field") renamedField: int,
  \"Illegal-field name": int,
}
@genType
let myRecord = {
  normalField: IllegalName,
  renamedField: 42,
  \"Illegal-field name": 7,
}
