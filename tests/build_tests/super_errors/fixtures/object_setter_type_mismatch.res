/* Assignment cannot give a field a type unrelated to its getter: writing
   promotes the field (open row) and unifies the value with the field's one
   type. Under the previous phantom-member encoding this compiled and
   produced a value of declared type int that was "hello" at runtime. */
let breakSoundness = (o: {.."x": int}): int => {
  o["x"] = "hello"
  o["x"]
}
