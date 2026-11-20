// Invalid escapes in @as template strings are syntax errors.
type field = {@as(`\xZZ`) x: int}
type constructor = | @as(`\u{110000}`) A | B
type first = {@as(`\01`) @as("a") y: int}
type inline = C({@as(`\xZZ`) z: int})
