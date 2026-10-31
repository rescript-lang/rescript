// Conversions between Temporal types return the shared types of
// `Temporal.Common`, which complete from the module of each type.
let dateTime = Temporal.PlainDateTime.fromStringOrThrow("2024-02-29T09:30")
let date = dateTime->Temporal.PlainDateTime.toPlainDate
// date->toPlainY
//               ^com
