val migrate :
  entryPointFile:string ->
  outputMode:[`File | `Stdout] ->
  ([`Changed of string | `Unchanged of string], string) result
