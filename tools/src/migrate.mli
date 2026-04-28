val migrate :
  entryPointFile:string ->
  outputMode:[`File | `Stdout] ->
  (string, string) result
