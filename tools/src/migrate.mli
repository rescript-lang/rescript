val migrate :
  entry_point_file:string ->
  output_mode:[`File | `Stdout] ->
  (string, string) result
