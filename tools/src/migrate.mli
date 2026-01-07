val migrate :
  entryPointFile:string ->
  outputMode:[`File | `Stdout] ->
  ([`Changed of string | `Unchanged of string], string) result

val filter_deprecations_for_project :
  entryPointFile:string ->
  Cmt_utils.deprecated_used list ->
  Cmt_utils.deprecated_used list
