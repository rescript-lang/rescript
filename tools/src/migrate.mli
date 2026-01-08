val migrate :
  outputMode:[`File | `Stdout] ->
  ?package:Analysis.SharedTypes.package ->
  string ->
  ([`Changed of string | `Unchanged of string], string) result

val filter_deprecations_for_project :
  entryPointFile:string ->
  ?package:Analysis.SharedTypes.package ->
  Cmt_utils.deprecated_used list ->
  Cmt_utils.deprecated_used list
