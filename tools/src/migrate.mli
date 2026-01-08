val migrate :
  ?package:Analysis.SharedTypes.package ->
  ?dependency_paths:Analysis.SharedTypes.FileSet.t ->
  outputMode:[`File | `Stdout] ->
  string ->
  ([`Changed of string | `Unchanged of string], string) result

val filter_deprecations_for_project :
  ?package:Analysis.SharedTypes.package ->
  ?dependency_paths:Analysis.SharedTypes.FileSet.t ->
  entryPointFile:string ->
  deprecated_used:Cmt_utils.deprecated_used list ->
  Cmt_utils.deprecated_used list
