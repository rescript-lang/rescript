val migrate :
  ?dependency_paths:Analysis.SharedTypes.FileSet.t ->
  ?package:Analysis.SharedTypes.package ->
  outputMode:[`File | `Stdout] ->
  string ->
  ([`Changed of string | `Unchanged of string], string) result

val filter_deprecations_for_project :
  ?dependency_paths:Analysis.SharedTypes.FileSet.t ->
  ?package:Analysis.SharedTypes.package ->
  deprecated_used:Cmt_utils.deprecated_used list ->
  string ->
  Cmt_utils.deprecated_used list
