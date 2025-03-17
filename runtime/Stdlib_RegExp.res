type t

module Result = {
  type t = array<option<string>>
  @get_index external fullMatch: (t, @as(0) _) => string = ""
  @send external matches: (t, @as(1) _) => array<string> = "slice"
  @get external index: t => int = "index"
  @get external input: t => string = "input"
}

@new external fromString: string => t = "RegExp"
@new external fromStringWithFlags: (string, ~flags: string) => t = "RegExp"

@send external test: (t, string) => bool = "test"
@return(nullable) @send external exec: (t, string) => option<Result.t> = "exec"

@get external lastIndex: t => int = "lastIndex"
@set external setLastIndex: (t, int) => unit = "lastIndex"
@get external ignoreCase: t => bool = "ignoreCase"
@get external global: t => bool = "global"
@get external multiline: t => bool = "multiline"
@get external source: t => string = "source"
@get external sticky: t => bool = "sticky"
@get external unicode: t => bool = "unicode"
