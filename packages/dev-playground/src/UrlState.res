@module("./UrlState.js") external initialSource: string => promise<string> = "initialSource"
@module("./UrlState.js") external queryCompilerVersion: string => string = "queryCompilerVersion"
@module("./UrlState.js") external queryModuleSystem: string => string = "queryModuleSystem"
@module("./UrlState.js") external queryWarnFlags: string => string = "queryWarnFlags"
@module("./UrlState.js") external queryJsxPreserveMode: bool => bool = "queryJsxPreserveMode"
@module("./UrlState.js") external queryExperimentalFeatures: unit => array<string> = "queryExperimentalFeatures"
@module("./UrlState.js")
external replaceUrlState: (
  string,
  string,
  string,
  string,
  bool,
  array<string>,
) => promise<unit> = "replaceUrlState"
@module("./UrlState.js")
external copyUrlState: (
  string,
  string,
  string,
  string,
  bool,
  array<string>,
) => promise<string> = "copyUrlState"
