let initialSource: string => promise<string>

let queryCompilerVersion: string => string

let queryModuleSystem: PlaygroundConfig.moduleSystem => PlaygroundConfig.moduleSystem

let queryWarnFlags: string => string

let queryJsxPreserveMode: bool => bool

let queryExperimentalFeatures: unit => array<PlaygroundConfig.experimentalFeature>

let replaceUrlState: (
  ~source: string,
  ~compilerVersion: string,
  ~moduleSystem: PlaygroundConfig.moduleSystem,
  ~warnFlags: string,
  ~jsxPreserveMode: bool,
  ~experimentalFeatures: array<PlaygroundConfig.experimentalFeature>,
) => promise<unit>

let copyUrlState: (
  ~source: string,
  ~compilerVersion: string,
  ~moduleSystem: PlaygroundConfig.moduleSystem,
  ~warnFlags: string,
  ~jsxPreserveMode: bool,
  ~experimentalFeatures: array<PlaygroundConfig.experimentalFeature>,
) => promise<result<unit, string>>
