type moduleSystem =
  | @as("esmodule") Esmodule
  | @as("commonjs") Commonjs

let parseModuleSystem = value =>
  switch value {
  | "esmodule" | "es6" => Some(Esmodule)
  | "commonjs" | "nodejs" => Some(Commonjs)
  | _ => None
  }

type experimentalFeature = | @as("LetUnwrap") LetUnwrap

let parseExperimentalFeature = value =>
  switch value {
  | "LetUnwrap" => Some(LetUnwrap)
  | _ => None
  }

type sourceMapMode =
  | @as("disabled") Disabled
  | @as("linked") Linked
  | @as("inline") Inline
  | @as("hidden") Hidden

let parseSourceMapMode = value =>
  switch value {
  | "disabled" | "false" | "none" => Some(Disabled)
  | "linked" => Some(Linked)
  | "inline" => Some(Inline)
  | "hidden" => Some(Hidden)
  | _ => None
  }

let sourceMapModeCompilerValue = mode =>
  switch mode {
  | Disabled => "false"
  | Linked => "linked"
  | Inline => "inline"
  | Hidden => "hidden"
  }

type t = {
  compilerVersion: string,
  moduleSystem: moduleSystem,
  warnFlags: string,
  jsxPreserveMode: bool,
  experimentalFeatures: array<experimentalFeature>,
  gentypeEnabled: bool,
  sourceMapMode: sourceMapMode,
  sourceMapSourcesContent: bool,
  sourceMapRoot: string,
}
