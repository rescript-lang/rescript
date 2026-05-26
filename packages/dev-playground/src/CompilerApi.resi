module Version: {
  type t = {
    id: string,
    label: string,
  }
}

type info = {
  bundleId: string,
  version: string,
  apiVersion: string,
  moduleSystem: PlaygroundConfig.moduleSystem,
  warnFlags: string,
  jsxPreserveMode: bool,
  experimentalFeatures: array<PlaygroundConfig.experimentalFeature>,
  libraries: array<string>,
}

type success = {
  jsCode: string,
  parsetree: string,
  typedtree: string,
  lambda: string,
  lam: string,
  warnings: array<string>,
  time: float,
}

type failure = {
  errors: array<string>,
  warnings: array<string>,
  message: string,
  time: float,
}

let defaultWarnFlags: string
let defaultCompilerVersion: string
let availableCompilerVersions: array<Version.t>

type compileResult = result<success, failure>
type formatResult = result<string, failure>

let init: string => promise<info>

let compile: (string, PlaygroundConfig.t) => promise<compileResult>

let format: (string, PlaygroundConfig.t) => promise<formatResult>
