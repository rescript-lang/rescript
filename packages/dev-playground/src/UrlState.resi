type state = {
  source: string,
  config: PlaygroundConfig.t,
}

let init: (
  ~defaultSource: string,
  ~defaultConfig: PlaygroundConfig.t,
  ~availableCompilerVersions: array<CompilerApi.Version.t>,
) => promise<state>

let replace: (~source: string, ~config: PlaygroundConfig.t) => promise<unit>

let copy: (~source: string, ~config: PlaygroundConfig.t) => promise<result<unit, string>>
