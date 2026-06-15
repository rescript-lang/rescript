type state = {
  source: string,
  config: PlaygroundConfig.t,
}

let init: (~defaultSource: string) => promise<state>

let replace: (~source: string, ~config: PlaygroundConfig.t) => promise<unit>

let copy: (~source: string, ~config: PlaygroundConfig.t) => promise<result<unit, string>>
