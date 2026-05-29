type config = {name: string, version: string}
module SubConfig = {
  type t = {version: string}
}

let getVersion = (config: config) =>
  switch config {
  | {name: _, ...SubConfig.t as rest} =>
    rest.version
//  ^def
  }

let {name: _, ...SubConfig.t as localRest} = {name: "v", version: "1"}

//^hin
//^hig
