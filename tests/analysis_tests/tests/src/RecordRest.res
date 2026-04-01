type config = {name: string, version: string}
type subConfig = {version: string}

let getVersion = (config: config) =>
  switch config {
  | {name: _, ...subConfig as rest} =>
    rest.version
//  ^def
  }
