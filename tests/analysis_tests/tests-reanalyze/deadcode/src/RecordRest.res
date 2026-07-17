type config = {name: string, version: string}

module SubConfig = {
  type t = {version: string}
}

@live
let getRest = (config: config) =>
  switch config {
  | {name: _, ...SubConfig.t as rest} => rest
  }
