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

let getVersionFromParam = ({name: _, ...SubConfig.t as paramRest}: config) => {
  // param
  //      ^com
  paramRest.version
}

let {name: _, ...SubConfig.t as localRest} = {name: "v", version: "1"}
//                 ^ast

let {...SubConfig.t as wholeRest} = {version: "2"}
//       ^com

//^hin
//^hig
