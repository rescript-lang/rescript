type moduleSystem =
  | @as("esmodule") Esmodule
  | @as("commonjs") Commonjs

let parseModuleSystem = value =>
  switch value {
  | "esmodule" | "es6" => Some(Esmodule)
  | "commonjs" | "nodejs" => Some(Commonjs)
  | _ => None
  }

type experimentalFeature = @as("LetUnwrap") LetUnwrap

let parseExperimentalFeature = value =>
  switch value {
  | "LetUnwrap" => Some(LetUnwrap)
  | _ => None
  }
