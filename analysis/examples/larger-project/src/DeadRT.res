type moduleAccessPath =
  | Root(string)
  | Kaboom

let rec emitModuleAccessPath = moduleAccessPath =>
  switch moduleAccessPath {
  | Root(s) => s
  | Kaboom => ""
  }

let () = Js.log(Kaboom)
