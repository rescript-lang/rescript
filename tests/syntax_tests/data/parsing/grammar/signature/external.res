module type Signature = {
  type t

  @send
  external linkProgram: (t, ~program: webGlProgram) => unit = "linkProgram"

  external add_nat: (nat, int, int) => int = "add_nat_bytecode"


  // with semicolon
  external svg: () => React.element = "svg";
  // without semicolon
  external svg: () => React.element = "svg"
}
