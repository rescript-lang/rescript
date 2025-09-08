external clear : (t, int) => unit = "clear"

// multiple primitives
external add_nat: nat => int = "add_nat_bytecode"

@send
external attachShader: (t, ~program: webGlProgram, ~shader: webGlShader) => unit = "attachShader"

// with semicolon
external svg: () => React.element = "svg";
// without semicolon
external svg: () => React.element = "svg"

// should not infinite loop
@new
external createDate: (unit, unit) => date = "Date"

let foobar = createDate()()
