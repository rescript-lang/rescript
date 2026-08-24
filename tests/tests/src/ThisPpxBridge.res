@@config({flags: ["-bs-test-ast-conversion"]})

// The whole file round-trips through the frozen Parsetree0 external-PPX
// bridge before compiling. @this must survive the round trip as a
// function-node attribute or this compiles to a plain function.
let methodThroughPpxBridge = @this this => 1 + this
