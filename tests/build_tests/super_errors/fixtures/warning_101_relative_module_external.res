// A relative @module external is exported as a value, so that other modules
// don't inline it. Its unused attribute is reported once, and a @warning on
// the external applies to it.
@module("./local") external relative: @as("v") int => unit = "relative"

@warning("-101") @module("./local")
external silenced: @as("w") int => unit = "silenced"
