// A relative @module external is also kept in its resolved form, for the
// interface that stops other modules from inlining it. The unused attribute
// is reported once.
@module("./local") external relative: (@as("v") int) => unit = "relative"
