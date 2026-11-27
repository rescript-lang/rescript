// A recursive module's signature is typed twice, the first time without
// warnings, so the unused attribute is reported once.
module rec M: {
  @module("./local") external f: @as("v") int => unit = "f"
} = {
  @module("./local") external f: int => unit = "f"
}
