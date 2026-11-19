// Z is named only in @throws. The type checker looks up Z.E, so Z must be
// built first.
@throws(Z.E)
external find: int => int = "find"
