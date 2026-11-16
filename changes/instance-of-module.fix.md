- Read the class of an `@instanceOf` type from its module with `@module("m")`,
  importing it as a `@module` external does, and through the properties of
  `@scope`. Both were silently ignored, so the check read a global of the
  class's name.
