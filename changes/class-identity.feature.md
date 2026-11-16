- A class type (`@instanceOf`) coerces with `:>` to every declaration of its
  class and of its superclasses, in any module or package. Packages can each
  declare the part of a class such as the DOM's `Element` that they use, and
  explicitly coerce values to each other's declarations.
