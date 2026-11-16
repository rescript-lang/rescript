- A class type (`@instanceOf`) coerces with `:>` to every declaration of its
  class and of its superclasses, in any module or package, and is accepted
  without `:>` as a function argument, a record field (so a JSX prop) or a
  constructor payload where another declaration of its class is expected.
  Packages can then each declare the part of a class such as the DOM's
  `Element` that they use, and pass values to each other.
