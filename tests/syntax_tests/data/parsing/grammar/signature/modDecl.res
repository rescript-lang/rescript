module type Signature = {
  module Tree: {let x: int}

  module Functor: () => {let x: int}
  module Functor: Tree => Map => {let x: int}
  module Functor: (_ : Foo, _ : Bar) => {let x: int}
  @attr
  module Functor: (_ : Foo, _ : Bar) => {let x: int}

  @attr
  module Tree: {let x: int}
}
