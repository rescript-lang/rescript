// `await` on a module outside a dynamic import has no effect on typing

module M: {
  type t
  let x: t
} = {
  type t = int
  let x = 1
}

// `module type of` doesn't strengthen the module's types
module type T = module type of await M
module N: T = {
  type t = string
  let x = ""
}

// `()` stays the argument a generative functor needs
module G = () => {
  let y = 1
}
module A = G(await {})
let y = A.y
