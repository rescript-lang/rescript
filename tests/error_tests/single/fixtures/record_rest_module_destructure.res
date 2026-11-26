module A = Belt.Array

let {push, ...arrayMethods as rest} = module(A)
