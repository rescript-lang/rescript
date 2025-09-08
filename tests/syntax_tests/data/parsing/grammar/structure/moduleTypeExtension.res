module type A = %modTypeExtension

module type B = %mod.type.extension

module  type C = %mod.type.extension.with.args("argument")

module type D = %mod.type.extension.with.args(x => f(x))


