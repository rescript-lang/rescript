@module("three") @instanceOf("Mesh") type mesh
@module("three") @instanceOf("Mesh") type sameMesh

@unboxed type t = A(mesh) | B(sameMesh)
