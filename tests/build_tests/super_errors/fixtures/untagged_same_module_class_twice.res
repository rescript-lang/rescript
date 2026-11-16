@module("three") @instanceOf("Mesh") type mesh
@module("three") @instanceOf("Mesh") type sameMesh

@unboxed
type t = Mesh(mesh) | SameMesh(sameMesh)
