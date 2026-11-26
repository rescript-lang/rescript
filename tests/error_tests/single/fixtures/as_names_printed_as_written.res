// The @as names of constructors and fields print as they are written in
// source, non-ASCII letters and quotes included
module M: {
  type t = | @as("é") A({@as("prénom") name: string}) | @as("a\"b") B
} = {
  type t = | @as("é") A({@as("prénom") name: int}) | @as("a\"b") B
}
