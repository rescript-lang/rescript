type t
@new @module("bad-words") external make: unit => t = "default"
@send external clean: (t, string) => string = "clean"
