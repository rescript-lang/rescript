@module("node:test")
external test: (string, unit => promise<unit>) => unit = "test"
