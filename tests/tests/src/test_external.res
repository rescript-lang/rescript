type any /* just pass it through --
 we need find an elegant way to walk around ocaml's type system */

/** It's okay to do this in javascript, you will never get segfault" */
external \"~~": 'a => 'b = "%identity"

type document
@val("document") external doc: unit => document = ""
@val("alert") external alert: string => unit = ""

type v = int => int
@val("ff") external f: string => v = "x"

let xx = doc()

let () = alert("hehha")

let b = f("x")(3)
