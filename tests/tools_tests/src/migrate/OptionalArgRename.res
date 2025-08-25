module Target = {
  let doStuff = (~newName: option<int>=?) => ()
}

external doStuff: (~oldName: option<int>=?) => unit = "doStuff"

/* Intentionally no usage here; this test exercises migration config parsing and ensures optional label rename is preserved and doesn’t crash. */
