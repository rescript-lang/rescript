@@directive("'use client';")

module Debug = {
  // Relative FFI is also hoisted when there is no .resi.
  @react.component @module("./external-component.js")
  external make: (~children: React.element) => React.element = "default"
}
