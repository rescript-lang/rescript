/* @scope should not be shadowed by a module with the same name. */
module URL = {
  @val @scope("URL")
  external revokeObjectURL: string => unit = "revokeObjectURL"
}

URL.revokeObjectURL("some url") /* expect `globalThis.URL.revokeObjectURL(...)` */

module MyURL = URL

MyURL.revokeObjectURL("some url") /* expect `globalThis.URL.revokeObjectURL(...)` */
