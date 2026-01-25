// FIXME:
//   This exists for compatibility reason.
//   Move this into Pervasives or Core

@@deprecated("Use type `string` and `String` module instead. This will be removed in v13")

/** Return the ASCII code of the argument. */
@deprecated("Use type `string` and `String.charCodeAt` instead. This will be removed in v13")
external code: char => int = "%identity"

/** Return the character with the given ASCII code.
   Beware this function is unsafe. */
@deprecated("Use type `string` and `String.fromCharCode` instead. This will be removed in v13")
external chr: int => char = "%identity"

/** Return a string representing the given character,
    with special characters escaped following the lexical conventions
    of OCaml.
    All characters outside the ASCII printable range (32..126) are
    escaped, as well as backslash, double-quote, and single-quote. */
@deprecated("Use type `string` instead. This will be removed in v13")
let escaped: char => string

/** Convert the given character to its equivalent lowercase character,
   using the US-ASCII character set.
   @since 4.03.0 */
@deprecated("Use type `string` and `String.toLowerCase` instead. This will be removed in v13")
let lowercase_ascii: char => char

/** Convert the given character to its equivalent uppercase character,
   using the US-ASCII character set.
   @since 4.03.0 */
@deprecated("Use type `string` and `String.toUpperCase` instead. This will be removed in v13")
let uppercase_ascii: char => char

/** An alias for the type of characters. */
@deprecated("Use type `string` instead. This will be removed in v13")
type t = char

/** The comparison function for characters, with the same specification as
    {!Pervasives.compare}.  Along with the type [t], this function [compare]
    allows the module [Char] to be passed as argument to the functors
    {!Set.Make} and {!Map.Make}. */
@deprecated("Use type `string` and `String.compare` instead. This will be removed in v13")
let compare: (t, t) => int

/** The equal function for chars.
    @since 4.03.0 */
@deprecated("Use type `string` and `String.equal` instead. This will be removed in v13")
let equal: (t, t) => bool

/* The following is for system use only. Do not call directly. */

@deprecated("Use type `string` and `String.fromCharCode` instead. This will be removed in v13")
external unsafe_chr: int => char = "%identity"
