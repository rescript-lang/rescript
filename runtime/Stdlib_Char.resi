/***
 Functions for interacting with ASCII char type.
 */

/** 
Type representing an ASCII char.
*/
type t = char

/** Return the ASCII code of the argument. */
external code: char => int = "%identity"

/** Return the character with the given ASCII code.
   Beware this function is unsafe. */
@deprecated("Use `Char.fromIntExn` instead. This will be removed in v13")
external chr: int => char = "%identity"

/** Return a string representing the given character,
    with special characters escaped following the lexical conventions
    of OCaml.
    All characters outside the ASCII printable range (32..126) are
    escaped, as well as backslash, double-quote, and single-quote. */
let escaped: char => string

/** Convert the given character to its equivalent lowercase character,
   using the US-ASCII character set.
   @since 4.03.0 */
@deprecated("Use `Char.toLowerCaseAscii` instead. This will be removed in v13")
let lowercase_ascii: char => char

/** Convert the given character to its equivalent uppercase character,
   using the US-ASCII character set.
   @since 4.03.0 */
@deprecated("Use `Char.toUpperCaseAscii` instead. This will be removed in v13")
let uppercase_ascii: char => char

/** 
Convert the given character to its equivalent lowercase character,
using the US-ASCII character set. 

## Examples

```rescript
Char.toLowerCaseAscii('A')->assertEqual('a')
Char.toLowerCaseAscii('Z')->assertEqual('z')
Char.toLowerCaseAscii('a')->assertEqual('a')
```
*/
let toLowerCaseAscii: char => char

/** 
Convert the given character to its equivalent uppercase character,
using the US-ASCII character set. 

## Examples

```rescript
Char.toUpperCaseAscii('a')->assertEqual('A')
Char.toUpperCaseAscii('z')->assertEqual('Z')
Char.toUpperCaseAscii('A')->assertEqual('A')
```

*/
let toUpperCaseAscii: char => char

/** The comparison function for characters. */
external compare: (char, char) => Stdlib_Ordering.t = "%compare"

/** The equal function for chars. */
external equal: (char, char) => bool = "%equal"

/* The following is for system use only. Do not call directly. */
@deprecated("Use `Char.fromIntUnsafe` instead. This will be removed in v13")
external unsafe_chr: int => char = "%identity"

/** 
Return the character with the given ASCII code.
Beware this function is unsafe. 
*/
external fromIntUnsafe: int => t = "%identity"

/**
Return the character with the given ASCII code.
Raise an `invalid_arg` exception if the argument is not a valid ASCII code.
*/
let fromIntExn: int => t

/**
Return the character with the given ASCII code.
Return `None` if the argument is not a valid ASCII code.
*/
let fromInt: int => option<t>
