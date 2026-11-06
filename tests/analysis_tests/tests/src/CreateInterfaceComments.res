// ^int

/** Top level:
```rescript
let v: t = 1
```
  indented
*/
type top = int

module A = {
  module B = {
    /** Example:
```rescript
let v: t = 1
```
  two spaces
        eight spaces
*/
    type t = int

    // A line comment with "a quote and `a backtick
    type w = {
      /** Field doc
  continued
*/
      a: int,
      /* block
inside */
      b: string,
      /* Nested /* comment
  with "a quote, 'a char and `a backtick
*/ still in the outer comment
*/
      c: bool,
    }

    /** Doc of now
   continued */
    @val external now: unit => float = "Date.now"
  }

  /**
   * Starred doc
   *   indented
   */
  type v = B.t
}

module C = {
      /** Source indented
      deeper */
      type d = {
        // A line comment with "a quote, `a backtick and /* no block
        e: int,
        /* block
        comment */
        f: int,
      }
}
