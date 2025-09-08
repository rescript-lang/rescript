@ns.doc("  Doc comment with a triple-backquote example
  
  ```res example
    let a = 10
    /*
     * stuff
     */
  ```
")
let docComment1 = 12
//       ^hov

/**
  Doc comment with a triple-backquote example
  
  ```res example
    let a = 10
    /*
     * stuff
     */
  ```
*/
let docComment2 = 12
//    ^hov


@ns.doc("  Doc comment with a triple-backquote example
  
  ```res example
    let a = 10
    let b = 20
  ```
")
let docCommentNoNested1 = 12
//       ^hov

/**
  Doc comment with a triple-backquote example
  
  ```res example
    let a = 10
    let b = 20
  ```
*/
let docCommentNoNested2 = 12
//    ^hov

@res.doc("New doc comment format")
let newDoc = 10
//   ^hov