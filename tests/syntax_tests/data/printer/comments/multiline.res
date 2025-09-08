/* first line
* the leading stars
* should align here.
*/
let f = () => ()

 /* first line
* second line
   * third line */

 /* first line
*
   * third line */

/* x */
/* */
/**/
/*  */

/*
 * test
*/

/* ReScript outperforms a classic for-loop: 
   function equals3(m1, m2) {
    for (var i = 0; i < 4; i++) {
        for (var j = 0; j < 4; j++) {
            var x = m1[i][j];
            var y = m2[i][j];
            if(!floatEquals(x, y)) {
                return false
            }
        }
    }
    return true
  } */
let equals = (matrix1, matrix2) => {
  let rec loop = (i, j) =>
    if i > 3 {
      true
    } else {
      let x = matrix1->getUnsafe(~row=i, ~col=j)
      let y = matrix2->getUnsafe(~row=i, ~col=j)
      if !Float.equals(x, y) {
        false
      } else if j < 3 {
        loop(i, j + 1)
      } else {
        loop(i + 1, 0)
      }
    }
  
  loop(0, 0)
}

/*
foo
bar
*/

/*



foo
bar
*/


/*



foo




bar



*/

/* multi
line
comment
*/


/*
multi
line
comment                  */
