/* The occur check rejects a variable unified with a function type that
 contains it. */
let f = (x: 'a) => {
  let y: 'a => int = x
  y
}
