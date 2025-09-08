@genType
let testImmutableArrayGet = arr => {
  open ImmutableArray
  arr[3]
}

/*
   type error
   let testImmutableArraySet = arr => ImmutableArray.(arr[3] = 4);
 */

let testBeltArrayGet = arr => {
  open Belt
  arr[3]
}

let testBeltArraySet = arr => {
  open Belt
  arr[3] = 4
}

