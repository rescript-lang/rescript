@genType
let testImmutableArrayGet = arr => {
  ImmutableArray.Array.get(arr, 3)
}

/*
   type error
   let testImmutableArraySet = arr => ImmutableArray.(arr[3] = 4);
 */

let testBeltArrayGet = arr => {
  Belt.Array.get(arr, 3)
}

let testBeltArraySet = arr => {
  Belt.Array.set(arr, 3, 4)
}

