a[ /* zz */ 0 ] =  7 

let _ = (
  /* zz */ a  
  )[0]

let _ = (
  a // zz
  )[0]

 (
    incidents
    ->Belt.Array.keep(({status}) => status === #OPEN)
    // This comment will vanish 
    ->Belt.SortArray.stableSortBy((a, b) =>
      compare(a.createdTime, b.createdTime)
    )
  )[0]

let _ = [
  // comment 1
  a,
  // comment 2
  b,
  // comment 3
  c
]

let _ = [
  // comment 1
  a,
  // comment 2
  b, c
]

let _ = [
  // comment 0
  ...xs,
  // comment 1
  a,
  // comment 2
  b, c
]

let _ = [
  // comment 0
  ...xs,
  // comment 1
  a,
  // comment 2
  ...ys,
  // comment 3
  b, c
]
