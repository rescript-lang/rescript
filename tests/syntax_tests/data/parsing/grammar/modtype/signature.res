module type BT = { let x: int } 
module type BT = {
  let x: int
  let y: int
}

module type BT = @attr { let x: int } 
module type BT = @attr1 @attr2 {
  let x: int
  let y: int
}
