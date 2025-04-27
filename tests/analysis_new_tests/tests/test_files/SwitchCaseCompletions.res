// == TEST: Empty case, array
let someStringArr = ["hello"]

let x = switch someStringArr {
  | 
//  ^com
}

// == TEST: Empty case, record
let x = switch TestTypeDefs.nestedTestRecord {
  | 
//  ^com
}

// == TEST: Empty case, bool
let x = switch true {
  | 
//  ^com
}
