// == TEST: Empty case, array
let someStringArr = ["hello"]

let x = switch someStringArr {
  | 
//  ^crm
}

// == TEST: Empty case, record
let x = switch TestTypeDefs.nestedTestRecord {
  | 
//  ^crm
}

// == TEST: Empty case, bool
let x = switch true {
  | 
//  ^crm
}
