// == TEST: Record field completion in nested record
let x = TestTypeDefs.nestedTestRecord.
//                                    ^crm

// == TEST: Record field completion in nested record, another level
let x = TestTypeDefs.nestedTestRecord.nested.
//                                           ^crm