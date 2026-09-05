type t = Pair(int, int)

// Before the opening parenthesis
let _ = Pair  (1, 2)
//            ^she

// Whitespace after the constructor
let _ = Pair  (1, 2)
//           ^she

// Comment before arguments
let _ = Pair /* gap */ (1, 2)
//                ^she

// Just inside the opening parenthesis
let _ = Pair /* gap */ (1, 2)
//                      ^she

// Between arguments
let _ = Pair(1, 2)
//             ^she

// Just after the closing parenthesis
let _ = Pair(1, 2)
//                ^she

// After the argument list
let _ = Pair(1, 2) // after
//                 ^she

// Pattern whitespace before arguments
let read = value => switch value { | Pair  (a, b) => a }
//                                         ^she

// Pattern comment before arguments
let read = value => switch value { | Pair /* gap */ (a, b) => a }
//                                             ^she

// Pattern opening parenthesis
let read = value => switch value { | Pair /* gap */ (a, b) => a }
//                                                   ^she

// Pattern between arguments
let read = value => switch value { | Pair(a, b) => a }
//                                          ^she

// After the pattern argument list
let read = value => switch value { | Pair(a, b) => a }
//                                              ^she
