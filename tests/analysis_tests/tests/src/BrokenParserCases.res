// --- BROKEN PARSER CASES ---
// This below demonstrates an issue when what you're completing is the _last_ labelled argument, and there's a unit application after it. The parser wrongly merges the unit argument as the expression of the labelled argument assignment, where is should really let the trailing unit argument be, and set a %rescript.exprhole as the expression of the assignment, just like it normally does.
// let _ = someFn(~isOff=, ())
//                      ^com

// This should parse as a single item tuple when in a pattern?
// switch s { | (t) }
//               ^com

// Here the parser eats the arrow and considers the None in the expression part of the pattern.
// let _ = switch x { | None |  => None }
//                           ^com

