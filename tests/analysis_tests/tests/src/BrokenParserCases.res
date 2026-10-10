// --- BROKEN PARSER CASES ---
// This should parse as a single item tuple when in a pattern?
// switch s { | (t) }
//               ^com

// Here the parser eats the arrow and considers the None in the expression part of the pattern.
// let _ = switch x { | None |  => None }
//                           ^com

