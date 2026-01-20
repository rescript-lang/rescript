let #...shape = x
let #...\"Shape" = x
let #...\"type" = x
let #...\"test 🏚" = x
let #...\"Shape✅" = x

switch (selectedChoice, value) {
| (#...A.a, #...A.a) => true
| (#...A.b, #...A.b) => true
| (#...A.c, #...A.c) => true
| _ => false
}
