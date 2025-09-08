let f = (. a, b) => a + b
let f = (. a, . b) => a + b
let f = (. a, b, . c, d) => a + b + c + d

let f = @attr (. a) => @attr2 b => @attr3 (. c) => @attr4 d => ()
let f = (. @attr a, @attr2 b, . @attr3 c, @attr4 d) => ()
let f = (. (@attr a), (@attr2 b), . (@attr3 c), (@attr4 d)) => ()

add(. 1, 2)
add(. 2, 3, 4, . 5, 6, 7, . 8, 9, 10);
