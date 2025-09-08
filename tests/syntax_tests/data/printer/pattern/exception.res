let exception Exit = 1
let exception (Exit | Quit) = 1
let exception Exit as z = 1
let exception (Exit as z) = 1
