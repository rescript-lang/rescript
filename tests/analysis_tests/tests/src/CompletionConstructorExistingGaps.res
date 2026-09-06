type gap = Gap(bool, bool, bool) | Unary((bool, bool))

let first: gap = Gap(true,  false, true)
//                         ^com

let second: gap = Gap(true, false,  true)
//                                 ^com

let inferred = Gap(true,  false, true)
//                       ^com

let unary: gap = Unary(true,  false)
//                           ^com

let nested: gap = Gap((true,  false, true))
//                           ^com
