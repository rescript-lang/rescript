let \"_" = 1
let \"_'" = 2
let \"_''" = 3
let \"__'" = 4
let \"_a'" = 5
let f = (~\"_'") => \"_'"
let a = #"_'"
let b = #"_"
type r = {\"_'": int}
let r = {\"_'": 1}
let g = r.\"_'"
let h = (~\"_") => 1
type s = {\"_": int}
