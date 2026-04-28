type t

@send @return(null_to_opt) external getItem: (t, string) => option<string> = "getItem"
@send external setItem: (t, string, string) => unit = "setItem"
@send external removeItem: (t, string) => unit = "removeItem"
@send external clear: t => unit = "clear"
@send @return(null_to_opt) external key: (t, int) => option<string> = "key"
@get external length: t => int = "length"

@val external localStorage: t = "localStorage"
@val external sessionStorage: t = "sessionStorage"
