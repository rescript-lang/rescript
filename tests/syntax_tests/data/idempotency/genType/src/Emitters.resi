type t

let initial: t

let exportEarly: (~emitters: t, string) => t

let requireEarly: (~emitters: t, string) => t

let \"export": (~emitters: t, string) => t

let \"import": (~emitters: t, string) => t

let require: (~emitters: t, string) => t

let toString: (~separator: string, t) => string
