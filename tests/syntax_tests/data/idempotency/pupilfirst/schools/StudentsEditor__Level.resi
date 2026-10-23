type t

let name: t => string

let number: t => int

let id: t => string

let decode: JSON.t => t

let title: t => string

let unsafeFind: (array<t>, string, string) => t
