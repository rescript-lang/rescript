type t

let id: t => string

let name: t => string
let avatarUrl: t => string
let teamId: t => string

let decode: JSON.t => t
