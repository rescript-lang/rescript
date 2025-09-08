type t

let make: (~locations: array<string>) => t
let updateColors: (~locations: array<string>, t) => t
let getColor: (~location: string, t) => (string, string)
