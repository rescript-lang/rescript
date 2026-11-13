type t = {cents: int}
let make = cents => {cents: cents}
let add = (a, b) => {cents: a.cents + b.cents}
