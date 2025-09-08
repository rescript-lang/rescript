// Equivalent to styles/_theme.css

@deriving(jsConverter)
type t = [
  | @as("theme-reason") #Reason
  | @as("theme-js") #Js
]

let toCN = value => tToJs(value)
