@deriving(jsConverter)
type orientation = [
  | @as("horizontal") #Horizontal
  | @as("vertical") #Vertical
]

let () = Js.log(orientationToJs(#Horizontal))
