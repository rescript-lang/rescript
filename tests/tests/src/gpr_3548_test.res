@deriving(jsConverter)
type orientation = [
  | @as("horizontal") #Horizontal
  | @as("vertical") #Vertical
]

let () = Console.log(orientationToJs(#Horizontal))
