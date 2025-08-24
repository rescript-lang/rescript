type myType = [
  | #first
  | #second(string)
  | #third
  | #fourth
]

// just a quick way to make the switch compile
@val external x: myType = "myVariable"

switch x {
| #first => Console.log("first")
}
