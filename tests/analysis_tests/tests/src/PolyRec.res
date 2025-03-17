let rec sum = x =>
  switch x {
  | #Leaf => 0
  | #Node(value, left, right) => value + left->sum + right->sum
  }

let myTree = #Node(
  1,
  #Node(2, #Node(4, #Leaf, #Leaf), #Node(6, #Leaf, #Leaf)),
  #Node(3, #Node(5, #Leaf, #Leaf), #Node(7, #Leaf, #Leaf)),
)

let () = myTree->sum->Console.log
//        ^hov
