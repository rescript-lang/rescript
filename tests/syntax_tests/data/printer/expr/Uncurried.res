@@uncurried

let add3 = (x,y,z) => x+y+z

let triangle = 3->add3(1,2)->add3(4,5)

let () = 3->ignore

let foo = (/* ddd */ x) => x

let f = (
  // comment
  ~a,
) => a