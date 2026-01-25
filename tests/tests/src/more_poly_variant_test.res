type rec vlist<'a> = [#Nil | #Cons('a, vlist<'a>)]

let rec map = (f): (vlist<'a> => vlist<'b>) =>
  x =>
    switch x {
    | #Nil => #Nil
    | #Cons(a, l) => #Cons(f(a), map(f)(l))
    }

let split_cases = x =>
  switch x {
  | (#Nil | #Cons(_)) as x => #A(x)
  | #Snoc(_) as x => #B(x)
  }

type myvariant = [#Tag1(int) | #Tag2(bool)]

let f = x =>
  switch x {
  | #...myvariant => "myvariant"
  | #Tag3 => "Tag3"
  }

let g1 = x =>
  switch x {
  | #Tag1(_) => "Tag1"
  | #Tag2(_) => "Tag2"
  }

let g = x =>
  switch x {
  | #...myvariant as x => g1(x)
  | #Tag3 => "Tag3"
  }

type abc = [#A | #B | #C]

let f1 = x =>
  switch x {
  | #As => "A"
  | #...abc => "other"
  }

type myvariant2 = [#Tag3(int) | #Tag4 | myvariant]
type x = [#a | #b | #c]
let f2 = x =>
  switch x {
  | #...myvariant2 as x =>
    Console.log(x)
    2
  | #hello => 3
  | #h => 2
  | #...x => 333
  }
