@@config({no_export: no_export})
type t =
  | A_list
  | A_hashtable

let string_of_associative_type = x =>
  switch x {
  | A_list => "list"
  | A_hashtable => "Hashtbl.t"
  }

Console.log(string_of_associative_type(A_list))

type t2 =
  | A_list
  | A_hashtable
  | A_bad
let string_of_associative_type = x =>
  switch x {
  | A_list => "list"
  | A_hashtable => "Hashtbl.t"
  | A_bad => "bad"
  }

Console.log(string_of_associative_type(A_list))

let v = Some(3)

let f = x =>
  switch x {
  | Some(_) => "Some"
  | None => "None"
  }
let u = v => f(Some(v))
Console.log((f(v), f(None), f(Some(3))))

type v =
  | A(int)
  | B(int)
  | C(int)
  | D(int)
  | E(int)
  | F(int)
  | G(int)
  | H(int)

let ff = x =>
  switch x {
  | A(_) => "A"
  | B(_) => "B"
  | C(_) => "C"
  | D(_) => "D"
  | E(_) => "E"
  | F(_) => "F"
  | G(_) => "G"
  | H(_) => "H"
  }

Console.log((
  ff(A(3)),
  (
    x =>
      switch x {
      | A(_) => "A"
      | B(_) => "B"
      | _ => "?"
      }
  )(A(3)),
))
