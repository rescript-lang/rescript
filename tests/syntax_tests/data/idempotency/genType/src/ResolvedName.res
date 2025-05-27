type t = list<string>

let dot = (s, x) => \"@"(x, list{s})

let fromString = x => list{x}

let toList = x => x

let toString = x => x->String.concat("_")

type eq = (t, t)

module NameSet = Set.Make({
  type t = t
  let rec compare = (x: t, y: t) =>
    switch (x, y) {
    | (list{}, list{}) => 0
    | (list{}, list{_, ..._}) => -1
    | (list{_, ..._}, list{}) => -1
    | (list{s1, ...rest1}, list{s2, ...rest2}) =>
      let n = String.compare(s1, s2)
      n != 0 ? n : compare(rest1, rest2)
    }
})

let rec applyEquation = (~el: t, eq: eq): list<t> =>
  switch (eq, el) {
  | ((list{}, rhs), _) => list{\"@"(rhs, el)}
  | ((list{s1, ...rest1}, rhs), list{s2, ...rest2}) =>
    s1 == s2 ? (rest1, rhs)->applyEquation(~el=rest2) : list{}
  | ((list{_, ..._}, _), list{}) => list{}
  }

let rec applyEquationsToElements = (~eqs: list<eq>, ~seen, elements: list<t>): list<eq> => {
  let applyEqs = el => {
    let freshElements =
      eqs->List.map(applyEquation(~el))->List.concat->List.filter(y => !NameSet.mem(y, seen))
    freshElements->List.map(elFresh => (elFresh, el))
  }

  let newEquations = elements->List.map(applyEqs)->List.concat
  let newElements = newEquations->List.map(fst)
  let newSeen = NameSet.union(seen, newElements->NameSet.of_list)

  newEquations == list{}
    ? newEquations
    : \"@"(newEquations, newElements->applyEquationsToElements(~eqs, ~seen=newSeen))
}

/* Apply equations of the form e.g. X.Y = A from the alias: module A = X.Y.
   Return a list of equations on types.
   E.g. if the element is X.Y.t, return equation A.t = X.Y.t */
let applyEquations = (~eqs: list<eq>, el: t): list<eq> =>
  list{el}->applyEquationsToElements(~eqs, ~seen=NameSet.empty)
