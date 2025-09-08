type r = {nonopt: int, opt?: string}

let v = {nonopt: ?3, opt: ?None}

let f = r =>
  switch r {
  | {nonopt: ?_, opt: ?_} => true
  }

type inline = A({nonopt: int, opt?: string})

let vi = A({nonopt: ?3, opt: ?None})

let fi = a =>
  switch a {
  | A({nonopt: ?_, opt: ?_}) => true
  }
