// Exmple of several DCE checks operating in unison

type break =
  | IfNeed
  | Never
  | Always

type t = {
  break: break,
  doc: string,
}

type rec stack =
  | Empty
  | Cons(t, stack)

let group = (~break=IfNeed, doc) => {break: break, doc: doc}

let rec fits = (w, stack) =>
  switch stack {
  | _ if w < 0 => false
  | Empty => true
  | Cons({doc}, stack) => fits(w - String.length(doc), stack)
  }

let rec toString = (~width, stack) =>
  switch stack {
  | Cons({break, doc}, stack) =>
    switch break {
    | IfNeed => (fits(width, stack) ? "fits " : "no ") ++ (stack |> toString(~width=width - 1))
    | Never => "never " ++ (doc ++ (stack |> toString(~width=width - 1)))
    | Always => "always " ++ (doc ++ (stack |> toString(~width=width - 1)))
    }
  | Empty => ""
  }

toString(~width=80, Empty)
toString(~width=80, Cons(group(~break=Never, "abc"), Empty))
toString(~width=80, Cons(group(~break=Always, "d"), Empty))
