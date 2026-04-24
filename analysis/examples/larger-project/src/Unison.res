// Exmple of several DCE checks operating in unison

type break_ =
  | IfNeed
  | Never
  | Always

type t = {
  break_: break_,
  doc: string,
}

type rec stack =
  | Empty
  | Cons(t, stack)

let group = (~break_=IfNeed, doc) => {break_, doc: doc}

let rec fits = (w, stack) =>
  switch stack {
  | _ if w < 0 => false
  | Empty => true
  | Cons({doc}, stack) => fits(w - String.length(doc), stack)
  }

let rec toString = (~width, stack) =>
  switch stack {
  | Cons({break_, doc}, stack) =>
    switch break_ {
    | IfNeed => (fits(width, stack) ? "fits " : "no ") ++ (stack |> toString(~width=width - 1))
    | Never => "never " ++ (doc ++ (stack |> toString(~width=width - 1)))
    | Always => "always " ++ (doc ++ (stack |> toString(~width=width - 1)))
    }
  | Empty => ""
  }

toString(~width=80, Empty)
toString(~width=80, Cons(group(~break_=Never, "abc"), Empty))
toString(~width=80, Cons(group(~break_=Always, "d"), Empty))
