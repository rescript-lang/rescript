type rec expression =
  | /** non-negative integer constant */
  Numeral(float)
  | /** Addition [e1 + e2] */
  Plus(expression, expression)
  | /** Difference [e1 - e2] */
  Minus(expression, expression)
  | /** Product [e1 * e2] */
  Times(expression, expression)
  | /** Quotient [e1 / e2] */
  Divide(expression, expression)
  | /** Opposite value [-e] */
  Negate(expression)
  | Variable(string)

let rec str = e =>
  switch e {
  | Numeral(f) => f->Js.Float.toString
  | Plus(a, b) => str(a) ++ ("+" ++ str(b))
  | Minus(a, b) => str(a) ++ ("-" ++ str(b))
  | Times(a, b) => str(a) ++ ("*" ++ str(b))
  | Divide(a, b) => str(a) ++ ("/" ++ str(b))
  | Negate(a) => "-" ++ str(a)
  | Variable(s) => s
  }
