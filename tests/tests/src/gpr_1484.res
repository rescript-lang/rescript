type t
@set external clearNodeValue: (t, %raw("null")) => unit = "nodeValue"

/* TODO: more test cases */
/* external clearNodeValue2 : */
/* t -> (_ [@as {json|null|json}]) -> int -> unit = */
/* "nodeValue" [@@set] */

let test = x => clearNodeValue(x)
