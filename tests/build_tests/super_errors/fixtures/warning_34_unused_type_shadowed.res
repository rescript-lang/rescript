module M: {
  let v: option<int>
  let w: string
} = {
  type t = option<int>
  type unusedAlias = option<int>

  module Inner = {
    type t = string
    type unusedInner = t
    let w: t = "w"
  }

  open Inner
  let w: t = w
  let v = (Some(String.length(w)): option<int>)
}
