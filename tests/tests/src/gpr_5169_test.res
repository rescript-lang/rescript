module NotOption = {
  type t<'a> = None | Some('a)
}

let a = NotOption.None /* undefined */
let b = NotOption.Some(1) /* 1 */
let c = {
  open NotOption
  None
}
let d = {
  open NotOption
  Some(1)
}

module TotallyNotOption = {
  type t<'a> = Some('a) | None
}

let e = TotallyNotOption.None /* undefined */
let f = TotallyNotOption.Some(1) /* 1 */
let g = {
  open TotallyNotOption
  None
}
let h = {
  open TotallyNotOption
  Some(1)
}

module NotOption2 = {
  type t<'a> = None | Some('a) | Bogus
}

let i = NotOption2.None
let j = NotOption2.Some(1)
let k = {
  open NotOption2
  None
}
let l = {
  open NotOption2
  Some(1)
}
