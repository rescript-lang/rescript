// Note: this is exposed to support syntax
external mutableCell: ('a, list<'a>) => list<'a> = "#makemutablelist"

@set external unsafeMutateTail: (list<'a>, list<'a>) => unit = "tl"

let rec copyAux = (list, previous) =>
  switch list {
  | list{} => previous
  | list{head, ...tail} =>
    let next = mutableCell(head, list{})
    unsafeMutateTail(previous, next)
    copyAux(tail, next)
  }

let concat = (left, right) =>
  switch left {
  | list{} => right
  | list{head, ...tail} =>
    let result = mutableCell(head, list{})
    unsafeMutateTail(copyAux(tail, result), right)
    result
  }

let spread = lists =>
  switch lists {
  | [] => list{}
  | [list] => list
  | _ =>
    let length = Primitive_array_extern.length(lists)
    let result = ref(Primitive_array_extern.getUnsafe(lists, length - 1))
    for i in length - 2 downto 0 {
      result.contents = concat(Primitive_array_extern.getUnsafe(lists, i), result.contents)
    }
    result.contents
  }
