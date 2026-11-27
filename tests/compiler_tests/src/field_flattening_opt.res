module WithNestedMutableFields = {
  type nested = {mutable field?: int}
  type group = {nested: nested}

  let group: group = {nested: {}}

  let fn = str => {
    group.nested.field = str->Int.fromString
  }
}

module NoOptionalFields = {
  type record = {field: int}
  type pair = (record, string)

  let p: pair = ({field: 2}, "")

  let x = Pair.first(p)
  let y = Pair.first(p)
}
