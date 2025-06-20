type t = {
  id: string,
  author: option<User.t>,
  note: string,
  createdAt: Js.Date.t,
}

let make = (~id, ~note, ~createdAt, ~author) => {
  id: id,
  note: note,
  createdAt: createdAt,
  author: author,
}

let id = t => t.id

let note = t => t.note

let createdAt = t => t.createdAt

let author = t => t.author

let noteOn = t => t.createdAt->DateFns.format("MMMM D, YYYY")

let sort = notes =>
  notes->ArrayUtils.copyAndSort((x, y) =>
    DateFns.differenceInSeconds(y.createdAt, x.createdAt)->int_of_float
  )

let makeFromJs = note =>
  make(
    ~id=note["id"],
    ~note=note["note"],
    ~createdAt=note["createdAt"]->DateFns.parseString,
    ~author=note["author"]->OptionUtils.map(User.makeFromJs),
  )
