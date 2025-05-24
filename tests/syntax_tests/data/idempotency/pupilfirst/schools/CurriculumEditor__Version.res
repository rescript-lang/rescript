type t = {
  id: string,
  number: int,
  createdAt: Js.Date.t,
  updatedAt: Js.Date.t,
}

let id = t => t.id

let createdAt = t => t.createdAt

let updatedAt = t => t.updatedAt

let number = t => t.number

let make = (id, number, createdAt, updatedAt) => {
  id: id,
  number: number,
  createdAt: createdAt,
  updatedAt: updatedAt,
}

let makeArrayFromJs = js => {
  let length = js->Array.length
  js
  ->ArrayUtils.copyAndSort((x, y) =>
    DateFns.differenceInSeconds(
      y["createdAt"]->Json.Decode.string->DateFns.parseString,
      x["createdAt"]->Json.Decode.string->DateFns.parseString,
    )->int_of_float
  )
  ->Array.mapi((number, c) =>
    make(
      c["id"],
      length - number,
      c["createdAt"]->Json.Decode.string->DateFns.parseString,
      c["updatedAt"]->Json.Decode.string->DateFns.parseString,
    )
  )
}

let versionAt = t => t.createdAt->DateFns.format("MMM D, YYYY HH:MM")

let isLatestTargetVersion = (versions, t) => {
  let length = versions->Array.length
  t.number == length
}
