type t = {
  id: string,
  title: string,
  createdAt: Js.Date.t,
  passedAt: option<Js.Date.t>,
  levelId: string,
}

let make = (~id, ~title, ~createdAt, ~passedAt, ~levelId) => {
  id: id,
  title: title,
  createdAt: createdAt,
  passedAt: passedAt,
  levelId: levelId,
}

let id = t => t.id

let levelId = t => t.levelId

let title = t => t.title

let sort = submissions =>
  submissions->ArrayUtils.copyAndSort((x, y) =>
    DateFns.differenceInSeconds(y.createdAt, x.createdAt)->int_of_float
  )

let failed = t =>
  switch t.passedAt {
  | Some(_passedAt) => false
  | None => true
  }

let createdAtPretty = t => t.createdAt->DateFns.format("MMMM D, YYYY")

let makeFromJs = submissions =>
  submissions->Js.Array.map(submission =>
    switch submission {
    | Some(submission) =>
      let createdAt = submission["createdAt"]->Json.Decode.string->DateFns.parseString
      let passedAt = switch submission["passedAt"] {
      | Some(passedAt) => Some(passedAt->Json.Decode.string->DateFns.parseString)
      | None => None
      }
      list{
        make(
          ~id=submission["id"],
          ~title=submission["title"],
          ~createdAt,
          ~passedAt,
          ~levelId=submission["levelId"],
        ),
      }
    | None => list{}
    }
  )
