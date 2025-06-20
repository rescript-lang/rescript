type t = {
  createdAt: Js.Date.t,
  value: string,
  coachName: string,
  coachAvatarUrl: option<string>,
  coachTitle: string,
}
let value = t => t.value
let coachName = t => t.coachName
let coachAvatarUrl = t => t.coachAvatarUrl
let coachTitle = t => t.coachTitle
let createdAtPretty = t => t.createdAt->DateFns.format("MMMM D, YYYY")

let make = (~coachName, ~coachAvatarUrl, ~coachTitle, ~createdAt, ~value) => {
  coachName: coachName,
  coachAvatarUrl: coachAvatarUrl,
  coachTitle: coachTitle,
  createdAt: createdAt,
  value: value,
}
