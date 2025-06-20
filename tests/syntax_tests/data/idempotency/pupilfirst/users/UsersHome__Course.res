type t = {
  id: string,
  name: string,
  review: bool,
  author: bool,
  enableLeaderboard: bool,
  description: string,
  exited: bool,
  thumbnailUrl: option<string>,
  linkedCommunities: array<string>,
  ended: bool,
}

let name = t => t.name
let id = t => t.id
let review = t => t.review
let author = t => t.author
let description = t => t.description
let exited = t => t.exited
let thumbnailUrl = t => t.thumbnailUrl
let linkedCommunities = t => t.linkedCommunities
let ended = t => t.ended
let enableLeaderboard = t => t.enableLeaderboard

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    name: json->field("name", string),
    description: json->field("description", string),
    exited: json->field("exited", bool),
    review: json->field("review", bool),
    author: json->field("author", bool),
    enableLeaderboard: json->field("enableLeaderboard", bool),
    thumbnailUrl: json->field("thumbnailUrl", nullable(string))->Js.Null.toOption,
    linkedCommunities: json->field("linkedCommunities", array(string)),
    ended: json->field("ended", bool),
  }
}
