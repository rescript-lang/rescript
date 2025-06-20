type t = {
  id: string,
  name: string,
  avatarUrl: option<string>,
  email: string,
  tags: array<string>,
  excludedFromLeaderboard: bool,
  title: string,
  affiliation: option<string>,
}

let name = t => t.name

let avatarUrl = t => t.avatarUrl

let id = t => t.id

let title = t => t.title

let affiliation = t => t.affiliation

let email = t => t.email

let tags = t => t.tags

let excludedFromLeaderboard = t => t.excludedFromLeaderboard

let updateInfo = (~name, ~excludedFromLeaderboard, ~title, ~affiliation, ~student) => {
  ...student,
  name: name,
  excludedFromLeaderboard: excludedFromLeaderboard,
  title: title,
  affiliation: affiliation,
}

let make = (
  ~id,
  ~name,
  ~avatarUrl,
  ~email,
  ~tags,
  ~excludedFromLeaderboard,
  ~title,
  ~affiliation,
) => {
  id: id,
  name: name,
  avatarUrl: avatarUrl,
  email: email,
  tags: tags,
  excludedFromLeaderboard: excludedFromLeaderboard,
  title: title,
  affiliation: affiliation,
}

let makeFromJS = studentDetails =>
  make(
    ~id=studentDetails["id"],
    ~name=studentDetails["name"],
    ~avatarUrl=studentDetails["avatarUrl"],
    ~email=studentDetails["email"],
    ~tags=studentDetails["tags"],
    ~excludedFromLeaderboard=studentDetails["excludedFromLeaderboard"],
    ~title=studentDetails["title"],
    ~affiliation=studentDetails["affiliation"],
  )

let update = (~name, ~tags, ~excludedFromLeaderboard, ~title, ~affiliation, ~student) => {
  ...student,
  name: name,
  tags: tags,
  excludedFromLeaderboard: excludedFromLeaderboard,
  title: title,
  affiliation: affiliation,
}

let encode = (teamName, t) => {
  open Json.Encode
  object_(list{
    ("id", t.id->string),
    ("name", t.name->string),
    ("team_name", teamName->string),
    ("email", t.email->string),
    ("excluded_from_leaderboard", t.excludedFromLeaderboard->bool),
    ("title", t.title->string),
    ("affiliation", t.affiliation->OptionUtils.toString->string),
  })
}
