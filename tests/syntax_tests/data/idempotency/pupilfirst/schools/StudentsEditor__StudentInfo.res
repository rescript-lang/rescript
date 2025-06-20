type t = {
  name: string,
  email: string,
  tags: array<string>,
  title: string,
  affiliation: string,
  teamName: option<string>,
}

let name = t => t.name

let email = t => t.email

let tags = t => t.tags

let title = t => t.title

let teamName = t => t.teamName

let affiliation = t => t.affiliation

let encode = t => {
  open Json.Encode
  object_(list{
    ("name", t.name->string),
    ("email", t.email->string),
    ("title", t.title->string),
    ("affiliation", t.affiliation->string),
    ("tags", t.tags->array(string)),
    ("team_name", t.teamName->nullable(string)),
  })
}

let make = (~name, ~email, ~title, ~affiliation, ~tags, ~teamName) => {
  name: name,
  email: email,
  title: title,
  affiliation: affiliation,
  tags: tags,
  teamName: teamName,
}
