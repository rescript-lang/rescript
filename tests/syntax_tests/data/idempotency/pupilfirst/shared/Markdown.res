@module("./markdownIt") external markdownIt: string => string = "default"
@module("./sanitize")
external sanitize: (string, string) => string = "default"

type profile =
  | Comment
  | QuestionAndAnswer
  | Permissive
  | AreaOfText

let profileString = profile =>
  switch profile {
  | Comment => "comment"
  | QuestionAndAnswer => "questionAndAnswer"
  | Permissive => "permissive"
  | AreaOfText => "areaOfText"
  }

let parse = (profile, markdown) => markdown->markdownIt->sanitize(profileString(profile))
