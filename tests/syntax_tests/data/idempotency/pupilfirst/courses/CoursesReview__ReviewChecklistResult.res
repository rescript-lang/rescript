type t = {
  title: string,
  feedback: option<string>,
}
let title = t => t.title
let feedback = t => t.feedback

let make = (~title, ~feedback) => {title: title, feedback: feedback}

let makeFromJs = data => data->Js.Array.map(r => make(~title=r["title"], ~feedback=r["feedback"]))

let emptyTemplate = () => [
  make(~title="Yes", ~feedback=Some("Sample feedback for yes")),
  make(~title="No", ~feedback=Some("Sample feedback for no")),
]

let empty = () => make(~title="", ~feedback=None)

let replace = (t, index, checklist) =>
  checklist->Array.mapi((resultIndex, result) => resultIndex == index ? t : result)

let updateTitle = (title, t, index, checklist) =>
  checklist->replace(make(~title, ~feedback=t.feedback), index)

let updateFeedback = (feedback, t, index, checklist) => {
  let optionalFeedback = feedback->Js.String.trim == "" ? None : Some(feedback)

  checklist->replace(make(~title=t.title, ~feedback=optionalFeedback), index)
}

let trim = t => {...t, title: t.title->String.trim}

let encode = t => {
  let title = list{("title", t.title->Json.Encode.string)}

  let feedback = switch t.feedback {
  | Some(f) => list{("feedback", f->Json.Encode.string)}
  | None => list{}
  }

  open Json.Encode
  object_(\"@"(title, feedback))
}
