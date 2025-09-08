type t = {
  title: string,
  result: array<CoursesReview__ReviewChecklistResult.t>,
}
let title = t => t.title
let result = t => t.result

let make = (~title, ~result) => {title: title, result: result}

let makeFromJs = data =>
  data->Js.Array.map(rc =>
    make(
      ~title=rc["title"],
      ~result=rc["result"]->CoursesReview__ReviewChecklistResult.makeFromJs,
    )
  )

let empty = () => [make(~title="", ~result=[CoursesReview__ReviewChecklistResult.empty()])]

let emptyTemplate = () => [
  make(~title="Default checklist", ~result=CoursesReview__ReviewChecklistResult.emptyTemplate()),
]

let updateTitle = (title, t) => make(~title, ~result=t.result)

let updateChecklist = (result, t) => make(~title=t.title, ~result)

let replace = (t, itemIndex, result) =>
  result->Array.mapi((index, item) => index == itemIndex ? t : item)

let appendEmptyChecklistItem = t =>
  make(
    ~title=t.title,
    ~result=[CoursesReview__ReviewChecklistResult.empty()]->Array.append(t.result),
  )

let deleteResultItem = (index, t) =>
  make(~title=t.title, ~result=t.result->Js.Array.filteri((_el, i) => i != index))

let trim = t => {
  title: t.title->String.trim,
  result: t.result->Array.map(CoursesReview__ReviewChecklistResult.trim),
}

let encode = t => {
  open Json.Encode
  object_(list{
    ("title", t.title->string),
    ("result", t.result->array(CoursesReview__ReviewChecklistResult.encode)),
  })
}

let encodeArray = checklist =>
  checklist->{
    open Json.Encode
    array(encode)
  }
