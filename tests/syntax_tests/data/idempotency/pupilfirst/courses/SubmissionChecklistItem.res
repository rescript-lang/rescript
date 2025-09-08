type file = {
  id: string,
  name: string,
  url: string,
}

type result =
  | ShortText(string)
  | LongText(string)
  | Link(string)
  | Files(array<file>)
  | MultiChoice(string)

type status =
  | Passed
  | Failed
  | NoAnswer

type t = {
  title: string,
  result: result,
  status: status,
}

let title = t => t.title
let result = t => t.result
let status = t => t.status
let fileName = file => file.name
let fileUrl = file => file.url

let make = (~title, ~result, ~status) => {title: title, result: result, status: status}

let makeFile = (~name, ~url, ~id) => {name: name, url: url, id: id}

let makeFiles = data =>
  data->Js.Array.map(a => makeFile(~url=a["url"], ~name=a["title"], ~id=a["id"]))

let makeResult = (result, kind, files) =>
  switch kind {
  | "shortText" => ShortText(result)
  | "longText" => LongText(result)
  | "link" => Link(result)
  | "multiChoice" => MultiChoice(result)
  | "files" => Files(files)
  | randomKind =>
    Rollbar.error(
      "Unkown kind: " ++ (randomKind ++ "recived in CurriculumEditor__TargetChecklistItem"),
    )
    ShortText("Error")
  }

let makeStatus = data =>
  switch data {
  | "noAnswer" => NoAnswer
  | "passed" => Passed
  | "failed" => Failed
  | unkownStatus =>
    Rollbar.error(
      "Unkown status:" ++ (unkownStatus ++ "recived in CourseReview__SubmissionChecklist"),
    )
    NoAnswer
  }

let makeArrayFromJs = (files, checklist) =>
  checklist->Js.Array.map(c =>
    make(
      ~title=c["title"],
      ~result=makeResult(c["result"], c["kind"], makeFiles(files)),
      ~status=makeStatus(c["status"]),
    )
  )

let decodeFile = json => {
  open Json.Decode
  {
    name: json->field("name", string),
    url: json->field("url", string),
    id: json->field("id", string),
  }
}

let decode = (files, json) => {
  open Json.Decode
  {
    result: makeResult(json->field("result", string), json->field("kind", string), files),
    status: makeStatus(json->field("status", string)),
    title: json->field("title", string),
  }
}

let updateStatus = (checklist, index, status) =>
  checklist->Array.mapi((i, t) =>
    i == index ? make(~title=t.title, ~result=t.result, ~status) : t
  )

let makeNoAnswer = (index, checklist) => updateStatus(checklist, index, NoAnswer)

let makeFailed = (index, checklist) => updateStatus(checklist, index, Failed)

let makePassed = (index, checklist) => updateStatus(checklist, index, Passed)

let encodeKind = t =>
  switch t.result {
  | ShortText(_) => "shortText"
  | LongText(_) => "longText"
  | Link(_) => "link"
  | Files(_) => "files"
  | MultiChoice(_) => "multiChoice"
  }

let encodeResult = t =>
  switch t.result {
  | ShortText(t)
  | LongText(t)
  | Link(t) => t
  | MultiChoice(_)
  | Files(_) => "files"
  }

let encodeStatus = t =>
  switch t.status {
  | NoAnswer => "noAnswer"
  | Passed => "passed"
  | Failed => "failed"
  }

let encode = t => {
  open Json.Encode
  object_(list{
    ("title", t.title->string),
    ("kind", encodeKind(t)->string),
    ("status", encodeStatus(t)->string),
    ("result", encodeResult(t)->string),
  })
}

let encodeArray = checklist =>
  checklist->{
    open Json.Encode
    array(encode)
  }
