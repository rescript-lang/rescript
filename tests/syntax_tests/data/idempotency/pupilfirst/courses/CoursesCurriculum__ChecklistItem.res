type file = {
  id: string,
  name: string,
}

type rec result =
  | Files(array<file>)
  | Link(string)
  | ShortText(string)
  | LongText(string)
  | MultiChoice(choices, option<int>)

and choices = array<string>

type t = {
  title: string,
  optional: bool,
  result: result,
}

let title = t => t.title

let result = t => t.result

let optional = t => t.optional

let make = (~result, ~title, ~optional) => {result: result, title: title, optional: optional}

let fromTargetChecklistItem = targetChecklist =>
  targetChecklist->Array.map(tc => {
    let title = tc->TargetChecklistItem.title
    let optional = tc->TargetChecklistItem.optional
    let result = switch tc->TargetChecklistItem.kind {
    | Files => Files([])
    | Link => Link("")
    | ShortText => ShortText("")
    | LongText => LongText("")
    | MultiChoice(choices) => MultiChoice(choices, None)
    }
    make(~title, ~optional, ~result)
  })

let updateResultAtIndex = (index, result, checklist) =>
  checklist->Array.mapi((i, c) => i == index ? {...c, result: result} : c)

let makeFile = (id, name) => {id: id, name: name}

let filename = file => file.name

let fileId = file => file.id

let fileIds = checklist =>
  checklist
  ->Array.map(c =>
    switch c.result {
    | Files(files) => files->Array.map(a => a.id)->Array.to_list
    | _anyOtherResult => list{}
    }
  )
  ->ArrayUtils.flatten

let kindAsString = t =>
  switch t.result {
  | Files(_) => "files"
  | Link(_) => "link"
  | ShortText(_) => "shortText"
  | LongText(_) => "longText"
  | MultiChoice(_, _) => "multiChoice"
  }

let resultAsString = t =>
  switch t.result {
  | Files(_) => "files"
  | Link(t)
  | ShortText(t)
  | LongText(t) => t
  | MultiChoice(choices, index) =>
    index->OptionUtils.flatMap(i => choices->ArrayUtils.getOpt(i))->OptionUtils.default("")
  }

let validString = (s, maxLength) => {
  let length = s->String.trim->String.length
  length >= 1 && length <= maxLength
}

let validShortText = s => validString(s, 250)

let validLongText = s => validString(s, 5000)

let validFiles = files => files != [] && files->Array.length < 3

let validMultiChoice = (choices, index) =>
  index->OptionUtils.mapWithDefault(i => choices->Array.length > i, false)

let validResponse = (response, allowBlank) => {
  let optional = allowBlank ? response.optional : false

  switch (response.result, optional) {
  | (Files(files), false) => validFiles(files)
  | (Files(files), true) => files->ArrayUtils.isEmpty || validFiles(files)
  | (Link(link), false) => link->UrlUtils.isValid(false)
  | (Link(link), true) => link->UrlUtils.isValid(true)
  | (ShortText(t), false) => validShortText(t)
  | (ShortText(t), true) => validShortText(t) || t == ""
  | (LongText(t), false) => validLongText(t)
  | (LongText(t), true) => validLongText(t) || t == ""
  | (MultiChoice(choices, index), false) => validMultiChoice(choices, index)
  | (MultiChoice(choices, index), true) => validMultiChoice(choices, index) || index == None
  }
}

let validChecklist = checklist =>
  checklist
  ->Array.map(c => validResponse(c, true))
  ->Js.Array.filter(c => !c)
  ->ArrayUtils.isEmpty

let validResonses = responses => responses->Js.Array.filter(c => validResponse(c, false))

let encode = t => {
  open Json.Encode
  object_(list{
    ("title", t.title->string),
    ("kind", kindAsString(t)->string),
    ("status", "noAnswer"->string),
    ("result", resultAsString(t)->string),
  })
}

let encodeArray = checklist =>
  validResonses(checklist)->{
    open Json.Encode
    array(encode)
  }

let makeFiles = checklist =>
  checklist
  ->Js.Array.find(c =>
    switch c.result {
    | Files(_files) => true
    | _anyOtherResult => false
    }
  )
  ->OptionUtils.mapWithDefault(c =>
    switch c.result {
    | Files(files) => files
    | _anyOtherResult => []
    }
  , [])
  ->Array.map(f => {
    let url = "/timeline_event_files/" ++ (f.id ++ "/download")
    SubmissionChecklistItem.makeFile(~name=f.name, ~id=f.id, ~url)
  })
