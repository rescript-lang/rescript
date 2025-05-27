type question = {
  questionId: string,
  questionTitle: string,
}

type t = {
  id: string,
  name: string,
  questions: list<question>,
}

let decodeQuestion = json => {
  open Json.Decode
  {
    questionId: json->field("id", string),
    questionTitle: json->field("title", string),
  }
}

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    name: json->field("name", string),
    questions: json->field("questions", list(decodeQuestion)),
  }
}

let id = t => t.id

let name = t => t.name

let questions = t => t.questions

let questionId = question => question.questionId

let questionTitle = question => question.questionTitle
