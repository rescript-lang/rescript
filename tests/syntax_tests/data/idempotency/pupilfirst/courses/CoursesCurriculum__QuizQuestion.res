type answerOption = {
  id: string,
  value: string,
}

type t = {
  index: int,
  question: string,
  answerOptions: list<answerOption>,
}

let decodeAnswerOption = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    value: json->field("value", string),
  }
}

let decode = json => {
  open Json.Decode
  {
    index: json->field("index", int),
    question: json->field("question", string),
    answerOptions: json->field("answerOptions", list(decodeAnswerOption)),
  }
}

let index = t => t.index

let question = t => t.question

let answerOptions = t => t.answerOptions

let answerId = answerOption => answerOption.id

let answerValue = answerOption => answerOption.value

let lastQuestion = questions => {
  let maxIndex =
    questions->List.sort((q1, q2) => q1.index - q2.index)->List.rev->List.hd->index
  questions->List.find(q => q.index == maxIndex)
}

let nextQuestion = (questions, question) =>
  questions->List.find(q => q.index == question.index + 1)

let isLastQuestion = (questions, question) => questions->lastQuestion == question
