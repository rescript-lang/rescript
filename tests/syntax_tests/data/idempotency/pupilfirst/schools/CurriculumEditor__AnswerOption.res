type id = string

type t = {
  id: id,
  answer: string,
  correctAnswer: bool,
}

let id = t => t.id

let answer = t => t.answer

let correctAnswer = t => t.correctAnswer

let empty = (id, correctAnswer) => {id: id, answer: "", correctAnswer: correctAnswer}

let updateAnswer = (answer, t) => {...t, answer: answer}

let markAsCorrect = t => {...t, correctAnswer: true}

let markAsIncorrect = t => {...t, correctAnswer: false}

let isValidAnswerOption = t => t.answer->Js.String.trim->Js.String.length >= 1

let makeFromJs = answerData => {
  id: answerData["id"],
  answer: answerData["answer"],
  correctAnswer: answerData["correctAnswer"],
}

let quizAnswersAsJsObject = quizAnswers =>
  quizAnswers->Array.map(qa => {"answer": qa.answer, "correctAnswer": qa.correctAnswer})
