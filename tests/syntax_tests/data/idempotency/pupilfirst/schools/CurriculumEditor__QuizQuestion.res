type id = string

type t = {
  id: id,
  question: string,
  answerOptions: array<CurriculumEditor__AnswerOption.t>,
}

let id = t => t.id

let question = t => t.question

let answerOptions = t => t.answerOptions

let empty = id => {
  id: id,
  question: "",
  answerOptions: [
    CurriculumEditor__AnswerOption.empty("0", true),
    CurriculumEditor__AnswerOption.empty("1", false),
  ],
}

let updateQuestion = (question, t) => {...t, question: question}

let newAnswerOption = (id, t) => {
  let answerOption = CurriculumEditor__AnswerOption.empty(id, false)
  let newAnswerOptions = t.answerOptions->ArrayUtils.copyAndPush(answerOption)
  {...t, answerOptions: newAnswerOptions}
}

let removeAnswerOption = (id, t) => {
  let newAnswerOptions =
    t.answerOptions->Js.Array.filter(a => a->CurriculumEditor__AnswerOption.id !== id)
  {...t, answerOptions: newAnswerOptions}
}

let replace = (id, answerOptionB, t) => {
  let newAnswerOptions =
    t.answerOptions->Array.map(a =>
      a->CurriculumEditor__AnswerOption.id == id ? answerOptionB : a
    )
  {...t, answerOptions: newAnswerOptions}
}

let markAsCorrect = (id, t) => {
  let newAnswerOptions =
    t.answerOptions->Array.map(a =>
      a->CurriculumEditor__AnswerOption.id == id
        ? CurriculumEditor__AnswerOption.markAsCorrect(a)
        : CurriculumEditor__AnswerOption.markAsIncorrect(a)
    )
  {...t, answerOptions: newAnswerOptions}
}

let isValidQuizQuestion = t => {
  let validQuestion = t.question->Js.String.trim->Js.String.length >= 1
  let hasZeroInvalidAnswerOptions =
    t.answerOptions
    ->Js.Array.filter(answerOption =>
      answerOption->CurriculumEditor__AnswerOption.isValidAnswerOption != true
    )
    ->ArrayUtils.isEmpty
  let hasOnlyOneCorrectAnswerOption =
    t.answerOptions
    ->Js.Array.filter(answerOption =>
      answerOption->CurriculumEditor__AnswerOption.correctAnswer == true
    )
    ->Array.length == 1
  validQuestion && (hasZeroInvalidAnswerOptions && hasOnlyOneCorrectAnswerOption)
}

let makeFromJs = quizData => {
  id: quizData["id"],
  question: quizData["question"],
  answerOptions: quizData["answerOptions"]->Array.map(answerOption =>
    answerOption->CurriculumEditor__AnswerOption.makeFromJs
  ),
}

let quizAsJsObject = quiz =>
  quiz->Array.map(quiz =>
    {
      "question": quiz.question,
      "answerOptions": CurriculumEditor__AnswerOption.quizAnswersAsJsObject(quiz.answerOptions),
    }
  )
