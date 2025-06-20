let str = React.string

@react.component
let make = (
  ~questionNumber,
  ~quizQuestion,
  ~updateQuizQuestionCB,
  ~removeQuizQuestionCB,
  ~questionCanBeRemoved,
) => {
  let answerOptionId = (questionId, index) =>
    questionId ++ ("-answer-option-" ++ (index + 1->string_of_int))

  let updateQuestion = question =>
    updateQuizQuestionCB(
      quizQuestion->CurriculumEditor__QuizQuestion.id,
      quizQuestion->CurriculumEditor__QuizQuestion.updateQuestion(question),
    )

  let updateAnswerOptionCB = (id, answer) =>
    updateQuizQuestionCB(
      quizQuestion->CurriculumEditor__QuizQuestion.id,
      quizQuestion->CurriculumEditor__QuizQuestion.replace(id, answer),
    )
  let removeAnswerOptionCB = id =>
    updateQuizQuestionCB(
      quizQuestion->CurriculumEditor__QuizQuestion.id,
      quizQuestion->CurriculumEditor__QuizQuestion.removeAnswerOption(id),
    )
  let markAsCorrectCB = id =>
    updateQuizQuestionCB(
      quizQuestion->CurriculumEditor__QuizQuestion.id,
      quizQuestion->CurriculumEditor__QuizQuestion.markAsCorrect(id),
    )

  let addAnswerOption = () =>
    updateQuizQuestionCB(
      quizQuestion->CurriculumEditor__QuizQuestion.id,
      quizQuestion->CurriculumEditor__QuizQuestion.newAnswerOption(
        Js.Date.now()->Js.Float.toString,
      ),
    )
  let canBeDeleted =
    quizQuestion->CurriculumEditor__QuizQuestion.answerOptions->Array.length > 2
  let questionId = "quiz-question-" ++ questionNumber

  <div className="quiz-maker__question-container p-4 bg-gray-100 rounded-lg border mt-4">
    <div className="flex items-center justify-between">
      <label
        className="block tracking-wide uppercase text-gray-800 text-sm font-bold"
        htmlFor=questionId>
        {"Question " ++ questionNumber->str}
      </label>
      <div className="quiz-maker__question-remove-button invisible">
        {questionCanBeRemoved
          ? <button
              className="flex items-center flex-shrink-0 bg-white p-2 rounded-lg text-gray-600 hover:text-gray-700 text-xs"
              type_="button"
              title="Remove Quiz Question"
              onClick={event => {
                ReactEvent.Mouse.preventDefault(event)
                removeQuizQuestionCB(quizQuestion->CurriculumEditor__QuizQuestion.id)
              }}>
              <i className="fas fa-trash-alt text-lg" />
            </button>
          : ReasonReact.null}
      </div>
    </div>
    <div className="my-2 bg-white">
      <MarkdownEditor
        textareaId=questionId
        placeholder="Type the question here (supports markdown)"
        value={quizQuestion->CurriculumEditor__QuizQuestion.question}
        onChange=updateQuestion
        profile=Markdown.Permissive
      />
    </div>
    <div className="quiz-maker__answers-container relative">
      {quizQuestion
      ->CurriculumEditor__QuizQuestion.answerOptions
      ->Array.mapi((index, answerOption) =>
        <CurriculumEditor__TargetQuizAnswer
          key={answerOption->CurriculumEditor__AnswerOption.id}
          answerOption
          updateAnswerOptionCB
          removeAnswerOptionCB
          canBeDeleted
          markAsCorrectCB
          answerOptionId={answerOptionId(questionId, index)}
        />
      )
      ->React.array}
      <div
        onClick={_event => {
          ReactEvent.Mouse.preventDefault(_event)
          addAnswerOption()
        }}
        className="quiz-maker__add-answer-option cursor-pointer relative">
        <div
          className="flex items-center border border-dashed border-primary-500 justify-center text-gray-600 quiz-maker__add-answer-option-pointer quiz-maker__add-answer-option-pointer">
          <i className="fas fa-plus" />
        </div>
        <a
          className="quiz-maker__add-answer-option-button flex items-center h-11 text-gray-900 bg-gray-200 border border-dashed border-primary-400 hover:bg-white hover:text-primary-500 hover:shadow-md rounded-lg ml-12 py-3 px-4">
          <p className="text-xs"> {"Add another Answer Option"->str} </p>
        </a>
      </div>
    </div>
  </div>
}
