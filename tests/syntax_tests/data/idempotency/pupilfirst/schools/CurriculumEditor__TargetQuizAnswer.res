let str = React.string

let correctAnswerOptionClasses = bool =>
  "relative mb-2 overflow-hidden " ++ (bool ? "quiz-maker__answer-option-correct" : "")

@react.component
let make = (
  ~answerOption: CurriculumEditor__AnswerOption.t,
  ~updateAnswerOptionCB,
  ~removeAnswerOptionCB,
  ~canBeDeleted,
  ~markAsCorrectCB,
  ~answerOptionId,
) =>
  <div
    className={correctAnswerOptionClasses(
      answerOption->CurriculumEditor__AnswerOption.correctAnswer,
    )}>
    {answerOption->CurriculumEditor__AnswerOption.correctAnswer
      ? <div
          className="quiz-maker__answer-option-pointer flex justify-center items-center quiz-maker__answer-option-pointer--correct">
          <Icon className="if i-check-light text-xs" />
        </div>
      : <div
          onClick={_event => {
            ReactEvent.Mouse.preventDefault(_event)
            markAsCorrectCB(answerOption->CurriculumEditor__AnswerOption.id)
          }}
          className="quiz-maker__answer-option-pointer cursor-pointer">
          ReasonReact.null
        </div>}
    <div
      id={answerOptionId ++ "-block"}
      className="quiz-maker__answer-option-answer flex flex-col bg-white border border-gray-400 rounded-lg ml-12">
      <div className="flex">
        <textarea
          id=answerOptionId
          className="appearance-none block w-full bg-white text-gray-800 text-sm rounded-lg px-4 py-3 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
          placeholder="Answer option (supports markdown)"
          value={answerOption->CurriculumEditor__AnswerOption.answer}
          onChange={event =>
            updateAnswerOptionCB(
              answerOption->CurriculumEditor__AnswerOption.id,
              answerOption->CurriculumEditor__AnswerOption.updateAnswer(
                ReactEvent.Form.target(event)["value"],
              ),
            )}
        />
        <button
          className={answerOption->CurriculumEditor__AnswerOption.correctAnswer
            ? "w-28 flex-shrink-0 border-l border-gray-400 text-green-600 font-semibold cursor-default focus:outline-none text-xs py-1 px-2"
            : "w-28 flex-shrink-0 border-l border-gray-400 text-gray-800 hover:text-gray-900 focus:outline-none text-xs py-1 px-2"}
          type_="button"
          onClick={_event => {
            ReactEvent.Mouse.preventDefault(_event)
            markAsCorrectCB(answerOption->CurriculumEditor__AnswerOption.id)
          }}>
          {answerOption->CurriculumEditor__AnswerOption.correctAnswer
            ? "Correct Answer"->str
            : "Mark as correct"->str}
        </button>
        {canBeDeleted
          ? <button
              className="flex-shrink-0 border-l border-gray-400 text-gray-600 hover:text-gray-900 focus:outline-none text-xs py-1 px-3"
              type_="button"
              title="Remove this answer option"
              onClick={event => {
                ReactEvent.Mouse.preventDefault(event)
                removeAnswerOptionCB(answerOption->CurriculumEditor__AnswerOption.id)
              }}>
              <i className="fas fa-trash-alt text-sm" />
            </button>
          : ReasonReact.null}
      </div>
    </div>
  </div>
