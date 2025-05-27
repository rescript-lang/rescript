open QuestionsShow__Types

let str = React.string

@val external currentTime: unit => string = "Date.now"

module CreateAnswerQuery = %graphql(`
  mutation CreateAnswerMutation($description: String!, $questionId: ID!) {
    createAnswer(description: $description, questionId: $questionId) @bsVariant {
      answerId
      errors
    }
  }
`)

module UpdateAnswerQuery = %graphql(`
  mutation UpdateAnswerMutation($id: ID!, $description: String!) {
    updateAnswer(id: $id, description: $description) @bsVariant {
      success
      errors
    }
  }
`)

module CreateAnswerError = {
  type t = [#InvalidLengthDescription | #BlankQuestionId]

  let notification = error =>
    switch error {
    | #InvalidLengthDescription => (
        "InvalidLengthDescription",
        "Supplied description must be greater than 1 characters in length",
      )
    | #BlankQuestionId => ("BlankQuestionId", "Question id is required for creating Answer")
    }
}

module UpdateAnswerError = {
  type t = [#InvalidLengthDescription]

  let notification = error =>
    switch error {
    | #InvalidLengthDescription => (
        "InvalidLengthDescription",
        "Supplied description must be greater than 1 characters in length",
      )
    }
}

module CreateAnswerErrorHandler = GraphqlErrorHandler.Make(CreateAnswerError)
module UpdateAnswerErrorHandler = GraphqlErrorHandler.Make(UpdateAnswerError)

let dateTime = currentTime()

let handleAnswerCreateCB = (
  id,
  description,
  currentUserId,
  setDescription,
  setSaving,
  handleAnswerCB,
) => {
  let answer = Answer.create(id, description, currentUserId, None, dateTime, dateTime, false)
  setDescription(_ => "")
  setSaving(_ => false)
  handleAnswerCB(answer, true)
}

let handleAnswerUpdateResponseCB = (
  id,
  description,
  currentUserId,
  setDescription,
  setSaving,
  handleAnswerCB,
  answer,
) => {
  let newAnswer = Answer.create(
    id,
    description,
    answer->Answer.creatorId,
    Some(currentUserId),
    answer->Answer.createdAt,
    dateTime,
    false,
  )
  setDescription(_ => "")
  setSaving(_ => false)
  handleAnswerCB(newAnswer, false)
}

let handleAnswer = (
  description,
  question,
  setSaving,
  currentUserId,
  setDescription,
  handleAnswerCB,
  answer,
  event,
) => {
  event->ReactEvent.Mouse.preventDefault
  if description != "" {
    setSaving(_ => true)

    switch answer {
    | Some(answer) =>
      let answerId = answer->Answer.id

      UpdateAnswerQuery.make(~description, ~id=answerId, ())
      ->GraphqlQuery.sendQuery
      ->Js.Promise.then_(response =>
        switch response["updateAnswer"] {
        | #Success(answerUpdated) =>
          answerUpdated
            ? handleAnswerUpdateResponseCB(
                answerId,
                description,
                currentUserId,
                setDescription,
                setSaving,
                handleAnswerCB,
                answer,
              )
            : Notification.error("Something went wrong", "Please refresh the page and try again")
          Notification.success("Success", "Answer updated successfully")
          Js.Promise.resolve()
        | #Errors(errors) => Js.Promise.reject(UpdateAnswerErrorHandler.Errors(errors))
        }
      )
      ->UpdateAnswerErrorHandler.catch(() => setSaving(_ => false))
      ->ignore
    | None =>
      CreateAnswerQuery.make(~description, ~questionId=question->Question.id, ())
      ->GraphqlQuery.sendQuery
      ->Js.Promise.then_(response =>
        switch response["createAnswer"] {
        | #AnswerId(answerId) =>
          handleAnswerCreateCB(
            answerId,
            description,
            currentUserId,
            setDescription,
            setSaving,
            handleAnswerCB,
          )
          Notification.success("Done!", "Answer has been saved.")
          Js.Promise.resolve()
        | #Errors(errors) => Js.Promise.reject(CreateAnswerErrorHandler.Errors(errors))
        }
      )
      ->CreateAnswerErrorHandler.catch(() => setSaving(_ => false))
      ->ignore
    }
  } else {
    Notification.error("Empty", "Answer cant be blank")
  }
}

@react.component
let make = (~question, ~currentUserId, ~handleAnswerCB, ~answer=?, ~handleCloseCB=?) => {
  let (description, setDescription) = React.useState(() =>
    switch answer {
    | Some(answer) => answer->Answer.description
    | None => ""
    }
  )
  let (saving, setSaving) = React.useState(() => false)
  let updateMarkdownCB = description => setDescription(_ => description)
  <DisablingCover disabled=saving>
    <div className="py-2 max-w-3xl w-full flex mx-auto items-center justify-center relative">
      <div className="flex w-full pb-4">
        <div className="w-full flex flex-col">
          <label
            className="inline-block tracking-wide text-gray-900 text-lg font-semibold mb-2"
            htmlFor="new-answer">
            {"Your Answer"->str}
          </label>
          <MarkdownEditor
            placeholder="Type in your answer. You can use Markdown to format your response."
            textareaId="new-answer"
            onChange=updateMarkdownCB
            value=description
            profile=Markdown.QuestionAndAnswer
            maxLength=10000
          />
          <div className="flex justify-end pt-3 border-t">
            {switch handleCloseCB {
            | Some(handleCloseCB) =>
              <button
                disabled=saving onClick={_ => handleCloseCB()} className="btn btn-default mr-2">
                {"Cancel"->str}
              </button>
            | None => React.null
            }}
            <button
              disabled={saving || description == ""}
              onClick={handleAnswer(
                description,
                question,
                setSaving,
                currentUserId,
                setDescription,
                handleAnswerCB,
                answer,
              )}
              className="btn btn-primary">
              {switch answer {
              | Some(_) => "Update Your Answer"
              | None => "Post Your Answer"
              }->str}
            </button>
          </div>
        </div>
      </div>
    </div>
  </DisablingCover>
}
