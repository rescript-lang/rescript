@val external currentTime: unit => string = "Date.now"

open QuestionsShow__Types

let str = React.string

module CreateCommentQuery = %graphql(`
  mutation CreateCommentMutation($value: String!, $commentableId: ID!, $commentableType: String!) {
    createComment(value: $value, commentableId: $commentableId, commentableType: $commentableType) @bsVariant {
      commentId
      errors
    }
  }
`)

module CreateCommentError = {
  type t = [
    | #InvalidCommentableType
    | #InvalidLengthValue
    | #BlankCommentableId
  ]

  let notification = error =>
    switch error {
    | #InvalidCommentableType => (
        "InvalidCommentableType",
        "Supplied type must be one of Question or Answer",
      )
    | #InvalidLengthValue => (
        "InvalidLengthValue",
        "Supplied comment must be greater than 1 characters in length",
      )
    | #BlankCommentableId => (
        "BlankCommentableId",
        "Commentable id is required for creating a Comment",
      )
    }
}

module CreateCommentErrorHandler = GraphqlErrorHandler.Make(CreateCommentError)

@react.component
let make = (~commentableType, ~commentableId, ~addCommentCB, ~currentUserId) => {
  let (value, setValue) = React.useState(() => "")
  let (saving, setSaving) = React.useState(() => false)
  let validComment = value != ""

  let handleResponseCB = id => {
    let comment = Comment.create(
      id,
      value,
      currentUserId,
      commentableId,
      commentableType,
      currentTime(),
    )
    setValue(_ => "")
    setSaving(_ => false)
    addCommentCB(comment)
  }

  let handleCreateComment = event => {
    event->ReactEvent.Mouse.preventDefault

    if validComment {
      setSaving(_ => true)

      CreateCommentQuery.make(~value, ~commentableId, ~commentableType, ())
      ->GraphqlQuery.sendQuery
      ->Js.Promise.then_(response =>
        switch response["createComment"] {
        | #CommentId(commentId) =>
          handleResponseCB(commentId)
          Notification.success("Done!", "Comment has been saved.")
          Js.Promise.resolve()
        | #Errors(errors) => Js.Promise.reject(CreateCommentErrorHandler.Errors(errors))
        }
      )
      ->CreateCommentErrorHandler.catch(() => setSaving(_ => false))
      ->ignore
    } else {
      ()
    }
  }
  <div className="w-full flex flex-col mx-auto items-center justify-center">
    <div
      className="w-full border rounded-b border-dashed border-gray-500 border-t-0 overflow-hidden focus:border-gray-600">
      <DisablingCover disabled=saving containerClasses="flex flex-row">
        <input
          title={"Add your comment for " ++ commentableType}
          placeholder="Add your comment"
          value
          onChange={event => setValue(ReactEvent.Form.target(event)["value"])}
          className="community-qa-comment__input text-xs text-left bg-gray-200 py-3 px-4 rounded-b appearance-none block w-full leading-tight hover:bg-gray-100 focus:outline-none focus:bg-white"
        />
        {validComment
          ? <button
              onClick=handleCreateComment
              className="flex items-center whitespace-no-wrap text-sm font-semibold py-2 px-4 btn-primary appearance-none focus:outline-none text-center">
              {"Comment"->str}
            </button>
          : ReasonReact.null}
      </DisablingCover>
    </div>
  </div>
}
