let str = React.string

open QuestionsShow__Types

type similar = {
  search: string,
  suggestions: array<QuestionSuggestion.t>,
}

type state = {
  title: string,
  titleTimeoutId: option<Js.Global.timeoutId>,
  similar: similar,
  searching: bool,
  description: string,
  saving: bool,
}

let computeInitialState = question => {
  let (title, description) = switch question {
  | Some(question) => (question->Question.title, question->Question.description)
  | None => ("", "")
  }

  {
    title: title,
    description: description,
    titleTimeoutId: None,
    similar: {
      search: "",
      suggestions: [],
    },
    searching: false,
    saving: false,
  }
}

type action =
  | UpdateTitle(string)
  | UpdateTitleAndTimeout(string, Js.Global.timeoutId)
  | UpdateDescription(string)
  | BeginSaving
  | FailSaving
  | BeginSearching
  | FinishSearching(string, array<QuestionSuggestion.t>)
  | FailSearching

let reducer = (state, action) =>
  switch action {
  | UpdateTitle(title) =>
    let similar = title->String.trim == "" ? {search: "", suggestions: []} : state.similar

    {...state, title: title, similar: similar}
  | UpdateTitleAndTimeout(title, timeoutId) => {
      ...state,
      title: title,
      titleTimeoutId: Some(timeoutId),
    }
  | UpdateDescription(description) => {...state, description: description}
  | BeginSaving => {...state, saving: true}
  | FailSaving => {...state, saving: false}
  | BeginSearching => {...state, searching: true}
  | FinishSearching(search, suggestions) => {
      ...state,
      searching: false,
      similar: {
        search: search,
        suggestions: suggestions,
      },
    }
  | FailSearching => {...state, searching: false}
  }

module SimilarQuestionsQuery = %graphql(`
    query SimilarQuestionsQuery($communityId: ID!, $title: String!) {
      similarQuestions(communityId: $communityId, title: $title) {
        id
        title
        createdAt
        answersCount
      }
    }
  `)

let searchForSimilarQuestions = (send, title, communityId, ()) => {
  send(BeginSearching)

  let trimmedTitle = title->String.trim

  SimilarQuestionsQuery.make(~communityId, ~title=trimmedTitle, ())
  ->GraphqlQuery.sendQuery
  ->Js.Promise.then_(result => {
    let suggestions = result["similarQuestions"]->Array.map(QuestionSuggestion.makeFromJs)
    send(FinishSearching(trimmedTitle, suggestions))
    Js.Promise.resolve()
  })
  ->Js.Promise.catch(e => {
    Js.log(e)
    Notification.warn(
      "Oops!",
      "We failed to fetch similar questions from the server! Our team has been notified about this error.",
    )
    send(FailSaving)
    Js.Promise.resolve()
  })
  ->ignore
}

let isInvalidString = s => s->String.trim == ""

let updateTitleAndSearch = (state, send, communityId, title) => {
  state.titleTimeoutId->Belt.Option.forEach(Js.Global.clearTimeout)

  let trimmedTitle = title->String.trim

  if title->isInvalidString || trimmedTitle == state.similar.search {
    send(UpdateTitle(title))
  } else {
    let timeoutId = Js.Global.setTimeout(
      searchForSimilarQuestions(send, trimmedTitle, communityId),
      1500,
    )

    send(UpdateTitleAndTimeout(title, timeoutId))
  }
}

module CreateQuestionQuery = %graphql(`
  mutation CreateQuestionQuery($title: String!, $description: String!, $communityId: ID!, $targetId: ID) {
    createQuestion(description: $description, title: $title, communityId: $communityId, targetId: $targetId) @bsVariant {
      questionId
      errors
    }
  }
`)

module UpdateQuestionQuery = %graphql(`
  mutation UpdateQuestionQuery($id: ID!, $title: String!, $description: String!) {
    updateQuestion(id: $id, title: $title, description: $description) @bsVariant {
      success
      errors
    }
  }
`)

module CreateQuestionError = {
  type t = [
    | #InvalidLengthTitle
    | #InvalidLengthDescription
    | #BlankCommunityID
  ]

  let notification = error =>
    switch error {
    | #InvalidLengthTitle => (
        "InvalidLengthTitle",
        "Supplied title must be between 1 and 250 characters in length",
      )
    | #InvalidLengthDescription => (
        "InvalidLengthDescription",
        "Supplied description must be greater than 1 characters in length",
      )
    | #BlankCommunityID => ("BlankCommunityID", "Community id is required for creating Answer")
    }
}

module UpdateQuestionError = {
  type t = [#InvalidLengthTitle | #InvalidLengthDescription]

  let notification = error =>
    switch error {
    | #InvalidLengthTitle => (
        "InvalidLengthTitle",
        "Supplied title must be between 1 and 250 characters in length",
      )
    | #InvalidLengthDescription => (
        "InvalidLengthDescription",
        "Supplied description must be greater than 1 characters in length",
      )
    }
}

let handleResponseCB = (id, title) => {
  let window = Webapi.Dom.window
  let parameterizedTitle =
    title->Js.String.toLowerCase->Js.String.replaceByRe(/[^0-9a-zA-Z]+/gi, "-")
  let redirectPath = "/questions/" ++ (id ++ ("/" ++ parameterizedTitle))
  redirectPath->Webapi.Dom.Window.setLocation(window)
}

let handleBack = () => Webapi.Dom.window->Webapi.Dom.Window.history->Webapi.Dom.History.back

module CreateQuestionErrorHandler = GraphqlErrorHandler.Make(CreateQuestionError)

module UpdateQuestionErrorHandler = GraphqlErrorHandler.Make(UpdateQuestionError)

let saveDisabled = state => state.description->isInvalidString || state.title->isInvalidString

let handleCreateOrUpdateQuestion = (
  state,
  send,
  communityId,
  target,
  question,
  updateQuestionCB,
  event,
) => {
  event->ReactEvent.Mouse.preventDefault

  if !saveDisabled(state) {
    send(BeginSaving)

    switch question {
    | Some(question) =>
      let id = question->Question.id
      UpdateQuestionQuery.make(~id, ~title=state.title, ~description=state.description, ())
      ->GraphqlQuery.sendQuery
      ->Js.Promise.then_(response =>
        switch response["updateQuestion"] {
        | #Success(updated) =>
          switch (updated, updateQuestionCB) {
          | (true, Some(questionCB)) =>
            questionCB(state.title, state.description)
            Notification.success("Done!", "Question Updated sucessfully")
          | (_, _) =>
            Notification.error("Something went wrong", "Please refresh the page and try again")
          }
          Js.Promise.resolve()
        | #Errors(errors) => Js.Promise.reject(UpdateQuestionErrorHandler.Errors(errors))
        }
      )
      ->UpdateQuestionErrorHandler.catch(() => send(FailSaving))
      ->ignore
    | None =>
      let targetId = target->OptionUtils.map(LinkedTarget.id)

      CreateQuestionQuery.make(
        ~description=state.description,
        ~title=state.title,
        ~communityId,
        ~targetId?,
        (),
      )
      ->GraphqlQuery.sendQuery
      ->Js.Promise.then_(response =>
        switch response["createQuestion"] {
        | #QuestionId(questionId) =>
          handleResponseCB(questionId, state.title)
          Notification.success("Done!", "Question has been saved.")
          Js.Promise.resolve()
        | #Errors(errors) => Js.Promise.reject(CreateQuestionErrorHandler.Errors(errors))
        }
      )
      ->CreateQuestionErrorHandler.catch(() => send(FailSaving))
      ->ignore
    }
  } else {
    Notification.error("Error!", "Question title and description must be present.")
  }
}

let suggestions = state => {
  let suggestions = state.similar.suggestions

  suggestions->ArrayUtils.isNotEmpty
    ? <div className="pt-3">
        <span className="tracking-wide text-gray-900 text-xs font-semibold">
          {"Similar Questions"->str}
        </span>
        {state.searching
          ? <span className="ml-2"> <FaIcon classes="fa fa-spinner fa-pulse" /> </span>
          : React.null}
        {suggestions
        ->Array.map(suggestion => {
          let askedOn =
            suggestion->QuestionSuggestion.createdAt->DateTime.format(DateTime.OnlyDate)
          let (answersText, answersClasses) = switch suggestion->QuestionSuggestion.answersCount {
          | 0 => ("No answers", "bg-gray-300 text-gray-700")
          | 1 => ("1 answer", "bg-green-500 text-white")
          | n => ((n->string_of_int) ++ " answers", "bg-green-500 text-white")
          }

          <a
            href={"/questions/" ++ (suggestion->QuestionSuggestion.id)}
            target="_blank"
            key={suggestion->QuestionSuggestion.id}
            className="flex w-full items-center justify-between mt-1 p-3 rounded cursor-pointer border bg-gray-100 hover:text-primary-500 hover:bg-gray-200">
            <div className="flex flex-col min-w-0">
              <h5
                title={suggestion->QuestionSuggestion.title}
                className="font-semibold text-sm leading-snug md:text-base pr-1 truncate flex-1">
                {suggestion->QuestionSuggestion.title->str}
              </h5>
              <p className="text-xs mt-1 leading-tight text-gray-800">
                {"Asked on " ++ askedOn->str}
              </p>
            </div>
            <div
              className={"text-xs px-1 py-px ml-2 rounded font-semibold flex-shrink-0 " ++
              answersClasses}>
              {answersText->str}
            </div>
          </a>
        })
        ->React.array}
      </div>
    : React.null
}

let searchingIndicator = state =>
  state.similar.suggestions->ArrayUtils.isEmpty && state.searching
    ? <div className="md:flex-1 pl-1 pb-3 md:p-0">
        <FaIcon classes="fas fa-spinner fa-pulse" />
      </div>
    : React.null

@react.component
let make = (~communityId, ~showBackButton=true, ~target, ~question=?, ~updateQuestionCB=?) => {
  let (state, send) = React.useReducerWithMapState(reducer, question, computeInitialState)
  <DisablingCover disabled=state.saving>
    <div className="bg-gray-100">
      <div className="flex-1 flex flex-col">
        <div className="px-3 lg:px-0">
          {showBackButton
            ? <div className="max-w-3xl w-full mx-auto mt-5 pb-2">
                <a className="btn btn-subtle" onClick={_ => handleBack()}>
                  <i className="fas fa-arrow-left" />
                  <span className="ml-2"> {"Back"->str} </span>
                </a>
              </div>
            : React.null}
        </div>
        {switch target {
        | Some(target) =>
          <div className="max-w-3xl w-full mt-5 mx-auto px-3 lg:px-0">
            <div
              className="flex py-4 px-4 md:px-5 w-full bg-white border border-primary-500  shadow-md rounded-lg justify-between items-center mb-2">
              <p className="w-3/5 md:w-4/5 text-sm">
                <span className="font-semibold block text-xs"> {"Linked Target: "->str} </span>
                <span> {target->LinkedTarget.title->str} </span>
              </p>
              <a href="./new_question" className="btn btn-default"> {"Clear"->str} </a>
            </div>
          </div>
        | None => React.null
        }}
        <h4 className="max-w-3xl w-full mx-auto pb-2 mt-2 px-3 lg:px-0">
          {switch question {
          | Some(_) => "Edit question"
          | None => "Ask a new question"
          }->str}
        </h4>
        <div className="md:px-3">
          <div
            className="mb-8 max-w-3xl w-full mx-auto relative border-t border-b md:border-0 bg-white md:shadow md:rounded-lg">
            <div className="flex w-full flex-col p-3 md:p-6">
              <label
                className="inline-block tracking-wide text-gray-900 text-xs font-semibold mb-2"
                htmlFor="title">
                {"Question"->str}
              </label>
              <input
                id="title"
                value=state.title
                className="appearance-none block w-full bg-white text-gray-900 font-semibold border border-gray-400 rounded py-3 px-4 mb-4 leading-tight focus:outline-none focus:bg-white focus:border-gray-500"
                onChange={event => {
                  let newTitle = ReactEvent.Form.target(event)["value"]

                  switch question {
                  | Some(_) => send(UpdateTitle(newTitle))
                  | None => updateTitleAndSearch(state, send, communityId, newTitle)
                  }
                }}
                placeholder="Ask your question here briefly."
              />
              <label
                className="inline-block tracking-wide text-gray-900 text-xs font-semibold mb-2"
                htmlFor="description">
                {"Description"->str}
              </label>
              <div className="w-full flex flex-col">
                <MarkdownEditor
                  textareaId="description"
                  onChange={markdown => send(UpdateDescription(markdown))}
                  value=state.description
                  placeholder="Your description gives people the information they need to help you answer your question. You can use Markdown to format this text."
                  profile=Markdown.QuestionAndAnswer
                  maxLength=10000
                />
                <div>
                  {suggestions(state)}
                  <div
                    className="flex flex-col md:flex-row justify-end mt-3 items-center md:items-start">
                    {searchingIndicator(state)}
                    <button
                      disabled={saveDisabled(state)}
                      onClick={handleCreateOrUpdateQuestion(
                        state,
                        send,
                        communityId,
                        target,
                        question,
                        updateQuestionCB,
                      )}
                      className="btn btn-primary border border-transparent w-full md:w-auto">
                      {switch question {
                      | Some(_) => "Update Question"
                      | None => "Post Your Question"
                      }->str}
                    </button>
                  </div>
                </div>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  </DisablingCover>
}
