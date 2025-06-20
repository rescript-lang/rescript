%raw(`require("./QuestionsShow.css")`)

open QuestionsShow__Types

let str = React.string

type action =
  | AddComment(Comment.t)
  | AddAnswer(Answer.t, bool)
  | AddLike(Like.t)
  | RemoveLike(string)
  | UpdateShowAnswerCreate(bool)
  | UpdateShowQuestionEdit(bool)
  | UpdateQuestion(Question.t)
  | UpdateAnswer(Answer.t)
  | RemoveAnswer(string)
  | RemoveComment(string)

type state = {
  question: Question.t,
  answers: list<Answer.t>,
  comments: list<Comment.t>,
  likes: list<Like.t>,
  showAnswerCreate: bool,
  showQuestionEdit: bool,
}

let reducer = (state, action) =>
  switch action {
  | AddComment(comment) => {
      ...state,
      comments: Comment.addComment(state.comments, comment),
    }
  | AddAnswer(answer, bool) => {
      ...state,
      answers: Answer.addAnswer(state.answers, answer),
      showAnswerCreate: bool,
    }

  | AddLike(like) => {...state, likes: state.likes->Like.addLike(like)}
  | RemoveLike(id) => {...state, likes: state.likes->Like.removeLike(id)}
  | UpdateShowAnswerCreate(bool) => {...state, showAnswerCreate: bool}
  | UpdateShowQuestionEdit(bool) => {...state, showQuestionEdit: bool}
  | UpdateQuestion(question) => {...state, question: question, showQuestionEdit: false}
  | RemoveAnswer(id) => {
      ...state,
      answers: state.answers->Answer.delete(id),
    }
  | RemoveComment(id) => {
      ...state,
      comments: state.comments->Comment.delete(id),
    }
  | UpdateAnswer(answer) => {
      ...state,
      answers: Answer.updateAnswer(state.answers, answer),
    }
  }

let showAnswersCreateComponent = (answers, showAnswerCreate, currentUserId) =>
  if showAnswerCreate {
    true
  } else {
    answers->Answer.answerFromUser(currentUserId)->ListUtils.isEmpty
  }

let likesForAnswer = (likes, answerId) => likes->Like.likesForAnswer(answerId)->List.length

let handleUpdateQuestion = (title, description, currentUserId, question, dispatch) => {
  let newQuestion = Question.create(
    question->Question.id,
    title,
    description,
    question->Question.creatorId,
    Some(currentUserId),
    question->Question.createdAt,
    question->Question.updatedAt,
  )
  dispatch(UpdateQuestion(newQuestion))
}

@react.component
let make = (
  ~question,
  ~answers,
  ~comments,
  ~users,
  ~likes,
  ~currentUserId,
  ~communityPath,
  ~isCoach,
  ~communityId,
  ~target,
) => {
  let (state, dispatch) = React.useReducer(
    reducer,
    {
      question: question,
      answers: answers,
      comments: comments,
      likes: likes,
      showAnswerCreate: false,
      showQuestionEdit: false,
    },
  )
  let addCommentCB = comment => dispatch(AddComment(comment))
  let handleAnswerCB = (answer, newAnswer) =>
    newAnswer ? dispatch(AddAnswer(answer, false)) : dispatch(UpdateAnswer(answer))
  let addLikeCB = like => dispatch(AddLike(like))
  let removeLikeCB = id => dispatch(RemoveLike(id))
  let updateQuestionCB = (title, description) =>
    handleUpdateQuestion(title, description, currentUserId, question, dispatch)
  let archiveCB = (id, resourceType) =>
    switch resourceType {
    | "Question" => communityPath->Webapi.Dom.Window.setLocation(Webapi.Dom.window)
    | "Answer" => dispatch(RemoveAnswer(id))
    | "Comment" => dispatch(RemoveComment(id))
    | _ => Notification.error("Something went wrong", "Please refresh the page and try again")
    }
  let filteredAnswers = state.answers->List.filter(answer => !(answer->Answer.archived))

  <div className="bg-gray-100">
    <div className="flex-col items-center justify-between">
      {state.showQuestionEdit
        ? <div>
            <div className="max-w-3xl w-full mx-auto mt-5 pb-2 px-3 lg:px-0">
              <a
                id="close-button"
                className="btn btn-subtle cursor-default"
                onClick={event => {
                  event->ReactEvent.Mouse.preventDefault
                  dispatch(UpdateShowQuestionEdit(false))
                }}>
                <i className="fas fa-arrow-left" /> <span className="ml-2"> {"Close"->str} </span>
              </a>
            </div>
            <QuestionsShow__QuestionEditor
              communityId target=None showBackButton=false question=state.question updateQuestionCB
            />
          </div>
        : <div className="flex flex-col px-3 lg:px-0">
            <div className="max-w-3xl w-full mx-auto mt-5 pb-2">
              <a className="btn btn-subtle" href=communityPath>
                <i className="fas fa-arrow-left" /> <span className="ml-2"> {"Back"->str} </span>
              </a>
            </div>
            {switch target {
            | Some(target) =>
              <div className="max-w-3xl w-full mt-5 mx-auto">
                <div
                  className="flex py-4 px-4 md:px-5 w-full bg-white border border-primary-500 shadow-md rounded-lg justify-between items-center mb-2">
                  <p className="w-3/5 md:w-4/5 text-sm">
                    <span className="font-semibold block text-xs">
                      {"Linked Target: "->str}
                    </span>
                    <span> {target->Target.title->str} </span>
                  </p>
                  {switch target->Target.id {
                  | Some(id) =>
                    <a href={"/targets/" ++ id} className="btn btn-default">
                      {"View Target"->str}
                    </a>
                  | None => React.null
                  }}
                </div>
              </div>
            | None => React.null
            }}
            <div
              className="max-w-3xl w-full flex mx-auto items-center justify-center relative border-t border-b md:border-0 bg-white rounded md:rounded-lg shadow overflow-hidden z-10">
              <div className="flex w-full">
                <div title="Question block" className="flex flex-col w-full relative">
                  <div
                    className="absolute right-0 top-0 flex border border-t-0 border-r-0 border-gray-400 bg-gray-200 rounded-bl">
                    {switch state.question->Question.editorId {
                    | Some(_) =>
                      <a
                        href={"/questions/" ++ ((state.question->Question.id) ++ "/versions")}
                        title="Edit History"
                        className="inline-flex items-center whitespace-no-wrap text-xs font-semibold py-1 px-3 bg-transparent hover:bg-primary-100 hover:text-primary-500 cursor-pointer text-gray-800 border-r border-gray-400">
                        <i className="fas fa-history text-sm" />
                        <span className="ml-2"> {"History"->str} </span>
                      </a>
                    | None => React.null
                    }}
                    {state.question->Question.creatorId == currentUserId || isCoach
                      ? <div className="flex">
                          <a
                            onClick={_ => dispatch(UpdateShowQuestionEdit(true))}
                            title="Edit Question"
                            className="inline-flex items-center whitespace-no-wrap text-xs font-semibold py-1 px-3 bg-transparent hover:bg-primary-100 hover:text-primary-500 text-gray-800 border-r border-gray-400 cursor-pointer">
                            <i className="fas fa-edit text-sm" />
                            <span className="ml-2"> {"Edit"->str} </span>
                          </a>
                          <QuestionsShow__ArchiveManager
                            id={question->Question.id} resourceType="Question" archiveCB
                          />
                        </div>
                      : React.null}
                  </div>
                  <div className="pt-7 px-3 md:px-6 flex flex-col">
                    <h1 className="text-base md:text-xl text-black font-semibold break-words">
                      {state.question->Question.title->str}
                    </h1>
                  </div>
                  <div className="pb-4 pt-2 px-3 md:px-6 flex flex-col">
                    <MarkdownBlock
                      markdown={state.question->Question.description}
                      className="leading-normal text-sm"
                      profile=Markdown.QuestionAndAnswer
                    />
                    {switch state.question->Question.editorId {
                    | Some(editorId) =>
                      <div>
                        <div
                          className="text-xs mt-2 inline-block px-2 py-1 rounded bg-orange-100 text-orange-900">
                          <span> {"Last edited by "->str} </span>
                          <span className="font-semibold">
                            {users->User.findById(editorId)->User.name->str}
                          </span>
                          <span>
                            {" on " ++
                            (state.question
                            ->Question.updatedAt
                            ->DateTime.stingToFormatedTime(DateTime.DateWithYearAndTime))->str}
                          </span>
                        </div>
                      </div>
                    | None => React.null
                    }}
                  </div>
                  <div className="flex flex-row justify-between px-3 md:px-6 pb-6">
                    <div className="pr-2 pt-6 text-center">
                      <i className="far fa-comments text-xl text-gray-600" />
                      <p className="text-xs py-1">
                        {state.comments
                        ->Comment.commentsForQuestion
                        ->List.length
                        ->string_of_int
                        ->str}
                      </p>
                    </div>
                    <QuestionsShow__UserShow
                      user={users->User.findById(state.question->Question.creatorId)}
                      createdAt={state.question->Question.createdAt}
                      textForTimeStamp="Asked"
                    />
                  </div>
                </div>
              </div>
            </div>
            <QuestionsShow__CommentShow
              comments={state.comments->Comment.commentsForQuestion}
              users
              commentableType="Question"
              commentableId={state.question->Question.id}
              addCommentCB
              currentUserId
              archiveCB
              isCoach
            />
            <div className="max-w-3xl w-full justify-center mx-auto pt-8 pb-2 border-b">
              <div className="flex items-end">
                <span className="text-lg font-semibold">
                  {
                    let numberOfAnswers = filteredAnswers->List.length
                    (numberOfAnswers->string_of_int) ++ (
                        numberOfAnswers == 1 ? " Answer" : " Answers"
                      )->str
                  }
                </span>
              </div>
            </div>
            <div className="community-answer-container">
              {filteredAnswers
              ->List.sort((answerA, answerB) =>
                DateFns.differenceInSeconds(
                  answerB->Answer.createdAt->DateFns.parseString,
                  answerA->Answer.createdAt->DateFns.parseString,
                )->int_of_float
              )
              ->List.stable_sort((answerA, answerB) =>
                likesForAnswer(likes, answerB->Answer.id) -
                likesForAnswer(likes, answerA->Answer.id)
              )
              ->List.map(answer =>
                <QuestionsShow__AnswerShow
                  key={answer->Answer.id}
                  answer
                  question=state.question
                  addCommentCB
                  currentUserId
                  addLikeCB
                  removeLikeCB
                  users
                  comments=state.comments
                  likes=state.likes
                  handleAnswerCB
                  isCoach
                  archiveCB
                />
              )
              ->Array.of_list
              ->ReasonReact.array}
            </div>
            {showAnswersCreateComponent(state.answers, state.showAnswerCreate, currentUserId)
              ? <QuestionsShow__AnswerEditor question=state.question currentUserId handleAnswerCB />
              : <div
                  className="community-ask-button-container mt-4 my-8 max-w-3xl w-full flex mx-auto justify-center">
                  <div className="bg-gray-100 px-1 z-10">
                    <button
                      className="btn btn-primary"
                      onClick={_ => dispatch(UpdateShowAnswerCreate(true))}>
                      {"Add another answer"->str}
                    </button>
                  </div>
                </div>}
          </div>}
    </div>
  </div>
}
