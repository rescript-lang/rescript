open QuestionsShow__Types

let str = React.string

module CreateAnswerLikeQuery = %graphql(`
  mutation CreateAnswerLikeMutation($answerId: ID!) {
    createAnswerLike(answerId: $answerId) @bsVariant {
      answerLikeId
      errors
    }
  }
`)

module DestroyAnswerLikeQuery = %graphql(`
  mutation DestroyAnswerLikeMutation($id: ID!) {
    destroyAnswerLike(id: $id) {
      success
    }
  }
  `)

module CreateAnswerLikeError = {
  type t = [#LikeExists | #BlankAnswerId]

  let notification = error =>
    switch error {
    | #LikeExists => ("Oops!", "You have already liked the answer.")
    | #BlankAnswerId => ("Oops!", "Answer ID is required for adding a like.")
    }
}

module CreateAnswerLikeErrorHandler = GraphqlErrorHandler.Make(CreateAnswerLikeError)

let iconClasses = (liked, saving) => {
  let classes = "text-xl text-gray-600"
  classes ++ if saving {
    " fas fa-thumbs-up cursor-pointer text-primary-200"
  } else if liked {
    " fas fa-thumbs-up cursor-pointer text-primary-400"
  } else {
    " far fa-thumbs-up cursor-pointer"
  }
}
let handleCreateResponse = (id, currentUserId, answerId, setSaving, addLikeCB) => {
  let like = Like.create(id, currentUserId, answerId)
  setSaving(_ => false)
  addLikeCB(like)
}
let handleAnswerLike = (
  saving,
  liked,
  setSaving,
  answerId,
  currentUserId,
  likes,
  removeLikeCB,
  handleCreateResponse,
  addLikeCB,
  event,
) => {
  event->ReactEvent.Mouse.preventDefault
  saving
    ? ()
    : {
        setSaving(_ => true)
        if liked {
          let id = Like.likeByCurrentUser(answerId, currentUserId, likes)->List.hd->Like.id
          DestroyAnswerLikeQuery.make(~id, ())
          ->GraphqlQuery.sendQuery
          ->Js.Promise.then_(_response => {
            removeLikeCB(id)
            setSaving(_ => false)
            Js.Promise.resolve()
          })
          ->ignore
        } else {
          CreateAnswerLikeQuery.make(~answerId, ())
          ->GraphqlQuery.sendQuery
          ->Js.Promise.then_(response =>
            switch response["createAnswerLike"] {
            | #AnswerLikeId(answerLikeId) =>
              handleCreateResponse(answerLikeId, currentUserId, answerId, setSaving, addLikeCB)
              Js.Promise.resolve()
            | #Errors(errors) => Js.Promise.reject(CreateAnswerLikeErrorHandler.Errors(errors))
            }
          )
          ->CreateAnswerLikeErrorHandler.catch(() => setSaving(_ => false))
          ->ignore
        }
      }
}

@react.component
let make = (~likes, ~answerId, ~currentUserId, ~addLikeCB, ~removeLikeCB) => {
  let liked = likes->Like.currentUserLiked(answerId, currentUserId)
  let (saving, setSaving) = React.useState(() => false)

  <div className="mr-1 md:mr-2">
    <div
      className="cursor-pointer"
      title={(liked ? "Unlike" : "Like") ++ " Answer"}
      onClick={handleAnswerLike(
        saving,
        liked,
        setSaving,
        answerId,
        currentUserId,
        likes,
        removeLikeCB,
        handleCreateResponse,
        addLikeCB,
      )}>
      <div
        className="flex items-center justify-center rounded-full hover:bg-gray-100 h-8 w-8 md:h-10 md:w-10 p-1 md:p-2"
        key={iconClasses(liked, saving)}>
        <i className={iconClasses(liked, saving)} />
      </div>
      <p className="text-xs pb-1">
        {likes->Like.likesForAnswer(answerId)->List.length->string_of_int->str}
      </p>
    </div>
  </div>
}
