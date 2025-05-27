open QuestionsShow__Types

let str = React.string

@react.component
let make = (
  ~comments,
  ~users,
  ~commentableType,
  ~commentableId,
  ~addCommentCB,
  ~currentUserId,
  ~archiveCB,
  ~isCoach,
) => {
  let (showAll, setShowAll) = React.useState(() => comments->List.length <= 3)

  let (commentsToShow, allCommentsShown) = switch (showAll, comments->Comment.sort) {
  | (false, list{e1, e2, e3, _e4, ..._rest}) => (list{e1, e2, e3}, false)
  | (_, comments) => (comments, true)
  }

  <ul
    className="list-reset max-w-3xl w-full flex flex-col mx-auto items-center justify-center px-3 lg:px-6">
    {commentsToShow
    ->List.sort((commentA, commentB) =>
      DateFns.differenceInSeconds(
        commentA->Comment.createdAt->DateFns.parseString,
        commentB->Comment.createdAt->DateFns.parseString,
      )->int_of_float
    )
    ->List.map(comment => {
      let commentText =
        (comment->Comment.value) ++
          (" - **" ++
          ((users->User.findById(comment->Comment.creatorId)->User.name) ++
            ("** on " ++
            (comment
            ->Comment.createdAt
            ->DateFns.parseString
            ->DateFns.format("Do MMMM, YYYY")))))

      <li
        key={comment->Comment.id} className="w-full text-left border border-gray-400 border-t-0">
        <div className="flex w-full leading-normal text-xs bg-white justify-between">
          <span className="flex items-center px-2 py-1 md:px-4 md:py-2">
            <MarkdownBlock markdown=commentText profile=Markdown.Comment />
          </span>
          {isCoach || comment->Comment.creatorId == currentUserId
            ? <QuestionsShow__ArchiveManager
                id={comment->Comment.id} resourceType="Comment" archiveCB
              />
            : React.null}
        </div>
      </li>
    })
    ->Array.of_list
    ->ReasonReact.array}
    {allCommentsShown
      ? <QuestionsShow__AddComment commentableType commentableId addCommentCB currentUserId />
      : React.null}
    {!allCommentsShown
      ? <a
          onClick={_ => setShowAll(_ => true)}
          className="bg-gray-200 rounded-full cursor-pointer border py-1 px-3 flex mx-auto appearance-none text-xs font-semibold hover:bg-primary-100 hover:text-primary-500 -mt-3">
          {"Show More"->str}
        </a>
      : ReasonReact.null}
  </ul>
}
