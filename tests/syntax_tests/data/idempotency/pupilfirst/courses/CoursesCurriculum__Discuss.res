let str = React.string

open CoursesCurriculum__Types

let linkToCommunity = (communityId, targetId) =>
  "/communities/" ++ (communityId ++ ("?target_id=" ++ targetId))

let linkToNewQuestion = (communityId, targetId) =>
  "/communities/" ++ (communityId ++ ("/new_question" ++ ("?target_id=" ++ targetId)))

let questionCard = question => {
  let questionId = question->Community.questionId
  let questionLink = "/questions/" ++ questionId
  <div
    href=questionLink
    key=questionId
    className="flex justify-between items-center px-5 py-4 bg-white border-t">
    <span className="text-sm font-semibold"> {question->Community.questionTitle->str} </span>
    <a href=questionLink className="btn btn-primary-ghost btn-small"> {"View"->str} </a>
  </div>
}

let handleEmpty = () =>
  <div className="flex flex-col justify-center items-center bg-white px-3 py-10">
    <i className="fa fa-comments text-5xl text-gray-600 mb-2 " />
    <div className="text-center">
      <h4 className="font-bold">
        {"There's been no recent discussion about this target."->str}
      </h4>
      <p> {"Use the community to clear your doubts, and to help your peers!"->str} </p>
    </div>
  </div>

let actionButtons = (community, targetId) => {
  let communityId = community->Community.id
  let communityName = community->Community.name

  <div className="flex">
    <a
      title={"Browse all questions about this target in the " ++ (communityName ++ " community")}
      href={linkToCommunity(communityId, targetId)}
      className="btn btn-default mr-3">
      {"Go to community"->str}
    </a>
    <a
      title={"Ask a question in the " ++ (communityName ++ " community")}
      href={linkToNewQuestion(communityId, targetId)}
      className="btn btn-primary">
      {"Ask a question"->str}
    </a>
  </div>
}

let communityTitle = community =>
  <h5 className="font-bold">
    {"Questions from " ++ ((community->Community.name) ++ " community")->str}
  </h5>

@react.component
let make = (~targetId, ~communities) =>
  <div className="">
    {communities
    ->List.map(community => {
      let communityId = community->Community.id
      <div key=communityId className="mt-12 bg-gray-100 px-6 py-4 rounded-lg">
        <div className="flex flex-col md:flex-row w-full justify-between pb-3 items-center">
          <div> {communityTitle(community)} </div> {actionButtons(community, targetId)}
        </div>
        <div className="justify-between rounded-lg overflow-hidden shadow">
          {switch community->Community.questions {
          | list{} => handleEmpty()
          | questions =>
            questions
            ->List.map(question => questionCard(question))
            ->Array.of_list
            ->React.array
          }}
        </div>
      </div>
    })
    ->Array.of_list
    ->React.array}
  </div>
