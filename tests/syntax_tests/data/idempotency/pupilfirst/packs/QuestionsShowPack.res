open QuestionsShow__Types

type props = {
  question: Question.t,
  answers: list<Answer.t>,
  comments: list<Comment.t>,
  users: list<User.t>,
  likes: list<Like.t>,
  currentUserId: string,
  communityPath: string,
  isCoach: bool,
  communityId: string,
  target: option<Target.t>,
}

let decodeProps = json => {
  open Json.Decode
  {
    question: json->field("questions", Question.decode),
    answers: json->field("answers", list(Answer.decode)),
    comments: json->field("comments", list(Comment.decode)),
    users: json->field("users", list(User.decode)),
    likes: json->field("likes", list(Like.decode)),
    currentUserId: json->field("currentUserId", string),
    communityPath: json->field("communityPath", string),
    isCoach: json->field("isCoach", bool),
    communityId: json->field("communityId", string),
    target: json->field("target", nullable(Target.decode))->Js.Null.toOption,
  }
}

let props = DomUtils.parseJsonAttribute()->decodeProps

ReactDOMRe.renderToElementWithId(
  <QuestionsShow
    question=props.question
    answers=props.answers
    comments=props.comments
    users=props.users
    likes=props.likes
    currentUserId=props.currentUserId
    communityPath=props.communityPath
    isCoach=props.isCoach
    communityId=props.communityId
    target=props.target
  />,
  "react-root",
)
