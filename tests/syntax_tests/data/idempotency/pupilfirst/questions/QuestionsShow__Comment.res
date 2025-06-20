type t = {
  id: string,
  value: string,
  commentableId: string,
  commentableType: string,
  creatorId: string,
  createdAt: string,
}

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    value: json->field("value", string),
    creatorId: json->field("creatorId", string),
    commentableId: json->field("commentableId", string),
    commentableType: json->field("commentableType", string),
    createdAt: json->field("createdAt", string),
  }
}

let creatorId = t => t.creatorId

let value = t => t.value

let id = t => t.id

let createdAt = t => t.createdAt
let commentsForQuestion = comments =>
  comments->List.filter(comment => comment.commentableType == "Question")

let commentsForAnswer = (answerId, comments) =>
  comments
  ->List.filter(comment => comment.commentableType == "Answer")
  ->List.filter(comment => comment.commentableId == answerId)

let addComment = (comments, comment) =>
  comments->List.rev->List.append(list{comment})->List.rev

let delete = (id, comments) => comments->List.filter(c => c.id != id)

let create = (id, value, creatorId, commentableId, commentableType, createdAt) => {
  id: id,
  value: value,
  creatorId: creatorId,
  commentableId: commentableId,
  commentableType: commentableType,
  createdAt: createdAt,
}

let sort = comments =>
  comments->List.sort((c1, c2) => (c1.id->int_of_string) - (c2.id->int_of_string))
