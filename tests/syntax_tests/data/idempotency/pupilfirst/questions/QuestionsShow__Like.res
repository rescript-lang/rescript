type t = {
  id: string,
  userId: string,
  answerId: string,
}

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    userId: json->field("userId", string),
    answerId: json->field("answerId", string),
  }
}

let id = t => t.id

let likesForAnswer = (answerId, likes) => likes->List.filter(like => like.answerId == answerId)

let likeByCurrentUser = (answerId, currentUserId, likes) =>
  likesForAnswer(answerId, likes)->List.filter(like => like.userId == currentUserId)

let currentUserLiked = (answerId, currentUserId, likes) =>
  likeByCurrentUser(answerId, currentUserId, likes)->ListUtils.isNotEmpty

let addLike = (like, likes) => likes->List.append(list{like})

let removeLike = (id, likes) => likes->List.filter(like => like.id != id)

let create = (id, userId, answerId) => {id: id, userId: userId, answerId: answerId}
