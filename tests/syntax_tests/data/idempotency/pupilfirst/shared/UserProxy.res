type t = {
  id: string,
  userId: string,
  name: string,
  avatarUrl: option<string>,
  title: string,
}

let id = t => t.id
let userId = t => t.userId
let name = t => t.name
let avatarUrl = t => t.avatarUrl
let title = t => t.title

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    userId: json->field("userId", string),
    name: json->field("name", string),
    avatarUrl: json->optional(field("avatarUrl", string)),
    title: json->field("title", string),
  }
}

let findById = (id, proxies) =>
  proxies->ListUtils.unsafeFind(
    proxy => proxy.id == id,
    "Unable to find a UserProxy with ID " ++ id,
  )

let make = (~id, ~userId, ~name, ~avatarUrl, ~title) => {
  id: id,
  userId: userId,
  name: name,
  avatarUrl: avatarUrl,
  title: title,
}

let makeFromJs = jsObject =>
  make(
    ~id=jsObject["id"],
    ~userId=jsObject["userId"],
    ~name=jsObject["name"],
    ~avatarUrl=jsObject["avatarUrl"],
    ~title=jsObject["title"],
  )
