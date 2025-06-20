type t = {
  id: string,
  name: string,
  avatarUrl: option<string>,
  title: string,
}

let id = t => t.id
let name = t => t.name
let avatarUrl = t => t.avatarUrl
let title = t => t.title

let decode = json => {
  open Json.Decode
  {
    id: json->field("id", string),
    name: json->field("name", string),
    avatarUrl: json->optional(field("avatarUrl", string)),
    title: json->field("title", string),
  }
}

let findById = (id, proxies) =>
  proxies->ListUtils.unsafeFind(proxy => proxy.id == id, "Unable to find a User with ID " ++ id)

let make = (~id, ~name, ~avatarUrl, ~title) => {
  id: id,
  name: name,
  avatarUrl: avatarUrl,
  title: title,
}

let makeFromJs = jsObject =>
  make(
    ~id=jsObject["id"],
    ~name=jsObject["name"],
    ~avatarUrl=jsObject["avatarUrl"],
    ~title=jsObject["title"],
  )
