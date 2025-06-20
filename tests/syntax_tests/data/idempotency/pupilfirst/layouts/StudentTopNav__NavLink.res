type t = {
  title: string,
  url: string,
}

let title = t => t.title

let url = t => t.url

let decode = json => {
  open Json.Decode
  {
    title: json->field("title", string),
    url: json->field("url", string),
  }
}
