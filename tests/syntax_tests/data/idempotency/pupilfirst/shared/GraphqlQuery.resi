let sendQuery: (
  ~notify: bool=?,
  {.."parse": Js.Json.t => 'a, "query": string, "variables": Js.Json.t},
) => Js.Promise.t<'a>
