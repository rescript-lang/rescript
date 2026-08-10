let sendQuery: (
  ~notify: bool=?,
  {.."parse": JSON.t => 'a, "query": string, "variables": JSON.t},
) => Promise.t<'a>
