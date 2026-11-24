type request = {
  method: string,
  path: string,
  body: string,
}

type response = {
  status: int,
  contentType: string,
  body: string,
}

type route =
  | Home(TodoModel.filter, string)
  | ListJson
  | GetTodo(int)
  | Create
  | Toggle(int)
  | Edit(int)
  | CyclePriority(int)
  | Delete(int)
  | ToggleAll
  | ClearCompleted
  | BadId
  | NotFound

let todosPath = "/todos"
let todosPrefix = "/todos/"

// Parse a non-negative decimal integer id.
let parseId = (text: string): option<int> => {
  let len = String.length(text)
  if len == 0 {
    None
  } else {
    let n = ref(0)
    let valid = ref(true)
    let i = ref(0)
    while i.contents < len && valid.contents {
      switch String.codePointAt(text, i.contents) {
      | Some(c) if c >= 48 && c <= 57 => n := n.contents * 10 + (c - 48)
      | _ => valid := false
      }
      i := i.contents + 1
    }
    valid.contents ? Some(n.contents) : None
  }
}

let hexDigit = (code: option<int>): option<int> =>
  switch code {
  | Some(c) if c >= 48 && c <= 57 => Some(c - 48)
  | Some(c) if c >= 65 && c <= 70 => Some(c - 55)
  | Some(c) if c >= 97 && c <= 102 => Some(c - 87)
  | _ => None
  }

let decodeQueryValue = (raw: string): string => {
  let len = String.length(raw)
  let out = ref("")
  let i = ref(0)
  while i.contents < len {
    let ch = String.charAt(raw, i.contents)
    if ch == "+" {
      out := out.contents ++ " "
      i := i.contents + 1
    } else if ch == "%" && i.contents + 2 < len {
      let h1 = hexDigit(String.codePointAt(raw, i.contents + 1))
      let h2 = hexDigit(String.codePointAt(raw, i.contents + 2))
      switch (h1, h2) {
      | (Some(a), Some(b)) =>
        out := out.contents ++ String.fromCharCode(a * 16 + b)
        i := i.contents + 3
      | _ =>
        out := out.contents ++ ch
        i := i.contents + 1
      }
    } else {
      out := out.contents ++ ch
      i := i.contents + 1
    }
  }
  out.contents
}

let parseQuery = (qs: string): (TodoModel.filter, string) => {
  let filter = ref(TodoModel.All)
  let query = ref("")
  if String.length(qs) > 0 {
    let parts = String.split(qs, "&")
    for i in 0 to Array.length(parts) - 1 {
      switch parts[i] {
      | Some(pair) =>
        let eq = String.indexOf(pair, "=")
        if eq >= 0 {
          let key = String.slice(pair, ~start=0, ~end=eq)
          let value = decodeQueryValue(String.slice(pair, ~start=eq + 1))
          if key == "filter" {
            filter := TodoModel.filterFromString(value)
          } else if key == "q" {
            query := value
          }
        }
      | None => ()
      }
    }
  }
  (filter.contents, query.contents)
}

let parseCreateBody = (body: string): (string, TodoModel.priority) => {
  let sep = String.indexOf(body, "|")
  if sep >= 0 {
    let prefix = String.slice(body, ~start=0, ~end=sep)
    let rest = String.slice(body, ~start=sep + 1)
    let p = String.toLowerCase(String.trim(prefix))
    if p == "low" || p == "medium" || p == "high" {
      (rest, TodoModel.priorityFromString(p))
    } else {
      (body, TodoModel.Medium)
    }
  } else {
    (body, TodoModel.Medium)
  }
}

let parseRoute = (method: string, fullPath: string): route => {
  let qIdx = String.indexOf(fullPath, "?")
  let path = qIdx >= 0 ? String.slice(fullPath, ~start=0, ~end=qIdx) : fullPath
  let qs = qIdx >= 0 ? String.slice(fullPath, ~start=qIdx + 1) : ""
  if method == "GET" && path == "/" {
    let (filter, query) = parseQuery(qs)
    Home(filter, query)
  } else if method == "GET" && path == todosPath {
    ListJson
  } else if method == "POST" && path == todosPath {
    Create
  } else if method == "POST" && path == "/todos/toggle-all" {
    ToggleAll
  } else if method == "POST" && path == "/todos/clear-completed" {
    ClearCompleted
  } else if String.startsWith(path, todosPrefix) {
    let rest = String.slice(path, ~start=String.length(todosPrefix))
    if method == "POST" && String.endsWith(rest, "/toggle") {
      let idText = String.slice(rest, ~start=0, ~end=String.length(rest) - 7)
      switch parseId(idText) {
      | Some(id) => Toggle(id)
      | None => BadId
      }
    } else if method == "POST" && String.endsWith(rest, "/edit") {
      let idText = String.slice(rest, ~start=0, ~end=String.length(rest) - 5)
      switch parseId(idText) {
      | Some(id) => Edit(id)
      | None => BadId
      }
    } else if method == "POST" && String.endsWith(rest, "/priority") {
      let idText = String.slice(rest, ~start=0, ~end=String.length(rest) - 9)
      switch parseId(idText) {
      | Some(id) => CyclePriority(id)
      | None => BadId
      }
    } else if method == "DELETE" {
      switch parseId(rest) {
      | Some(id) => Delete(id)
      | None => BadId
      }
    } else if method == "GET" {
      switch parseId(rest) {
      | Some(id) => GetTodo(id)
      | None => BadId
      }
    } else {
      NotFound
    }
  } else {
    NotFound
  }
}

let escapeJsonString = (text: string): string => {
  let out = ref("")
  for i in 0 to String.length(text) - 1 {
    let c = String.charAt(text, i)
    let escaped = switch c {
    | "\\" => "\\\\"
    | "\"" => "\\\""
    | "\n" => "\\n"
    | "\r" => "\\r"
    | "\t" => "\\t"
    | _ => c
    }
    out := out.contents ++ escaped
  }
  "\"" ++ out.contents ++ "\""
}

let todoJson = (t: TodoModel.todo): string =>
  "{\"id\":" ++
  Int.toString(t.id) ++
  ",\"title\":" ++
  escapeJsonString(t.title) ++
  ",\"done\":" ++
  (t.done ? "true" : "false") ++
  ",\"priority\":" ++
  escapeJsonString(TodoModel.priorityToString(t.priority)) ++ "}"

let todosJson = (todos: array<TodoModel.todo>): string =>
  "[" ++ Array.join(Array.map(todos, todoJson), ",") ++ "]"

let jsonResponse = (status: int, body: string): response => {
  status,
  contentType: "application/json; charset=utf-8",
  body,
}

let textResponse = (status: int, body: string): response => {
  status,
  contentType: "text/plain; charset=utf-8",
  body,
}

let handleRequest = (state: TodoModel.appState, req: request): (TodoModel.appState, response) =>
  switch parseRoute(req.method, req.path) {
  | Home(filter, query) => (
      state,
      {
        status: 200,
        contentType: "text/html; charset=utf-8",
        body: TodoView.renderPage(state, filter, query),
      },
    )
  | ListJson => (state, jsonResponse(200, todosJson(state.todos)))
  | GetTodo(id) =>
    switch TodoModel.findTodo(state.todos, id) {
    | Some(todo) => (state, jsonResponse(200, todoJson(todo)))
    | None => (state, textResponse(404, "not found"))
    }
  | Create =>
    let (rawTitle, priority) = parseCreateBody(req.body)
    let title = String.trim(rawTitle)
    if String.length(title) == 0 {
      (state, textResponse(400, "empty title"))
    } else {
      let next = TodoModel.addTodo(state, title, priority)
      (next, jsonResponse(201, todosJson(next.todos)))
    }
  | Toggle(id) =>
    let next = TodoModel.toggleTodo(state, id)
    (next, jsonResponse(200, todosJson(next.todos)))
  | Edit(id) =>
    let title = String.trim(req.body)
    if String.length(title) == 0 {
      (state, textResponse(400, "empty title"))
    } else {
      let next = TodoModel.editTodo(state, id, title)
      (next, jsonResponse(200, todosJson(next.todos)))
    }
  | CyclePriority(id) =>
    let next = TodoModel.cyclePriority(state, id)
    (next, jsonResponse(200, todosJson(next.todos)))
  | Delete(id) =>
    let next = TodoModel.deleteTodo(state, id)
    (next, jsonResponse(200, todosJson(next.todos)))
  | ToggleAll =>
    let next = TodoModel.toggleAll(state)
    (next, jsonResponse(200, todosJson(next.todos)))
  | ClearCompleted =>
    let next = TodoModel.clearCompleted(state)
    (next, jsonResponse(200, todosJson(next.todos)))
  | BadId => (state, textResponse(400, "bad id"))
  | NotFound => (state, textResponse(404, "not found"))
  }
