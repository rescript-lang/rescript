type priority = Low | Medium | High

type filter = All | Active | Completed

type todo = {
  id: int,
  title: string,
  done: bool,
  priority: priority,
}

type appState = {
  nextId: int,
  todos: array<todo>,
}

type counts = {
  total: int,
  active: int,
  completed: int,
}

let initState = (): appState => {
  nextId: 4,
  todos: [
    {
      id: 1,
      title: "Compile ReScript modules with -bs-emit-ts",
      done: true,
      priority: High,
    },
    {
      id: 2,
      title: "Verify every generated function with TScert",
      done: true,
      priority: High,
    },
    {
      id: 3,
      title: "Add, filter, prioritize, or edit a task below",
      done: false,
      priority: Medium,
    },
  ],
}

let nextPriority = (p: priority): priority =>
  switch p {
  | Low => Medium
  | Medium => High
  | High => Low
  }

let priorityFromString = (s: string): priority =>
  switch String.toLowerCase(String.trim(s)) {
  | "high" => High
  | "low" => Low
  | _ => Medium
  }

let priorityToString = (p: priority): string =>
  switch p {
  | Low => "low"
  | Medium => "medium"
  | High => "high"
  }

let filterFromString = (s: string): filter =>
  switch String.toLowerCase(String.trim(s)) {
  | "active" => Active
  | "completed" => Completed
  | _ => All
  }

let filterToString = (f: filter): string =>
  switch f {
  | All => "all"
  | Active => "active"
  | Completed => "completed"
  }

let addTodo = (state: appState, title: string, priority: priority): appState => {
  let clean = String.trim(title)
  if String.length(clean) == 0 {
    state
  } else {
    let todo = {
      id: state.nextId,
      title: clean,
      done: false,
      priority,
    }
    let todos = Array.copy(state.todos)
    Array.push(todos, todo)
    {nextId: state.nextId + 1, todos}
  }
}

let toggleTodo = (state: appState, id: int): appState => {
  let todos = Array.map(state.todos, t => t.id == id ? {...t, done: !t.done} : t)
  {nextId: state.nextId, todos}
}

let editTodo = (state: appState, id: int, title: string): appState => {
  let clean = String.trim(title)
  if String.length(clean) == 0 {
    state
  } else {
    let todos = Array.map(state.todos, t => t.id == id ? {...t, title: clean} : t)
    {nextId: state.nextId, todos}
  }
}

let cyclePriority = (state: appState, id: int): appState => {
  let todos = Array.map(state.todos, t =>
    t.id == id ? {...t, priority: nextPriority(t.priority)} : t
  )
  {nextId: state.nextId, todos}
}

let deleteTodo = (state: appState, id: int): appState => {
  let todos = Array.filter(state.todos, t => t.id != id)
  {nextId: state.nextId, todos}
}

let clearCompleted = (state: appState): appState => {
  let todos = Array.filter(state.todos, t => !t.done)
  {nextId: state.nextId, todos}
}

let toggleAll = (state: appState): appState => {
  let hasActive = Array.some(state.todos, t => !t.done)
  let todos = Array.map(state.todos, t => {...t, done: hasActive})
  {nextId: state.nextId, todos}
}

let visibleTodos = (todos: array<todo>, filter: filter, query: string): array<todo> => {
  let q = String.toLowerCase(String.trim(query))
  Array.filter(todos, t => {
    let matchesFilter = switch filter {
    | All => true
    | Active => !t.done
    | Completed => t.done
    }
    let matchesQuery = String.length(q) == 0 || String.includes(String.toLowerCase(t.title), q)
    matchesFilter && matchesQuery
  })
}

let countTodos = (todos: array<todo>): counts => {
  let active = ref(0)
  let completed = ref(0)
  for i in 0 to Array.length(todos) - 1 {
    switch todos[i] {
    | Some(t) =>
      if t.done {
        completed := completed.contents + 1
      } else {
        active := active.contents + 1
      }
    | None => ()
    }
  }
  {
    total: active.contents + completed.contents,
    active: active.contents,
    completed: completed.contents,
  }
}

let findTodo = (todos: array<todo>, id: int): option<todo> => Array.find(todos, t => t.id == id)
