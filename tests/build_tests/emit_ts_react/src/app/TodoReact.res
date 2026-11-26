type inputProps = {
  value: string,
  placeholder: string,
  ariaLabel: string,
  onValue: string => unit,
  onEnter: unit => unit,
}

@inline(never)
let pressed = (active: bool): [#"true" | #"false" | #mixed] => active ? #"true" : #"false"

let remainingText = (counts: TodoModel.counts): string =>
  if counts.active == 0 {
    "All tasks completed"
  } else if counts.active == 1 {
    "1 task left"
  } else {
    Int.toString(counts.active) ++ " tasks left"
  }

let tabLabel = (label: string, count: int): string => label ++ " (" ++ Int.toString(count) ++ ")"

@react.component
let make = (
  ~state: TodoModel.state,
  ~dispatch: TodoModel.action => unit,
  ~input: React.component<inputProps>,
) => {
  let counts = TodoModel.countTodos(state.model.todos)
  let visible = TodoModel.visibleTodos(state.model.todos, state.filter, state.query)
  let rows = Array.map(visible, todo =>
    <TodoRow key={Int.toString(todo.id)} todo onAction=dispatch />
  )
  let listContent = if Array.length(rows) == 0 {
    <p className="empty-state">
      {React.string(
        String.length(String.trim(state.query)) > 0
          ? "No tasks match your search."
          : "No tasks in this view.",
      )}
    </p>
  } else {
    <ul className="todo-list">{React.array(rows)}</ul>
  }
  <main className="app-card">
    <header className="app-header">
      <div className="title-group">
        <h1>{React.string("Tasks")}</h1>
        <p className="subtitle">
          {React.string(
            Int.toString(counts.completed) ++ " of " ++ Int.toString(counts.total) ++ " done",
          )}
        </p>
      </div>
      <span className="verified-badge">{React.string("Verified reducer")}</span>
    </header>
    <div className="composer">
      {React.jsx(
        input,
        {
          value: state.draft,
          placeholder: "Add a new task...",
          ariaLabel: "New task",
          onValue: value => dispatch(TodoModel.Draft(value)),
          onEnter: () => dispatch(TodoModel.Add),
        },
      )}
      <button className="primary-btn" onClick={_ => dispatch(TodoModel.Add)}>
        {React.string("Add")}
      </button>
    </div>
    <div className="controls-bar">
      <div className="filter-group">
        <button
          className="filter-tab"
          ariaPressed={pressed(state.filter == All)}
          onClick={_ => dispatch(TodoModel.Filter(All))}
        >
          {React.string(tabLabel("All", counts.total))}
        </button>
        <button
          className="filter-tab"
          ariaPressed={pressed(state.filter == Active)}
          onClick={_ => dispatch(TodoModel.Filter(Active))}
        >
          {React.string(tabLabel("Active", counts.active))}
        </button>
        <button
          className="filter-tab"
          ariaPressed={pressed(state.filter == Completed)}
          onClick={_ => dispatch(TodoModel.Filter(Completed))}
        >
          {React.string(tabLabel("Completed", counts.completed))}
        </button>
      </div>
      <div className="search-wrap">
        {React.jsx(
          input,
          {
            value: state.query,
            placeholder: "Search tasks...",
            ariaLabel: "Search tasks",
            onValue: value => dispatch(TodoModel.Query(value)),
            onEnter: () => (),
          },
        )}
      </div>
    </div>
    {listContent}
    <footer className="app-footer">
      <span className="remaining">{React.string(remainingText(counts))}</span>
      <div className="footer-actions">
        <button className="ghost-btn" onClick={_ => dispatch(TodoModel.ToggleAll)}>
          {React.string(counts.active > 0 ? "Complete all" : "Reopen all")}
        </button>
        <button className="ghost-btn" onClick={_ => dispatch(TodoModel.ClearCompleted)}>
          {React.string("Clear completed")}
        </button>
      </div>
    </footer>
  </main>
}
