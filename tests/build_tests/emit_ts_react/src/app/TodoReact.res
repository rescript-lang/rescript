type inputProps = {value: string, onValue: string => unit}

@inline(never)
let pressed = (active: bool): [#"true" | #"false" | #mixed] => active ? #"true" : #"false"

@react.component
let make = (
  ~state: TodoModel.state,
  ~dispatch: TodoModel.action => unit,
  ~input: React.component<inputProps>,
) => {
  let rows =
    TodoModel.visibleTodos(state.model.todos, state.filter, state.query)->Array.map(todo =>
      <TodoRow key={Int.toString(todo.id)} todo onAction=dispatch />
    )
  <main>
    <h1>{React.string("Tasks")}</h1>
    <label>
      {React.string("New task")}
      {React.jsx(input, {value: state.draft, onValue: value => dispatch(TodoModel.Draft(value))})}
    </label>
    <div className="toolbar">
      <button onClick={_ => dispatch(TodoModel.Add)}>{React.string("Add task")}</button>
      <button onClick={_ => dispatch(TodoModel.ToggleAll)}>{React.string("Toggle all")}</button>
      <button onClick={_ => dispatch(TodoModel.ClearCompleted)}>
        {React.string("Clear completed")}
      </button>
    </div>
    <label>
      {React.string("Search tasks")}
      {React.jsx(input, {value: state.query, onValue: value => dispatch(TodoModel.Query(value))})}
    </label>
    <div className="toolbar">
      <button
        ariaPressed={pressed(state.filter == All)} onClick={_ => dispatch(TodoModel.Filter(All))}
      >
        {React.string("All")}
      </button>
      <button
        ariaPressed={pressed(state.filter == Active)}
        onClick={_ => dispatch(TodoModel.Filter(Active))}
      >
        {React.string("Active")}
      </button>
      <button
        ariaPressed={pressed(state.filter == Completed)}
        onClick={_ => dispatch(TodoModel.Filter(Completed))}
      >
        {React.string("Completed")}
      </button>
    </div>
    <ul>{React.array(rows)}</ul>
  </main>
}
