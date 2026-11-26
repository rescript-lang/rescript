let priorityClass = (priority: TodoModel.priority): string =>
  switch priority {
  | High => "pill priority-high"
  | Medium => "pill priority-medium"
  | Low => "pill priority-low"
  }

let priorityLabel = (priority: TodoModel.priority): string =>
  switch priority {
  | High => "High"
  | Medium => "Medium"
  | Low => "Low"
  }

@react.component
let make = (~todo: TodoModel.todo, ~onAction: TodoModel.action => unit) =>
  <li className={todo.done ? "todo-row done" : "todo-row"}>
    <button
      className={todo.done ? "check checked" : "check"}
      ariaLabel={todo.done ? "Mark active" : "Mark completed"}
      onClick={_ => onAction(TodoModel.Toggle(todo.id))}
    >
      {React.string(todo.done ? "✓" : "")}
    </button>
    <TodoLabel title=todo.title />
    <div className="row-actions">
      <button
        className={priorityClass(todo.priority)}
        ariaLabel="Cycle priority"
        onClick={_ => onAction(TodoModel.CyclePriority(todo.id))}
      >
        {React.string(priorityLabel(todo.priority))}
      </button>
      <button
        className="delete-btn"
        ariaLabel="Delete task"
        onClick={_ => onAction(TodoModel.Delete(todo.id))}
      >
        {React.string("×")}
      </button>
    </div>
  </li>
