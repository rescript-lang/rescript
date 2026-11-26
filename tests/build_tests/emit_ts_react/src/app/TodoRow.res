@react.component
let make = (~todo: TodoModel.todo, ~onAction: TodoModel.action => unit) =>
  <li className={todo.done ? "done" : ""}>
    <TodoLabel title=todo.title />
    <button onClick={_ => onAction(TodoModel.Toggle(todo.id))}>
      {React.string(todo.done ? "Reopen" : "Complete")}
    </button>
    <button onClick={_ => onAction(TodoModel.CyclePriority(todo.id))}>
      {React.string("Priority: " ++ TodoModel.priorityToString(todo.priority))}
    </button>
    <button onClick={_ => onAction(TodoModel.Delete(todo.id))}>{React.string("Delete")}</button>
  </li>
