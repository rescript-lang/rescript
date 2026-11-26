import { createElement, useReducer } from "react";
import { init, reduce } from "../app/TodoModel.res.ts";
import { type inputProps, make as TodoApp } from "../app/TodoReact.res.ts";

function TodoInput({
  value,
  placeholder,
  ariaLabel,
  onValue,
  onEnter,
}: inputProps) {
  return createElement("input", {
    type: "text",
    value,
    placeholder,
    "aria-label": ariaLabel,
    onChange: event => onValue(event.currentTarget.value),
    onKeyDown: event => {
      if (event.key === "Enter") onEnter();
    },
  });
}

export function TodoHost() {
  const [state, dispatch] = useReducer(reduce, undefined, init);
  return createElement(TodoApp, { state, dispatch, input: TodoInput });
}
