import { createElement, useReducer } from "react";
import { init, reduce } from "../app/TodoModel.res.ts";
import { type inputProps, make as TodoApp } from "../app/TodoReact.res.ts";

function TodoInput({ value, onValue }: inputProps) {
  return createElement("input", {
    value,
    onChange: event => onValue(event.currentTarget.value),
  });
}

export function TodoHost() {
  const [state, dispatch] = useReducer(reduce, undefined, init);
  return createElement(TodoApp, { state, dispatch, input: TodoInput });
}
