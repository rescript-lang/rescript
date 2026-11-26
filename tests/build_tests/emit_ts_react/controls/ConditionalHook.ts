import * as React from "react";

function reduce(state: number, action: number): number {
  return state + action;
}

export function ConditionalHook(enabled: boolean): number {
  const first = React.useReducer(reduce, 0);
  if (enabled) {
    const second = React.useReducer(reduce, 1);
    return second[0];
  }
  return first[0];
}
