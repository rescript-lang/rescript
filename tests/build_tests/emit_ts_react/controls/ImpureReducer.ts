/// <reference types="@cristianoc/tscert/effects" />

type State = { count: number };

export function mutate(
  state: State,
  action: number,
): Modifies<{ values: "keep"; spines: "keep" }, State> {
  state.count += action;
  return state;
}
