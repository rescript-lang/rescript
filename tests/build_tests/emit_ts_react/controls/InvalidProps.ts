import { type ElementHandle, jsx } from "react/jsx-runtime";

export function InvalidProps(): ElementHandle {
  return jsx("button", { onClick: 42, children: "Toggle" });
}
