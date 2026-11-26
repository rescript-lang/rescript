import { createElement } from "react";
import { createRoot } from "react-dom/client";
import { TodoHost } from "./TodoHost.ts";

const container = document.getElementById("app");
if (container === null) throw new Error("Missing app container");
createRoot(container).render(createElement(TodoHost));
