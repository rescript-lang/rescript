// Thin Node HTTP adapter around the TScert-verified ReScript modules
// (src/TodoModel.res.ts, src/TodoView.res.ts, src/TodoApp.res.ts).
// This file is not part of the verified core. Run with Node >= 23.6:
//
//   node tests/build_tests/emit_ts/todo-server.mts

import { createServer } from "node:http";
import { initState } from "./src/TodoModel.res.ts";
import { handleRequest } from "./src/TodoApp.res.ts";

let state = initState();

const server = createServer((req, res) => {
  let body = "";
  req.on("data", (chunk: Buffer) => {
    body += chunk.toString("utf8");
  });
  req.on("end", () => {
    const [next, response] = handleRequest(state, {
      method: req.method ?? "GET",
      path: req.url ?? "/",
      body,
    });
    state = next;
    res.writeHead(response.status, { "content-type": response.contentType });
    res.end(response.body);
  });
});

const port = Number(process.env.PORT ?? 8080);

server.listen(port, "127.0.0.1", () => {
  console.log(`ReScript + TScert todo app listening on http://127.0.0.1:${port}`);
});
