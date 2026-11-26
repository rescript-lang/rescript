import assert from "node:assert/strict";
import { readdir, readFile } from "node:fs/promises";
import path from "node:path";
import { createElement } from "react";
import { renderToStaticMarkup } from "react-dom/server";
import ts from "typescript";
import { build } from "./build.mjs";

const src = new URL("./src/app/", import.meta.url);
const forwarding = "./src/forwarding/Forwarding.res.ts";
const forwardingMutation = "./src/forwarding/ForwardingMutation.res.ts";
const effectClaims = new Set([
  "./src/app/TodoModel.res.ts",
  forwarding,
  forwardingMutation,
]);
const snapshots = [
  ...(await readdir(src))
    .filter(file => file.endsWith(".res"))
    .map(file => `./src/app/${file}.ts`),
  "./src/forwarding/Forwarded.res.ts",
  forwarding,
  forwardingMutation,
];
const peers = new Set(snapshots.map(file => path.posix.normalize(file)));
const expected = await Promise.all(
  snapshots.map(file => readFile(new URL(file, import.meta.url))),
);

await build();

for (const [index, file] of snapshots.entries()) {
  const emitted = await readFile(new URL(file, import.meta.url));
  const { importedFiles, typeReferenceDirectives } = ts.preProcessFile(
    emitted.toString(),
    true,
  );
  assert.deepEqual(
    typeReferenceDirectives.map(({ fileName }) => fileName),
    effectClaims.has(file) ? ["@cristianoc/tscert/effects"] : [],
    `Unexpected type references in ${file}`,
  );
  for (const { fileName } of importedFiles) {
    const directory = path.posix.dirname(path.posix.normalize(file));
    const imported = path.posix.join(directory, fileName);
    assert.ok(
      fileName === "react/jsx-runtime" ||
        (fileName.startsWith("./") &&
          path.posix.dirname(imported) === directory &&
          peers.has(imported)),
      `Snapshot ${file} imports outside its checked sources: ${fileName}`,
    );
  }
  assert.ok(
    emitted.equals(expected[index]),
    `Generated snapshot is stale: ${file}`,
  );
}

assert.match(
  await readFile(new URL(forwarding, import.meta.url), "utf8"),
  /export function increment\(n: number\): Modifies<\{\}, number> \{/,
  "The forwarding function must retain its no-mutation claim",
);

const { TodoHost } = await import("./src/trusted/TodoHost.ts");
const reducerSource = await readFile(new URL("TodoModel.res.ts", src), "utf8");
assert.match(
  reducerSource,
  /export function reduce\([^\n]+\): Modifies<\{\}, state> \{/,
  "The emitted reducer must state its no-mutation claim",
);
const { init, reduce, visibleTodos } = await import(
  "./src/app/TodoModel.res.ts"
);
const html = renderToStaticMarkup(createElement(TodoHost));
assert.match(html, /<h1>Tasks<\/h1>/);
assert.match(html, /Search tasks/);
assert.equal([...html.matchAll(/<li\b/g)].length, 3);

let state = init();
state = reduce(state, { TAG: "Draft", _0: "Test Todo" });
state = reduce(state, "Add");
const added = state.model.todos.at(-1);
assert.equal(added?.title, "Test Todo");
assert.equal(state.draft, "");
state = reduce(state, { TAG: "Toggle", _0: added.id });
state = reduce(state, { TAG: "CyclePriority", _0: added.id });
state = reduce(state, { TAG: "Filter", _0: "Completed" });
state = reduce(state, { TAG: "Query", _0: "Test" });
const filtered = visibleTodos(state.model.todos, state.filter, state.query);
assert.equal(filtered.length, 1);
assert.equal(filtered[0].id, added.id);
assert.notEqual(filtered[0].priority, added.priority);
state = reduce(state, { TAG: "Delete", _0: added.id });
assert.equal(
  visibleTodos(state.model.todos, state.filter, state.query).length,
  0,
);
