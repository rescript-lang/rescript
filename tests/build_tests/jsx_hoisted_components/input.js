// @ts-check

import assert from "node:assert/strict";
import { readFile, writeFile } from "node:fs/promises";
import { inspect } from "node:util";
import ts from "typescript";
import { setup } from "#dev/process";
import * as jsxRuntime from "./src/jsx-runtime.js";

const { execBuildOrThrow, execClean } = setup(import.meta.dirname);
const configUrl = new URL("rescript.json", import.meta.url);
const originalConfig = await readFile(configUrl, "utf8");
/** @param {string} name */
const source = name => new URL(`src/${name}.mjs`, import.meta.url);

/**
 * Model the RSC boundary: each actual client export becomes an opaque reference.
 * The server can pass it to JSX, but cannot inspect it or call its implementation.
 * @param {string} client
 */
function clientReferences(client) {
  const ast = ts.createSourceFile(
    "Sidebar.mjs",
    client,
    ts.ScriptTarget.ES2022,
  );
  const first = ast.statements[0];
  assert(ts.isExpressionStatement(first));
  assert(ts.isStringLiteral(first.expression));
  assert.equal(first.expression.text, "use client");

  return Object.fromEntries(
    ast.statements.flatMap(statement =>
      ts.isExportDeclaration(statement) &&
      statement.exportClause &&
      ts.isNamedExports(statement.exportClause)
        ? statement.exportClause.elements.map(({ name }) => [
            name.text,
            new Proxy(
              () => {
                throw new Error(`Cannot call client export ${name.text}`);
              },
              {
                get(_target, property) {
                  throw new Error(
                    `Cannot inspect client export ${name.text}.${String(property)}`,
                  );
                },
              },
            ),
          ])
        : [],
    ),
  );
}

/**
 * @param {string} client
 * @param {string} externalClient
 * @param {string} server
 */
function checkClientBoundary(client, externalClient, server) {
  const references = clientReferences(client);
  const externalReferences = clientReferences(externalClient);

  // Only convert the module syntax so the real compiled server code can run
  // against the boundary references, following jsx_preserve_semantics's harness.
  const code = ts.transpileModule(server, {
    compilerOptions: {
      module: ts.ModuleKind.CommonJS,
      target: ts.ScriptTarget.ES2022,
      esModuleInterop: false,
    },
  }).outputText;
  /** @param {string} id */
  const require = id => {
    if (id === "./Sidebar.mjs") return references;
    if (id === "./External.mjs") return externalReferences;
    if (id === "./React.mjs" || id === "./jsx-runtime.js") return jsxRuntime;
    assert.fail(`Unexpected server import: ${id}`);
  };
  const page = new Function("require", "exports", `${code}\nreturn exports;`)(
    require,
    {},
  );
  let element = page.make({});
  for (const name of [
    "Provider$make",
    "Group$Provider$make",
    "Provider$make",
    "WithProps$make",
    "Memo$make",
    "ForwardRef$make",
    "WithPrivate$make",
    "Sidebar$make",
    "External$make",
    "ExternalManual$make",
  ]) {
    assert(name in references, `Missing client export: ${name}`);
    assert.equal(element.type, references[name]);
    element = element.props.children;
  }
  assert("Debug$make" in externalReferences);
  assert.equal(element.type, externalReferences.Debug$make);
  assert.equal(element.props.children, "child");
  assert.equal(jsxRuntime.effects, 0);
}

try {
  for (const namespace of [false, true]) {
    for (const optimize of [false, true]) {
      const config = JSON.parse(originalConfig);
      config.namespace = namespace;
      config["compiler-flags"].push(
        optimize ? "-bs-cross-module-opt" : "-bs-no-cross-module-opt",
      );
      await writeFile(configUrl, JSON.stringify(config, null, 2) + "\n");
      await execClean();
      const result = await execBuildOrThrow();
      assert.doesNotMatch(result.stderr, /hoistedFunction|hoistedComponent/);

      const producer = await readFile(source("Sidebar"), "utf8");
      const consumer = await readFile(source("Consumer"), "utf8");
      for (const name of [
        "Provider$make",
        "Group$Provider$make",
        "WithProps$make",
        "Memo$make",
        "ForwardRef$make",
        "WithPrivate$make",
        "Sidebar$make",
        "External$make",
        "ExternalManual$make",
      ]) {
        assert(producer.includes(`  ${name},`), `Missing flat export: ${name}`);
      }
      assert.match(
        consumer,
        /\.jsx\(Sidebar(?:\$JsxHoistedComponents)?\.Provider\$make,/,
      );
      assert.match(
        consumer,
        /\.jsx\(Sidebar(?:\$JsxHoistedComponents)?\.Group\$Provider\$make,/,
      );
      assert.match(
        consumer,
        /\.jsx\(Sidebar(?:\$JsxHoistedComponents)?\.WithProps\$make,/,
      );
      assert.doesNotMatch(consumer, /\.Provider\.make/);
      assert.doesNotMatch(producer, /Hidden\$make/);
      assert.doesNotMatch(producer, /HiddenExternal|unused-private-component/);
      assert.doesNotMatch(producer, /Sidebar\$Provider\$make/);
      assert.doesNotMatch(producer, /Sidebar\$Sidebar\$make/);
      assert.doesNotMatch(producer, /External(?:Debug|Annotated)\$make/);
      assert.match(consumer, /from "node:util"/);
      assert.doesNotMatch(consumer, /\.External(?:Debug|Annotated)/);
      const dynamic = await readFile(source("Dynamic"), "utf8");
      assert.match(dynamic, /await import\("\.\/Sidebar\.mjs"\)/);
      assert.match(
        dynamic,
        /\.jsx\(Sidebar(?:\$JsxHoistedComponents)?\.Provider\$make,/,
      );
      checkClientBoundary(
        producer,
        await readFile(source("External"), "utf8"),
        await readFile(source("Page"), "utf8"),
      );
    }
  }

  const sidebar = await import(source("Sidebar").href);
  const consumer = await import(source("Consumer").href);
  const dynamic = await import(source("Dynamic").href);
  const external = await import(source("External").href);
  assert.equal(jsxRuntime.effects, 1);
  assert.equal(sidebar.Provider.make, sidebar.Provider$make);
  assert.equal(sidebar.Group.Provider.make, sidebar.Group$Provider$make);
  assert.equal(sidebar.WithProps.make, sidebar.WithProps$make);
  assert.equal(sidebar.Memo.make, sidebar.Memo$make);
  assert.equal(sidebar.ForwardRef.make, sidebar.ForwardRef$make);
  assert.equal(sidebar.WithPrivate.make, sidebar.WithPrivate$make);
  assert.equal(sidebar.Sidebar.make, sidebar.Sidebar$make);
  assert.equal(sidebar.External.make, sidebar.External$make);
  assert.equal(sidebar.ExternalManual.make, sidebar.ExternalManual$make);
  assert.equal(external.Debug.make, external.Debug$make);
  assert.equal(external.Debug$make({ children: "debug" }), "debug");
  assert.equal(consumer.debugElement.type, inspect);
  assert.equal(consumer.annotatedElement.type, inspect);
  assert.equal(sidebar.External$make({ children: "external" }), "external");
  const privateElement = sidebar.WithPrivate$make({ children: "hidden" });
  assert.equal(privateElement.type(privateElement.props), "hidden");
  assert.equal(dynamic.provider, sidebar.Provider.make);
  assert.equal(dynamic.element.type, sidebar.Provider.make);
  assert.equal(consumer.provider, sidebar.Provider.make);
  assert.equal(consumer.element.type, sidebar.Provider.make);
  assert.equal(consumer.deepElement.type, sidebar.Group.Provider.make);
  assert.equal(consumer.aliasElement.type, sidebar.Provider.make);
  assert.equal(consumer.propsElement.type, sidebar.WithProps.make);
  assert.equal(consumer.memoElement.type, sidebar.Memo.make);
  assert.equal(consumer.refElement.type, sidebar.ForwardRef.make);
  assert.equal(consumer.element.type(consumer.element.props), "child");
  assert.equal(consumer.deepElement.type(consumer.deepElement.props), "deep");
  assert.equal(
    consumer.aliasElement.type(consumer.aliasElement.props),
    "alias",
  );
  assert.equal(
    consumer.propsElement.type(consumer.propsElement.props),
    "props",
  );
  assert.equal(
    sidebar.privateElement.type(sidebar.privateElement.props),
    "private",
  );
  assert.equal(consumer.helper, "helper");
  assert.equal(
    consumer.memoElement.type.type(consumer.memoElement.props),
    "memo",
  );
  assert.equal(
    consumer.refElement.type.render(consumer.refElement.props, null),
    "ref",
  );
} finally {
  await writeFile(configUrl, originalConfig);
  await execClean();
}
