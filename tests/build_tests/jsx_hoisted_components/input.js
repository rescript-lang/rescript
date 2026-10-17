// @ts-check

import assert from "node:assert/strict";
import { readFile, writeFile } from "node:fs/promises";
import { setup } from "#dev/process";

const { execBuildOrThrow, execClean } = setup(import.meta.dirname);
const configUrl = new URL("rescript.json", import.meta.url);
const originalConfig = await readFile(configUrl, "utf8");
/** @param {string} name */
const source = name => new URL(`src/${name}.mjs`, import.meta.url);

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
      assert.doesNotMatch(producer, /Sidebar\$Provider\$make/);
    }
  }

  const sidebar = await import(source("Sidebar").href);
  const consumer = await import(source("Consumer").href);
  assert.equal(sidebar.Provider.make, sidebar.Provider$make);
  assert.equal(sidebar.Group.Provider.make, sidebar.Group$Provider$make);
  assert.equal(sidebar.WithProps.make, sidebar.WithProps$make);
  assert.equal(sidebar.Memo.make, sidebar.Memo$make);
  assert.equal(sidebar.ForwardRef.make, sidebar.ForwardRef$make);
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
