// @ts-check

import * as assert from "node:assert/strict";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const root = import.meta.dirname;
const { bsc } = setup(root);
const outputDir = path.join(root, "lib");

/**
 * @param {string} directory
 * @param {string} source
 * @param {string[]} [includes]
 * @param {string[]} [flags]
 */
async function compile(directory, source, includes = [], flags = []) {
  const output = path.join(outputDir, directory);
  const stem = path.basename(source, path.extname(source));
  await fs.mkdir(output, { recursive: true });
  return await bsc([
    "-color",
    "never",
    "-bs-cross-module-opt",
    "-check-lam",
    "-bs-project-root",
    root,
    "-bs-package-name",
    "unboxed_object_cross_module",
    "-bs-package-output",
    `esmodule:lib/${directory}:.mjs`,
    ...[directory, ...includes].flatMap(include => [
      "-I",
      path.join(outputDir, include),
    ]),
    ...flags,
    "-o",
    path.join(output, `${stem}.${source.endsWith(".resi") ? "cmi" : "cmj"}`),
    path.join(root, "fixtures", source),
  ]);
}

/** @param {Awaited<ReturnType<typeof compile>>} result */
function succeeds(result) {
  assert.equal(result.status, 0, result.stdout + result.stderr);
}

await fs.rm(outputDir, { recursive: true, force: true });

try {
  succeeds(await compile("legacy", "Legacy.res"));
  const legacy = await import(path.join(outputDir, "legacy", "Legacy.mjs"));
  assert.equal(legacy.answer, 42);

  // Multiple object cases still need a usable shared runtime field.
  const invalid = await compile("invalid", "Invalid.res");
  assert.notEqual(invalid.status, 0);
  assert.match(invalid.stderr, /discriminant/i);

  for (const [producer, consumerName, prefix] of [
    ["ObjectProducer", "ObjectConsumer", "object"],
    ["ObjectDomainProducer", "ObjectDomainConsumer", "object-domain"],
  ]) {
    succeeds(await compile(`${prefix}-inferred`, `${producer}.res`));
    succeeds(await compile(`${prefix}-explicit`, `${producer}.resi`));
    succeeds(
      await compile(
        `${prefix}-explicit`,
        `${producer}.res`,
        [],
        ["-bs-read-cmi"],
      ),
    );
    for (const directory of [`${prefix}-inferred`, `${prefix}-explicit`]) {
      succeeds(
        await compile(`${prefix}-app`, `${consumerName}.res`, [directory]),
      );
      const consumer = await import(
        path.join(outputDir, `${prefix}-app`, `${consumerName}.mjs`) +
          `?${directory}`
      );
      // Literal precedence agrees across inferred/explicit CMI boundaries and
      // cross-module optimization: Text(kind: "special") matches Literal.
      assert.equal(consumer.answer, 42);
      assert.equal(consumer.dynamicAnswer, 42);
    }
  }

  // Match snapshots are local to each cross-module inlined call. Nested
  // payload patterns share a read, while body reads and later calls stay fresh.
  succeeds(await compile("getter-inferred", "GetterProducer.res"));
  succeeds(await compile("getter-explicit", "GetterProducer.resi"));
  succeeds(
    await compile(
      "getter-explicit",
      "GetterProducer.res",
      [],
      ["-bs-read-cmi"],
    ),
  );
  for (const directory of ["getter-inferred", "getter-explicit"]) {
    const consumerDirectory = `${directory}-app`;
    succeeds(
      await compile(consumerDirectory, "GetterConsumer.res", [directory]),
    );
    const consumerPath = path.join(
      outputDir,
      consumerDirectory,
      "GetterConsumer.mjs",
    );
    const consumer = await import(consumerPath);
    assert.equal(consumer.first, "B");
    assert.equal(consumer.firstReads, 1);
    assert.equal(consumer.second, "A");
    assert.equal(consumer.totalReads, 2);
    assert.equal(consumer.destructureAnswer, "A");
    assert.equal(consumer.destructureReads.contents, 2);
    assert.equal(consumer.destructureEffects.contents, 1);
    assert.equal(consumer.nestedAnswer, "A");
    assert.equal(consumer.nestedReads.contents, 2);
    assert.equal(consumer.nestedEffects.contents, 1);
    const emitted = await fs.readFile(consumerPath, "utf8");
    assert.doesNotMatch(
      emitted,
      /GetterProducer\.(choose|bodyDestructure|bodyMatch)\(/,
    );
  }

  const producerDirectory = "object-domain-explicit";
  succeeds(await compile("facade", "Facade.res", [producerDirectory]));
  succeeds(
    await compile("app", "Transitive.res", ["facade", producerDirectory]),
  );
  const transitive = await import(
    path.join(outputDir, "app", "Transitive.mjs")
  );
  assert.equal(transitive.answer, 42);

  // Deferred module aliases retain the discriminated representation when the
  // dependency is first loaded by a downstream type or value reference.
  const deferredAlias = ["-no-alias-deps", "-w", "-49"];
  succeeds(
    await compile(
      "alias",
      "AliasFacade.res",
      [producerDirectory],
      deferredAlias,
    ),
  );
  for (const source of ["AliasTypeConsumer.res", "AliasValueConsumer.res"]) {
    succeeds(
      await compile("app", source, ["alias", producerDirectory], deferredAlias),
    );
  }
  const aliased = await import(
    path.join(outputDir, "app", "AliasValueConsumer.mjs")
  );
  assert.equal(aliased.answer, 42);
} finally {
  await fs.rm(outputDir, { recursive: true, force: true });
}
