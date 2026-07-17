// @ts-check

import * as assert from "node:assert";
import { Buffer } from "node:buffer";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuildOrThrow, execClean, node } = setup(import.meta.dirname);

const base64VlqChars =
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

function decodeVlq(segment) {
  const values = [];
  let value = 0;
  let shift = 0;

  for (const char of segment) {
    let digit = base64VlqChars.indexOf(char);
    assert.notEqual(digit, -1, `invalid base64 VLQ character: ${char}`);

    const continuation = (digit & 32) !== 0;
    digit &= 31;
    value += digit << shift;

    if (continuation) {
      shift += 5;
    } else {
      values.push(value & 1 ? -(value >> 1) : value >> 1);
      value = 0;
      shift = 0;
    }
  }

  return values;
}

function decodeMappings(mappings) {
  const decoded = [];
  let previousSource = 0;
  let previousOriginalLine = 0;
  let previousOriginalColumn = 0;

  mappings.split(";").forEach((line, generatedLine) => {
    let previousGeneratedColumn = 0;

    for (const segment of line.split(",")) {
      if (segment === "") {
        continue;
      }

      const fields = decodeVlq(segment);
      previousGeneratedColumn += fields[0];

      if (fields.length >= 4) {
        previousSource += fields[1];
        previousOriginalLine += fields[2];
        previousOriginalColumn += fields[3];
        decoded.push({
          generatedLine,
          generatedColumn: previousGeneratedColumn,
          sourceIndex: previousSource,
          originalLine: previousOriginalLine,
          originalColumn: previousOriginalColumn,
        });
      }
    }
  });

  return decoded;
}

function findTokenPositions(content, token) {
  return content.split(/\r?\n/).flatMap((line, lineIndex) => {
    const positions = [];
    let column = line.indexOf(token);

    while (column !== -1) {
      positions.push({ line: lineIndex, column });
      column = line.indexOf(token, column + token.length);
    }

    return positions;
  });
}

function findSingleTokenPosition(content, token) {
  const positions = findTokenPositions(content, token);
  assert.equal(positions.length, 1, `${token} should appear exactly once`);
  return positions[0];
}

async function fileExists(filename) {
  try {
    await fs.access(filename);
    return true;
  } catch {
    return false;
  }
}

function mapFromInlineComment(js, filename) {
  const match = js.match(
    /\/\/# sourceMappingURL=data:application\/json;base64,([A-Za-z0-9+/=]+)\s*$/,
  );
  assert.ok(match, `${filename} should include an inline source map`);
  return JSON.parse(Buffer.from(match[1], "base64").toString("utf8"));
}

const moduleNames = ["Demo", "Helper"];
const outputFilenames = moduleNames.flatMap(moduleName => [
  `${moduleName}.cjs`,
  `${moduleName}.mjs`,
]);

const demoSourcePath = path.join(import.meta.dirname, "src", "Demo.res");
const demoSource = await fs.readFile(demoSourcePath, "utf8");
const helperSourcePath = path.join(import.meta.dirname, "src", "Helper.res");
const helperSource = await fs.readFile(helperSourcePath, "utf8");
const configPath = path.join(import.meta.dirname, "rescript.json");
const originalConfig = await fs.readFile(configPath, "utf8");
const originalDebuggerPositions = findTokenPositions(demoSource, "%debugger");
assert.equal(originalDebuggerPositions.length, 2);
const originalRaiseErrorPositions = findTokenPositions(
  demoSource,
  "Js.Exn.raiseError",
);
assert.equal(originalRaiseErrorPositions.length, 2);
const originalPipeCallPositions = findTokenPositions(demoSource, "input->fn");
assert.equal(originalPipeCallPositions.length, 1);
const originalPatternBranchPositions = [
  findSingleTokenPosition(demoSource, "Int.toString(value)"),
  findSingleTokenPosition(demoSource, "Int.toString(left + right)"),
];
const originalHelperRaiseErrorPositions = findTokenPositions(
  helperSource,
  "Js.Exn.raiseError",
);
assert.equal(originalHelperRaiseErrorPositions.length, 1);
const originalHelperPipeCallPositions = findTokenPositions(
  helperSource,
  "value->fn",
);
assert.equal(originalHelperPipeCallPositions.length, 1);
const originalHelperPatternBranchPositions = [
  findSingleTokenPosition(helperSource, 'payload.label ++ ":empty"'),
  findSingleTokenPosition(helperSource, "Int.toString(count)"),
];

function configWithSourceMap(sourceMap) {
  const config = JSON.parse(originalConfig);
  config.sourceMap =
    typeof sourceMap === "object" && sourceMap !== null
      ? {
          ...config.sourceMap,
          ...sourceMap,
        }
      : sourceMap;
  return `${JSON.stringify(config, null, 2)}\n`;
}

function configWithoutSourceMap() {
  const config = JSON.parse(originalConfig);
  delete config.sourceMap;
  return `${JSON.stringify(config, null, 2)}\n`;
}

function configWithMode(mode) {
  return configWithSourceMap({ mode });
}

async function removeGeneratedMapFiles() {
  for (const filename of outputFilenames) {
    await fs.rm(path.join(import.meta.dirname, "src", `${filename}.map`), {
      force: true,
    });
    await fs.rm(
      path.join(import.meta.dirname, "lib", "bs", "src", `${filename}.map`),
      { force: true },
    );
  }
}

function moduleNameFromFilename(filename) {
  return filename.slice(0, filename.indexOf("."));
}

function sourceMapFixture(filename) {
  switch (moduleNameFromFilename(filename)) {
    case "Demo":
      return {
        sourceName: "Demo.res",
        sourceContentMarker: "let add = (a, b)",
        raiseErrorPositions: originalRaiseErrorPositions,
        pipeCallPositions: originalPipeCallPositions,
        patternBranchPositions: originalPatternBranchPositions,
        debuggerPositions: originalDebuggerPositions,
      };
    case "Helper":
      return {
        sourceName: "Helper.res",
        sourceContentMarker: "let multiply = (left, right)",
        raiseErrorPositions: originalHelperRaiseErrorPositions,
        pipeCallPositions: originalHelperPipeCallPositions,
        patternBranchPositions: originalHelperPatternBranchPositions,
        debuggerPositions: [],
      };
    default:
      throw new Error(`Unknown source map fixture for ${filename}`);
  }
}

function assertSourceMap(filename, js, map, options = {}) {
  const { expectSourcesContent = true, sourceRoot = undefined } = options;
  const fixture = sourceMapFixture(filename);
  assert.equal(map.version, 3);
  assert.equal(map.file, filename);
  assert.ok(map.mappings.length > 0, `${filename}.map should include mappings`);
  if (sourceRoot === undefined) {
    assert.equal(
      map.sourceRoot,
      undefined,
      `${filename}.map should not include sourceRoot`,
    );
  } else {
    assert.equal(map.sourceRoot, sourceRoot);
  }
  assert.ok(
    map.sources.some(source => source.endsWith(fixture.sourceName)),
    `${filename}.map should include ${fixture.sourceName}, got ${map.sources.join(", ")}`,
  );
  if (expectSourcesContent) {
    assert.ok(
      map.sourcesContent.some(content =>
        content.includes(fixture.sourceContentMarker),
      ),
      `${filename}.map should include source contents`,
    );
  } else {
    assert.equal(
      map.sourcesContent,
      undefined,
      `${filename}.map should not include source contents`,
    );
  }

  const generatedDebuggerPositions = findTokenPositions(js, "debugger");
  assert.equal(
    generatedDebuggerPositions.length,
    fixture.debuggerPositions.length,
  );

  const decodedMappings = decodeMappings(map.mappings);
  const generatedRaiseErrorPositions = findTokenPositions(
    js,
    "Stdlib_Exn.raiseError",
  );
  assert.equal(
    generatedRaiseErrorPositions.length,
    fixture.raiseErrorPositions.length,
  );
  assert.deepEqual(
    generatedRaiseErrorPositions.map(position => {
      const mapping = decodedMappings.find(
        decoded =>
          decoded.generatedLine === position.line &&
          decoded.generatedColumn === position.column,
      );
      assert.ok(
        mapping,
        `${filename}.map should include an exact mapping for raiseError at ${position.line}:${position.column}`,
      );
      assert.ok(
        map.sources[mapping.sourceIndex].endsWith(fixture.sourceName),
        `${filename}.map raiseError mapping should point to ${fixture.sourceName}`,
      );
      return {
        line: mapping.originalLine,
        column: mapping.originalColumn,
      };
    }),
    fixture.raiseErrorPositions,
  );

  for (const [label, positions] of [
    ["pipe call", fixture.pipeCallPositions],
    ["pattern branch", fixture.patternBranchPositions],
  ]) {
    for (const position of positions) {
      const mapping = decodedMappings.find(
        decoded =>
          decoded.originalLine === position.line &&
          map.sources[decoded.sourceIndex].endsWith(fixture.sourceName),
      );
      assert.ok(
        mapping,
        `${filename}.map should include a ${label} mapping for ${fixture.sourceName}:${position.line}`,
      );
    }
  }

  const debuggerMappings = generatedDebuggerPositions.map(position => {
    const mapping = decodedMappings.find(
      decoded =>
        decoded.generatedLine === position.line &&
        decoded.generatedColumn === position.column,
    );
    assert.ok(
      mapping,
      `${filename}.map should include an exact mapping for debugger at ${position.line}:${position.column}`,
    );
    return mapping;
  });

  assert.deepEqual(
    debuggerMappings.map(mapping => ({
      line: mapping.originalLine,
      column: mapping.originalColumn,
    })),
    fixture.debuggerPositions,
  );
  assert.ok(
    debuggerMappings.every(mapping =>
      map.sources[mapping.sourceIndex].endsWith(fixture.sourceName),
    ),
    `${filename}.map debugger mappings should point to ${fixture.sourceName}`,
  );
}

async function assertLinkedOutput() {
  await fs.writeFile(configPath, configWithMode("linked"));
  await execBuildOrThrow();

  for (const filename of outputFilenames) {
    const jsPath = path.join(import.meta.dirname, "lib", "bs", "src", filename);
    const mapPath = `${jsPath}.map`;

    const js = await fs.readFile(jsPath, "utf8");
    if (filename.startsWith("Demo.")) {
      assert.match(
        js,
        /\/\* @__PURE__ \*\/Primitive_exceptions\.create/,
        `${filename} should preserve real JS comments while source maps are enabled`,
      );
    }
    assert.match(
      js,
      new RegExp(`//# sourceMappingURL=${filename.replace(".", "\\.")}\\.map`),
    );

    assertSourceMap(
      filename,
      js,
      JSON.parse(await fs.readFile(mapPath, "utf8")),
    );
  }

  await assertNodeStackTraceUsesSourceMap();
}

async function assertNodeStackTraceUsesSourceMap() {
  const stackTraceScriptPath = path.join(
    import.meta.dirname,
    ".source-map-stack-trace.cjs",
  );
  await fs.writeFile(
    stackTraceScriptPath,
    `
    const Demo = require("./lib/bs/src/Demo.cjs");
    Demo.unicodeCrash();
  `,
  );

  try {
    const { status, stderr } = await node("--enable-source-maps", [
      stackTraceScriptPath,
    ]);

    assert.notEqual(status, 0, "Node source map stack test should throw");
    assert.match(
      stderr,
      /Demo\.res:\d+:\d+/,
      `Node stack trace should point to Demo.res, got:\n${stderr}`,
    );
  } finally {
    await fs.rm(stackTraceScriptPath, { force: true });
  }
}

async function assertHiddenOutput() {
  const sourceRoot = "rescript://source-map-test/";
  await removeGeneratedMapFiles();
  await fs.writeFile(
    configPath,
    configWithSourceMap({ mode: "hidden", sourceRoot }),
  );
  await execBuildOrThrow();

  for (const filename of outputFilenames) {
    const jsPath = path.join(import.meta.dirname, "lib", "bs", "src", filename);
    const mapPath = `${jsPath}.map`;

    const js = await fs.readFile(jsPath, "utf8");
    assert.doesNotMatch(
      js,
      /\/\/# sourceMappingURL=/,
      `${filename} should not include a sourceMappingURL comment in hidden mode`,
    );
    assert.ok(await fileExists(mapPath), `${filename}.map should exist`);
    assertSourceMap(
      filename,
      js,
      JSON.parse(await fs.readFile(mapPath, "utf8")),
      { sourceRoot },
    );
  }
}

async function assertDefaultOutput() {
  await fs.writeFile(configPath, configWithoutSourceMap());
  await execBuildOrThrow();

  const output = new Map();
  for (const filename of outputFilenames) {
    const jsPath = path.join(import.meta.dirname, "lib", "bs", "src", filename);
    const mapPath = `${jsPath}.map`;
    const js = await fs.readFile(jsPath, "utf8");

    assert.doesNotMatch(
      js,
      /\/\/# sourceMappingURL=/,
      `${filename} should not include a sourceMappingURL comment by default`,
    );
    assert.equal(
      await fileExists(mapPath),
      false,
      `${filename}.map should not exist by default`,
    );
    output.set(filename, js);
  }

  return output;
}

async function assertDisabledOutput(defaultOutput) {
  await fs.writeFile(configPath, configWithSourceMap(false));
  await execBuildOrThrow();

  for (const filename of outputFilenames) {
    const jsPath = path.join(import.meta.dirname, "lib", "bs", "src", filename);
    const mapPath = `${jsPath}.map`;

    const js = await fs.readFile(jsPath, "utf8");
    assert.doesNotMatch(
      js,
      /\/\/# sourceMappingURL=/,
      `${filename} should not include a sourceMappingURL comment when source maps are disabled`,
    );
    assert.equal(
      await fileExists(mapPath),
      false,
      `${filename}.map should be removed when source maps are disabled`,
    );
    assert.equal(
      js,
      defaultOutput.get(filename),
      `${filename} should be unchanged when source maps are disabled`,
    );
  }
}

async function assertInlineOutput() {
  await fs.writeFile(
    configPath,
    configWithSourceMap({ mode: "inline", sourcesContent: false }),
  );
  await execBuildOrThrow();

  for (const filename of outputFilenames) {
    const jsPath = path.join(import.meta.dirname, "lib", "bs", "src", filename);
    const mapPath = `${jsPath}.map`;

    const js = await fs.readFile(jsPath, "utf8");
    assert.match(
      js,
      /\/\/# sourceMappingURL=data:application\/json;base64,/,
      `${filename} should include an inline source map`,
    );
    assert.equal(
      await fileExists(mapPath),
      false,
      `${filename}.map should not exist in inline mode`,
    );
    assertSourceMap(filename, js, mapFromInlineComment(js, filename), {
      expectSourcesContent: false,
    });
  }
}

await execClean();
try {
  const defaultOutput = await assertDefaultOutput();
  await assertLinkedOutput();
  await assertDisabledOutput(defaultOutput);
  await assertHiddenOutput();
  await assertInlineOutput();
} finally {
  await fs.writeFile(configPath, originalConfig);
  await execClean();
}
