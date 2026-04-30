// @ts-check

import * as assert from "node:assert";
import * as fs from "node:fs/promises";
import * as path from "node:path";
import { setup } from "#dev/process";

const { execBuildOrThrow, execClean } = setup(import.meta.dirname);

await execBuildOrThrow();

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

const sourcePath = path.join(import.meta.dirname, "src", "Demo.res");
const source = await fs.readFile(sourcePath, "utf8");
const originalDebuggerPositions = findTokenPositions(source, "%debugger");
assert.equal(originalDebuggerPositions.length, 2);

for (const filename of ["Demo.cjs", "Demo.mjs"]) {
  const jsPath = path.join(import.meta.dirname, "lib", "bs", "src", filename);
  const mapPath = `${jsPath}.map`;

  const js = await fs.readFile(jsPath, "utf8");
  assert.match(
    js,
    new RegExp(`//# sourceMappingURL=${filename.replace(".", "\\.")}\\.map`),
  );

  const map = JSON.parse(await fs.readFile(mapPath, "utf8"));
  assert.equal(map.version, 3);
  assert.equal(map.file, filename);
  assert.ok(map.mappings.length > 0, `${filename}.map should include mappings`);
  assert.ok(
    map.sources.some(source => source.endsWith("Demo.res")),
    `${filename}.map should include Demo.res, got ${map.sources.join(", ")}`,
  );
  assert.ok(
    map.sourcesContent.some(content => content.includes("let add = (a, b)")),
    `${filename}.map should include source contents`,
  );

  const generatedDebuggerPositions = findTokenPositions(js, "debugger");
  assert.equal(generatedDebuggerPositions.length, 2);

  const decodedMappings = decodeMappings(map.mappings);
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
    originalDebuggerPositions,
  );
  assert.ok(
    debuggerMappings.every(mapping =>
      map.sources[mapping.sourceIndex].endsWith("Demo.res"),
    ),
    `${filename}.map debugger mappings should point to Demo.res`,
  );
}

await execClean();
