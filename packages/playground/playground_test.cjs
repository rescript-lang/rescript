// Playground bundle is UMD module
// It uses `module.exports` in current context, or fallback to `globalThis`
const assert = require("node:assert/strict");
const { rescript_compiler } = require("./compiler.js");

require("./packages/compiler-builtins/cmij.js");
require("./packages/@rescript/belt/cmij.js");
require("./packages/@rescript/react/cmij.js");

const compiler = rescript_compiler.make();

console.log("Initial compiler config: ", compiler.getConfig());

compiler.setExperimentalFeatures(["LetUnwrap"]);
compiler.setJsxPreserveMode(true);

console.log("Current compiler config: ", compiler.getConfig());

const result = compiler.rescript.compile(`
  @@jsxConfig({ version: 4, mode: "automatic" })

  module A = {
    @react.component
    let make = (~a) => {
      // This should yield a warning (unused variable state)
      let state = React.useState(() => 0)
      <div> {React.string(a)} </div>
    }
  }

  module B = {
    type props = { a: string }

    @react.componentWithProps
    let make = ({a}) => {
      <A a/>
    }
  }

  let sum = [1,2,3]
    ->Belt.Array.map(x => x * 2)
    ->Array.reduce(0, (acc, item) => acc + item)


  // Test uncurried behavior
  let runFn = (f, x) => f(x)
  runFn(x => x + 1, 1)->Console.log

  Console.log("Hello world!")

  let a = <B a="hello" />
`);

if (result.js_code !== "") {
  console.log("-- Playground test output --");
  console.log(`ReScript version: ${compiler.rescript.version}`);
  console.log("----");
  if (result.type === "unexpected_error") {
    console.log("UNEXPECTED ERROR");
    console.log(result);
    process.exit(1);
  }
  if (result.errors && result.errors.length > 0) {
    console.log("COMPILATION ERROR");
    for (const error of result.errors) {
      console.log(error.shortMsg);
    }
    process.exit(1);
  }

  if (result.warnings.length === 0) {
    console.log("TEST FAILED");
    console.log("The code should have at least one warning.");
    process.exit(1);
  }

  console.log(result.js_code);
  console.log("-- Playground test complete --");
}

assert.equal(compiler.setGentypeEnabled(true), true);
const defaultGentypeResult = compiler.rescript.compileWithDebug(
  "@genType let answer = 42\n",
);
assert.equal(defaultGentypeResult.type, "success");
assert.match(
  defaultGentypeResult.gentype,
  /require\(['"]\.\/playground\.js['"]\)/,
);
assert.doesNotMatch(defaultGentypeResult.gentype, /playground\.bs\.js/);

compiler.setFilename("Foo.res");
const customGentypeResult = compiler.rescript.compileWithDebug(
  "@genType let answer = 42\n",
);
assert.equal(customGentypeResult.type, "success");
assert.match(customGentypeResult.gentype, /require\(['"]\.\/Foo\.js['"]\)/);
assert.doesNotMatch(customGentypeResult.gentype, /Playground\.js/);
assert.equal(compiler.setGentypeEnabled(false), true);
compiler.setFilename("Playground.res");

console.log("-- Playground gentype filename test complete --");

const sourceMapSource = `let double = value => value * 2
let result = double(21)
`;

assert.equal(compiler.setSourceMapMode("linked"), true);
assert.equal(compiler.setSourceMapSourcesContent(true), true);
assert.equal(compiler.setSourceMapRoot("rescript://playground/"), true);
assert.deepEqual(
  {
    mode: compiler.getConfig().source_map_mode,
    sourcesContent: compiler.getConfig().source_map_sources_content,
    sourceRoot: compiler.getConfig().source_map_root,
  },
  {
    mode: "linked",
    sourcesContent: true,
    sourceRoot: "rescript://playground/",
  },
);

const linkedResult = compiler.rescript.compile(sourceMapSource);
assert.equal(linkedResult.type, "success");
assert.match(linkedResult.js_code, /\/\/# sourceMappingURL=Playground\.js\.map\n$/);

const sourceMap = JSON.parse(linkedResult.source_map);
assert.equal(sourceMap.version, 3);
assert.equal(sourceMap.file, "Playground.js");
assert.equal(sourceMap.sourceRoot, "rescript://playground/");
assert.ok(sourceMap.sources.some(source => source.endsWith("Playground.res")));
assert.ok(sourceMap.sourcesContent.includes(sourceMapSource));
assert.ok(sourceMap.mappings.length > 0);

assert.equal(compiler.setSourceMapMode("inline"), true);
const inlineResult = compiler.rescript.compile(sourceMapSource);
assert.equal(inlineResult.type, "success");
assert.match(
  inlineResult.js_code,
  /\/\/# sourceMappingURL=data:application\/json;base64,[A-Za-z0-9+/=]+\n$/,
);
assert.deepEqual(JSON.parse(inlineResult.source_map), sourceMap);

assert.equal(compiler.setSourceMapMode("hidden"), true);
const hiddenResult = compiler.rescript.compile(sourceMapSource);
assert.equal(hiddenResult.type, "success");
assert.doesNotMatch(hiddenResult.js_code, /\/\/# sourceMappingURL=/);
assert.deepEqual(JSON.parse(hiddenResult.source_map), sourceMap);

assert.equal(compiler.setSourceMapMode("unsupported"), false);
assert.equal(compiler.getConfig().source_map_mode, "hidden");

assert.equal(compiler.setSourceMapMode("false"), true);
const disabledResult = compiler.rescript.compile(sourceMapSource);
assert.equal(disabledResult.type, "success");
assert.equal(disabledResult.source_map, undefined);
assert.doesNotMatch(disabledResult.js_code, /\/\/# sourceMappingURL=/);

console.log("-- Playground source map test complete --");
