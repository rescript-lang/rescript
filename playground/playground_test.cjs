// Playground bundle is UMD module
// It uses `module.exports` in current context, or fallback to `globalThis`
const { rescript_compiler } = require("./compiler.cjs");

require("./packages/compiler-builtins/cmij.cjs");
require("./packages/@rescript/react/cmij.cjs");

const compiler = rescript_compiler.make();

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

    let make = ({a}) => {
      <A a/>
    }
  }

  let sum = [1,2,3]
    ->Array.map(x => x * 2)
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
