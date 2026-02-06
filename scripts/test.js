// @ts-check

import * as fs from "node:fs";
import * as os from "node:os";
import * as path from "node:path";
import {
  buildTestDir,
  compilerTestDir,
  docstringTestDir,
  ounitTestBin,
  projectDir,
} from "#dev/paths";

import {
  execBin,
  execBuild,
  execClean,
  mocha,
  node,
  rescript,
  shell,
  yarn,
} from "#dev/process";

let ounitTest = false;
let mochaTest = false;
let buildTest = false;
let formatTest = false;
let runtimeDocstrings = false;
let rewatchTest = false;

if (process.argv.includes("-ounit")) {
  ounitTest = true;
}

if (process.argv.includes("-mocha")) {
  mochaTest = true;
}

if (process.argv.includes("-build")) {
  buildTest = true;
}

if (process.argv.includes("-format")) {
  formatTest = true;
}

if (process.argv.includes("-docstrings")) {
  runtimeDocstrings = true;
}

if (process.argv.includes("-rewatch")) {
  rewatchTest = true;
}

if (process.argv.includes("-all")) {
  ounitTest = true;
  mochaTest = true;
  buildTest = true;
  formatTest = true;
  runtimeDocstrings = true;
  rewatchTest = true;
}

if (formatTest) {
  await shell("./scripts/format_check.sh", [], {
    cwd: projectDir,
    stdio: "inherit",
  });
}

if (ounitTest) {
  if (process.platform === "win32") {
    console.log("Skipping OUnit tests on Windows");
  } else {
    await execBin(ounitTestBin, [], {
      stdio: "inherit",
    });
  }
}

if (mochaTest) {
  // No need to clean beforehand, rewatch detects changes to the compiler binary
  // and rebuilds automatically in that case.
  await execBuild([], {
    cwd: compilerTestDir,
    stdio: "inherit",
  });

  await mocha(
    [
      "-t",
      "10000",
      "tests/tests/src/**/*_test.mjs",
      // Ignore the preserve_jsx_test.mjs file.
      // I can't run because Mocha doesn't support jsx.
      // We also want to keep the output as is.
      "--ignore",
      "tests/tests/src/jsx_preserve_test.mjs",
    ],
    {
      cwd: projectDir,
      stdio: "inherit",
    },
  );

  await node("tests/tests/src/stdlib/Stdlib_TestSuite.mjs", [], {
    cwd: projectDir,
    stdio: "inherit",
  });

  await node("tests/tests/src/stdlib/Stdlib_TempTests.mjs", [], {
    cwd: projectDir,
    stdio: "inherit",
  });

  // CommonJS tests
  const commonjsTestDir = path.join(projectDir, "tests/commonjs_tests");
  await execBuild([], {
    cwd: commonjsTestDir,
    stdio: "inherit",
  });

  await node("tests/commonjs_tests/src/belt_import.js", [], {
    cwd: projectDir,
    stdio: "inherit",
  });
}

if (buildTest) {
  console.log("Doing build_tests");
  const files = fs.readdirSync(buildTestDir);

  let hasError = false;

  for (const file of files) {
    const testDir = path.join(buildTestDir, file);
    if (file === "node_modules" || !fs.lstatSync(testDir).isDirectory()) {
      continue;
    }
    if (!fs.existsSync(path.join(testDir, "input.js"))) {
      console.warn(`input.js does not exist in ${testDir}`);
    } else {
      // note existsSync test already ensure that it is a directory
      const out = await node("input.js", [], { cwd: testDir });
      process.stdout.write(out.stdout);

      if (out.status === 0) {
        console.log(`✅ success in ${file}`);
      } else {
        console.log(`❌ error in ${file} with stderr:\n${out.stderr}`);
        hasError = true;
      }
    }
  }

  if (hasError) {
    process.exit(1);
  }
}

if (runtimeDocstrings) {
  if (process.platform === "win32") {
    console.log(`Skipping docstrings tests on ${process.platform}`);
  } else if (process.platform === "darwin" && os.release().startsWith("22")) {
    // Workaround for intermittent hangs in CI
    console.log("Skipping docstrings tests on macOS 13");
  } else {
    console.log("Running runtime docstrings tests");

    const generated_mocha_test_res = path.join(
      docstringTestDir,
      "generated_mocha_test.res",
    );

    await execClean([], {
      cwd: docstringTestDir,
      stdio: "inherit",
    });

    await execBuild([], {
      cwd: docstringTestDir,
      stdio: "inherit",
    });

    // Generate rescript file with all tests `generated_mocha_test.res`
    await node(path.join(docstringTestDir, "DocTest.res.js"), [], {
      cwd: projectDir,
      stdio: "inherit",
    });

    // Build again to check if generated_mocha_test.res has syntax or type erros
    await execBuild([], {
      cwd: docstringTestDir,
      stdio: "inherit",
    });

    // Format generated_mocha_test.res
    console.log("Formatting generated_mocha_test.res");
    await rescript("format", [generated_mocha_test_res], {
      cwd: projectDir,
      stdio: "inherit",
    });

    console.log("Run mocha test");
    await mocha([path.join(docstringTestDir, "generated_mocha_test.res.js")], {
      cwd: projectDir,
      stdio: "inherit",
    });
  }
}

if (rewatchTest) {
  console.log("Running rewatch tests");
  await yarn("workspace rewatch-tests test", [], {
    cwd: projectDir,
    stdio: "inherit",
  });
}
