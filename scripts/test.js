//@ts-check
const cp = require("child_process");
const path = require("path");
const fs = require("fs");
const { rescript_exe } = require("#cli/bin_path");

const duneBinDir = require("./dune").duneBinDir;

const { exec } = require("../tests/build_tests/utils.js");

let ounitTest = false;
let mochaTest = false;
let bsbTest = false;
let formatTest = false;
let runtimeDocstrings = false;

if (process.argv.includes("-ounit")) {
  ounitTest = true;
}

if (process.argv.includes("-mocha")) {
  mochaTest = true;
}

if (process.argv.includes("-bsb")) {
  bsbTest = true;
}

if (process.argv.includes("-format")) {
  formatTest = true;
}

if (process.argv.includes("-docstrings")) {
  runtimeDocstrings = true;
}

if (process.argv.includes("-all")) {
  ounitTest = true;
  mochaTest = true;
  bsbTest = true;
  formatTest = true;
  runtimeDocstrings = true;
}

async function runTests() {
  if (formatTest) {
    cp.execSync("npm run checkFormat", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });

    cp.execSync("bash scripts/format_check.sh", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });
  }

  if (ounitTest) {
    if (process.platform === "win32") {
      console.log("Skipping OUnit tests on Windows");
    } else {
      cp.execSync(path.join(duneBinDir, "ounit_tests"), {
        stdio: [0, 1, 2],
      });
    }
  }

  if (mochaTest) {
    cp.execSync(`${rescript_exe} clean`, {
      cwd: path.join(__dirname, "..", "tests/tests"),
      stdio: [0, 1, 2],
    });

    cp.execSync(`${rescript_exe} build`, {
      cwd: path.join(__dirname, "..", "tests/tests"),
      stdio: [0, 1, 2],
    });

    cp.execSync("npx mocha -t 10000 tests/tests/**/*_test.mjs", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });

    cp.execSync("node tests/tests/src/core/Core_TestSuite.mjs", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });

    cp.execSync("node tests/tests/src/core/Core_TempTests.mjs", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });
  }

  if (bsbTest) {
    console.log("Doing build_tests");
    const buildTestDir = path.join(__dirname, "..", "tests", "build_tests");
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
        console.log(`testing ${file}`);

        // note existsSync test already ensure that it is a directory
        const out = await exec(`node`, ["input.js"], { cwd: testDir });
        console.log(out.stdout);

        if (out.status === 0) {
          console.log("✅ success in", file);
        } else {
          console.log(`❌ error in ${file} with stderr:\n`, out.stderr);
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
    } else {
      console.log("Running runtime docstrings tests");

      const generated_mocha_test_res = path.join(
        "tests",
        "docstring_tests",
        "generated_mocha_test.res",
      );

      // Remove `generated_mocha_test.res` if file exists
      if (fs.existsSync(generated_mocha_test_res)) {
        console.log(`Removing ${generated_mocha_test_res}`);
        fs.unlinkSync(generated_mocha_test_res);
      }

      cp.execSync(`${rescript_exe} build`, {
        cwd: path.join(__dirname, "..", "tests/docstring_tests"),
        stdio: [0, 1, 2],
      });

      // Generate rescript file with all tests `generated_mocha_test.res`
      cp.execSync(
        `node ${path.join("tests", "docstring_tests", "DocTest.res.mjs")}`,
        {
          cwd: path.join(__dirname, ".."),
          stdio: [0, 1, 2],
        },
      );

      // Build again to check if generated_mocha_test.res has syntax or type erros
      cp.execSync(`${rescript_exe} build`, {
        cwd: path.join(__dirname, "..", "tests/docstring_tests"),
        stdio: [0, 1, 2],
      });

      // Format generated_mocha_test.res
      console.log("Formatting generated_mocha_test.res");
      cp.execSync(`./cli/rescript format ${generated_mocha_test_res}`, {
        cwd: path.join(__dirname, ".."),
        stdio: [0, 1, 2],
      });

      console.log("Run mocha test");
      cp.execSync(
        `npx mocha ${path.join("tests", "docstring_tests", "generated_mocha_test.res.mjs")}`,
        {
          cwd: path.join(__dirname, ".."),
          stdio: [0, 1, 2],
        },
      );
    }
  }
}

runTests();
