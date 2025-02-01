//@ts-check
var cp = require("child_process");
var path = require("path");
var fs = require("fs");

var duneBinDir = require("./dune").duneBinDir;

const { exec } = require("./utils.js");

var ounitTest = false;
var mochaTest = false;
var bsbTest = false;
var formatTest = false;
var all = false;

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

if (process.argv.includes("-all")) {
  all = true;
}
if (all) {
  ounitTest = true;
  mochaTest = true;
  bsbTest = true;
  formatTest = true;
}

async function runTests() {
  if (ounitTest) {
    cp.execSync(path.join(duneBinDir, "ounit_tests"), {
      stdio: [0, 1, 2],
    });
  }

  // running generated js tests
  if (mochaTest) {
    cp.execSync(`npx mocha -t 10000 jscomp/test/**/*test.js`, {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });
  }

  if (bsbTest) {
    console.log("Doing build_tests");
    var buildTestDir = path.join(__dirname, "..", "jscomp", "build_tests");
    var files = fs.readdirSync(buildTestDir);
    for (const file of files) {
      var testDir = path.join(buildTestDir, file);
      if (file === "node_modules" || !fs.lstatSync(testDir).isDirectory()) {
        return;
      }
      if (!fs.existsSync(path.join(testDir, "input.js"))) {
        console.warn(`input.js does not exist in ${testDir}`);
      } else {
        console.log(`testing ${file}`);

        // note existsSync test already ensure that it is a directory
        const out = await exec(`node`, ["input.js"], { cwd: testDir });
        console.log(out.stdout);

        if (out.code === 0) {
          console.log("✅ success in", file);
        } else {
          console.log(`❌ error in ${file} with stderr:\n`, out.stderr);
        }
      }
    }
  }

  if (formatTest) {
    cp.execSync("npm run checkFormat", {
      cwd: path.join(__dirname, ".."),
      stdio: [0, 1, 2],
    });
  }
}

runTests();
