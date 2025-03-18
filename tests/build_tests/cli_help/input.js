// @ts-check

const assert = require("node:assert");
const path = require("node:path");
const { exec, normalizeNewlines } = require("../utils.js");

const rescriptPath = path.join(__dirname, "..", "..", "..", "cli", "rescript");

const cliHelp =
  "Usage: rescript <options> <subcommand>\n" +
  "\n" +
  "`rescript` is equivalent to `rescript build`\n" +
  "\n" +
  "Options:\n" +
  "  -v, -version  display version number\n" +
  "  -h, -help     display help\n" +
  "\n" +
  "Subcommands:\n" +
  "  build\n" +
  "  clean\n" +
  "  format\n" +
  "  dump\n" +
  "  help\n" +
  "\n" +
  "Run `rescript <subcommand> -h` for subcommand help. Examples:\n" +
  "  rescript build -h\n" +
  "  rescript format -h\n";

const buildHelp =
  "Usage: rescript build <options> -- <ninja_options>\n" +
  "\n" +
  "`rescript build` builds the project with dependencies\n" +
  "\n" +
  "`rescript build -- -h` for Ninja options (internal usage only; unstable)\n" +
  "\n" +
  "Options:\n" +
  "  -w           Watch mode\n" +
  "  -ws          [host]:port set up host & port for WebSocket build notifications\n" +
  "  -verbose     Set the output to be verbose\n" +
  "  -with-deps   *deprecated* This is the default behavior now. This option will be removed in a future release\n" +
  '  -warn-error  Warning numbers and whether to turn them into errors, e.g., "+8+32-102"\n';

const cleanHelp =
  "Usage: rescript clean <options>\n" +
  "\n" +
  "`rescript clean` cleans build artifacts\n" +
  "\n" +
  "Options:\n" +
  "  -verbose    Set the output to be verbose\n" +
  "  -with-deps  *deprecated* This is the default behavior now. This option will be removed in a future release\n";

const formatHelp =
  "Usage: rescript format <options> [files]\n" +
  "\n" +
  "`rescript format` formats the current directory\n" +
  "\n" +
  "Options:\n" +
  "  -stdin  [.res|.resi] Read the code from stdin and print\n" +
  "          the formatted code to stdout in ReScript syntax\n" +
  "  -all    Format the whole project \n" +
  "  -check  Check formatting for file or the whole project. Use `-all` to check the whole project\n";

const dumpHelp =
  "Usage: rescript dump <options> [target]\n" +
  "`rescript dump` dumps the information for the target\n";

/**
 * @param {string[]} params
 * @param {{ stdout: string; stderr: string; status: number; }} expected
 */
async function runTest(params, expected) {
  const out = await exec("node", [rescriptPath, ...params], {
    cwd: __dirname,
  });

  assert.equal(normalizeNewlines(out.stdout), expected.stdout);
  assert.equal(normalizeNewlines(out.stderr), expected.stderr);
  assert.equal(out.status, expected.status);
}

async function test() {
  // Shows build help with --help arg
  await runTest(["build", "--help"], {
    stdout: buildHelp,
    stderr: "",
    status: 0,
  });

  await runTest(["build", "-w", "--help"], {
    stdout: buildHelp,
    stderr: "",
    status: 0,
  });

  await runTest(["-w", "--help"], { stdout: cliHelp, stderr: "", status: 0 });

  // Shows cli help with --help arg even if there are invalid arguments after it
  await runTest(["--help", "-w"], { stdout: cliHelp, stderr: "", status: 0 });

  // Shows build help with -h arg
  await runTest(["build", "-h"], { stdout: buildHelp, stderr: "", status: 0 });

  // Exits with build help with unknown arg
  await runTest(["build", "-foo"], {
    stdout: "",
    stderr: `Error: Unknown option "-foo".\n${buildHelp}`,
    status: 2,
  });

  // Shows cli help with --help arg
  await runTest(["--help"], { stdout: cliHelp, stderr: "", status: 0 });

  // Shows cli help with -h arg
  await runTest(["-h"], { stdout: cliHelp, stderr: "", status: 0 });

  // Shows cli help with -h arg
  await runTest(["help"], { stdout: cliHelp, stderr: "", status: 0 });

  // Exits with cli help with unknown command
  await runTest(["built"], {
    stdout: "",
    stderr: `Error: Unknown command "built".\n${cliHelp}`,
    status: 2,
  });

  // Exits with build help with unknown args
  await runTest(["-foo"], {
    stdout: "",
    stderr: `Error: Unknown option "-foo".\n${buildHelp}`,
    status: 2,
  });

  // Shows clean help with --help arg
  await runTest(["clean", "--help"], {
    stdout: cleanHelp,
    stderr: "",
    status: 0,
  });

  // Shows clean help with -h arg
  await runTest(["clean", "-h"], { stdout: cleanHelp, stderr: "", status: 0 });

  // Exits with clean help with unknown arg
  await runTest(["clean", "-foo"], {
    stdout: "",
    stderr: `Error: Unknown option "-foo".\n${cleanHelp}`,
    status: 2,
  });

  // Shows format help with --help arg
  await runTest(["format", "--help"], {
    stdout: formatHelp,
    stderr: "",
    status: 0,
  });

  // Shows format help with -h arg
  await runTest(["format", "-h"], {
    stdout: formatHelp,
    stderr: "",
    status: 0,
  });

  // Shows dump help with --help arg
  await runTest(["dump", "--help"], {
    stdout: dumpHelp,
    stderr: "",
    status: 0,
  });

  // Shows dump help with -h arg
  await runTest(["dump", "-h"], { stdout: dumpHelp, stderr: "", status: 0 });
}

void test();
