// @ts-check

import * as assert from "node:assert";
import { stripVTControlCharacters } from "node:util";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { rescript } = setup(import.meta.dirname);

const cliHelp =
  "ReScript - Fast, Simple, Fully Typed JavaScript from the Future\n" +
  "\n" +
  "Usage: rescript [OPTIONS] <COMMAND>\n" +
  "\n" +
  "Commands:\n" +
  "  build          Build the project (default command)\n" +
  "  watch          Build, then start a watcher\n" +
  "  clean          Clean the build artifacts\n" +
  "  format         Format ReScript files\n" +
  "  compiler-args  Print the compiler arguments for a ReScript source file\n" +
  "  compile-file   Compile a single file and output JavaScript to stdout\n" +
  "  help           Print this message or the help of the given subcommand(s)\n" +
  "\n" +
  "Options:\n" +
  "  -v, --verbose...  Increase logging verbosity\n" +
  "  -q, --quiet...    Decrease logging verbosity\n" +
  "  -h, --help        Print help\n" +
  "  -V, --version     Print version\n" +
  "\n" +
  "Notes:\n" +
  "  - If no command is provided, the build command is run by default. See `rescript help build` for more information.\n" +
  "  - To create a new ReScript project, or to add ReScript to an existing project, use https://github.com/rescript-lang/create-rescript-app.\n";

const buildHelp =
  "Build the project (default command)\n" +
  "\n" +
  "Usage: rescript build [OPTIONS] [FOLDER]\n" +
  "\n" +
  "Arguments:\n" +
  "  [FOLDER]  Path to the project or subproject. This folder must contain a rescript.json file [default: .]\n" +
  "\n" +
  "Options:\n" +
  "  -f, --filter <FILTER>            Filter source files by regex. E.g., filter out test files for compilation while doing feature work\n" +
  "  -v, --verbose...                 Increase logging verbosity\n" +
  "  -a, --after-build <AFTER_BUILD>  Run an additional command after build. E.g., play a sound or run a test suite when done compiling\n" +
  "  -q, --quiet...                   Decrease logging verbosity\n" +
  '      --warn-error <WARN_ERROR>    Override warning configuration from rescript.json. Example: --warn-error "+3+8+11+12+26+27+31+32+33+34+35+39+44+45+110"\n' +
  "  -n, --no-timing [<NO_TIMING>]    Disable output timing [default: false] [possible values: true, false]\n" +
  "  -h, --help                       Print help\n";

const cleanHelp =
  "Clean the build artifacts\n" +
  "\n" +
  "Usage: rescript clean [OPTIONS] [FOLDER]\n" +
  "\n" +
  "Arguments:\n" +
  "  [FOLDER]  Path to the project or subproject. This folder must contain a rescript.json file [default: .]\n" +
  "\n" +
  "Options:\n" +
  "  -v, --verbose...  Increase logging verbosity\n" +
  "  -q, --quiet...    Decrease logging verbosity\n" +
  "  -h, --help        Print help\n";

const formatHelp =
  "Format ReScript files\n" +
  "\n" +
  "Usage: rescript format [OPTIONS] [FILES]...\n" +
  "\n" +
  "Arguments:\n" +
  "  [FILES]...  Files to format. If no files are provided, all files are formatted\n" +
  "\n" +
  "Options:\n" +
  "  -c, --check          Check formatting status without applying changes\n" +
  "  -v, --verbose...     Increase logging verbosity\n" +
  "  -q, --quiet...       Decrease logging verbosity\n" +
  "  -s, --stdin <STDIN>  Read the code from stdin and print the formatted code to stdout [possible values: .res, .resi]\n" +
  "  -h, --help           Print help\n";

const compilerArgsHelp =
  "Print the compiler arguments for a ReScript source file\n" +
  "\n" +
  "Usage: rescript compiler-args [OPTIONS] <PATH>\n" +
  "\n" +
  "Arguments:\n" +
  "  <PATH>  Path to a ReScript source file (.res or .resi)\n" +
  "\n" +
  "Options:\n" +
  "  -v, --verbose...  Increase logging verbosity\n" +
  "  -q, --quiet...    Decrease logging verbosity\n" +
  "  -h, --help        Print help\n";

const compileFileHelp =
  "Compile a single file and output JavaScript to stdout\n" +
  "\n" +
  "Usage: rescript compile-file [OPTIONS] <PATH>\n" +
  "\n" +
  "Arguments:\n" +
  "  <PATH>  Path to a ReScript source file (.res or .resi)\n" +
  "\n" +
  "Options:\n" +
  "      --module-format <MODULE_FORMAT>  Module format to use (commonjs or esmodule). If not specified and multiple package-specs are configured, the first one is used with a warning\n" +
  "  -v, --verbose...                     Increase logging verbosity\n" +
  "  -q, --quiet...                       Decrease logging verbosity\n" +
  '      --warn-error <WARN_ERROR>        Override warning configuration from rescript.json. Example: --warn-error "+3+8+11+12+26+27+31+32+33+34+35+39+44+45+110"\n' +
  "  -h, --help                           Print help\n";

/**
 * @param {string[]} params
 * @param {{ stdout: string; stderr: string; status: number; }} expected
 */
async function test(params, expected) {
  const out = await rescript("", params);

  assert.equal(
    normalizeNewlines(stripVTControlCharacters(out.stdout)),
    expected.stdout,
  );
  assert.equal(
    normalizeNewlines(stripVTControlCharacters(out.stderr)),
    expected.stderr,
  );
  assert.equal(out.status, expected.status);
}

// Shows build help with --help arg
await test(["build", "--help"], {
  stdout: buildHelp,
  stderr: "",
  status: 0,
});

// Shows cli help with --help arg even if there are invalid arguments after it
await test(["--help", "-w"], { stdout: cliHelp, stderr: "", status: 0 });

// Shows build help with -h arg
await test(["build", "-h"], { stdout: buildHelp, stderr: "", status: 0 });

// Exits with build help with unknown arg
await test(["build", "--foo"], {
  stdout: "",
  stderr:
    "error: unexpected argument '--foo' found\n" +
    "\n" +
    "  tip: to pass '--foo' as a value, use '-- --foo'\n" +
    "\n" +
    "Usage: rescript build [OPTIONS] [FOLDER]\n" +
    "\n" +
    "For more information, try '--help'.\n",
  status: 2,
});

// Shows cli help with --help arg
await test(["--help"], { stdout: cliHelp, stderr: "", status: 0 });

// Shows cli help with -h arg
await test(["-h"], { stdout: cliHelp, stderr: "", status: 0 });

// Shows cli help with -h arg
await test(["help"], { stdout: cliHelp, stderr: "", status: 0 });

// Exits with cli help with unknown command
// Exits with build usage on unknown args
await test(["--foo"], {
  stdout: "",
  stderr:
    "error: unexpected argument '--foo' found\n" +
    "\n" +
    "  tip: to pass '--foo' as a value, use '-- --foo'\n" +
    "\n" +
    "Usage: rescript build [OPTIONS] [FOLDER]\n" +
    "\n" +
    "For more information, try '--help'.\n",
  status: 2,
});

// Shows clean help with --help arg
await test(["clean", "--help"], {
  stdout: cleanHelp,
  stderr: "",
  status: 0,
});

// Shows clean help with -h arg
await test(["clean", "-h"], { stdout: cleanHelp, stderr: "", status: 0 });

// Exits with clean help with unknown arg
await test(["clean", "--foo"], {
  stdout: "",
  stderr:
    "error: unexpected argument '--foo' found\n" +
    "\n" +
    "  tip: to pass '--foo' as a value, use '-- --foo'\n" +
    "\n" +
    "Usage: rescript clean [OPTIONS] [FOLDER]\n" +
    "\n" +
    "For more information, try '--help'.\n",
  status: 2,
});

// Shows format help with --help arg
await test(["format", "--help"], {
  stdout: formatHelp,
  stderr: "",
  status: 0,
});

// Shows format help with -h arg
await test(["format", "-h"], {
  stdout: formatHelp,
  stderr: "",
  status: 0,
});

// Shows compiler-args help with --help arg
await test(["compiler-args", "--help"], {
  stdout: compilerArgsHelp,
  stderr: "",
  status: 0,
});

// Shows compiler-args help with -h arg
await test(["compiler-args", "-h"], {
  stdout: compilerArgsHelp,
  stderr: "",
  status: 0,
});

// Shows compile-file help with --help arg
await test(["compile-file", "--help"], {
  stdout: compileFileHelp,
  stderr: "",
  status: 0,
});

// Shows compile-file help with -h arg
await test(["compile-file", "-h"], {
  stdout: compileFileHelp,
  stderr: "",
  status: 0,
});
