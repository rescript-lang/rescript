// @ts-check

import * as assert from "node:assert";
import { stripVTControlCharacters } from "node:util";
import { setup } from "#dev/process";
import { normalizeNewlines } from "#dev/utils";

const { rescript } = setup(import.meta.dirname);

const cliHelp =
  "NAME\n" +
  "       rescript - Fast, Simple, Fully Typed JavaScript from the Future\n" +
  "\n" +
  "SYNOPSIS\n" +
  "       rescript [COMMAND] …\n" +
  "\n" +
  "NOTES\n" +
  "       If no command is provided, the build command is run by default. See\n" +
  "       rescript help build for more information.\n" +
  "\n" +
  "       To create a new ReScript project, or to add ReScript to an existing\n" +
  "       project, use https://github.com/rescript-lang/create-rescript-app.\n" +
  "\n" +
  "COMMANDS\n" +
  "       build [OPTION]… [FOLDER]\n" +
  "           Build the project.\n" +
  "\n" +
  "       clean [--prod] [--quiet] [--verbose] [OPTION]… [FOLDER]\n" +
  "           Clean build artifacts.\n" +
  "\n" +
  "       compiler-args [--quiet] [--verbose] [OPTION]… PATH\n" +
  "           Print compiler arguments for a ReScript source file.\n" +
  "\n" +
  "       format [OPTION]… [FILES]…\n" +
  "           Format ReScript files.\n" +
  "\n" +
  "       help [OPTION]… [COMMAND]\n" +
  "           Print this message or command help.\n" +
  "\n" +
  "       watch [OPTION]… [FOLDER]\n" +
  "           Build, then start a watcher.\n" +
  "\n" +
  "ARGUMENTS\n" +
  "       FOLDER (absent=.)\n" +
  "           Path to the project or subproject containing rescript.json.\n" +
  "\n" +
  "OPTIONS\n" +
  "       -a COMMAND, --after-build=COMMAND\n" +
  "           Run an additional command after a successful build.\n" +
  "\n" +
  "       -f REGEX, --filter=REGEX\n" +
  "           Filter source files by regular expression.\n" +
  "\n" +
  "       --features=FEATURES\n" +
  "           Restrict the current package to comma-separated features.\n" +
  "\n" +
  "       -n, --no-timing\n" +
  "           Disable output timing.\n" +
  "\n" +
  "       --prod\n" +
  "           Skip development dependencies and sources.\n" +
  "\n" +
  "       -q, --quiet\n" +
  "           Decrease logging verbosity.\n" +
  "\n" +
  "       -v, --verbose\n" +
  "           Increase logging verbosity.\n" +
  "\n" +
  "       --warn-error=WARNINGS\n" +
  "           Override warning configuration from rescript.json.\n" +
  "\n" +
  "COMMON OPTIONS\n" +
  "       --help[=FMT] (default=auto)\n" +
  "           Show this help in format FMT. The value FMT must be one of auto,\n" +
  "           pager, groff or plain. With auto, the format is pager or plain\n" +
  "           whenever the TERM env var is dumb or undefined.\n" +
  "\n" +
  "       --version\n" +
  "           Show version information.\n" +
  "\n" +
  "EXIT STATUS\n" +
  "       rescript exits with:\n" +
  "\n" +
  "       0   on success.\n" +
  "\n" +
  "       1   on build, configuration, or file system errors.\n" +
  "\n" +
  "       2   on command-line usage errors and invalid package dependencies.\n" +
  "\n" +
  "       129-143\n" +
  "           when interrupted by a signal (128 plus the signal number).\n" +
  "\n";

const buildHelp =
  "NAME\n" +
  "       rescript-build - Build the project.\n" +
  "\n" +
  "SYNOPSIS\n" +
  "       rescript build [OPTION]… [FOLDER]\n" +
  "\n" +
  "ARGUMENTS\n" +
  "       FOLDER (absent=.)\n" +
  "           Path to the project or subproject containing rescript.json.\n" +
  "\n" +
  "OPTIONS\n" +
  "       -a COMMAND, --after-build=COMMAND\n" +
  "           Run an additional command after a successful build.\n" +
  "\n" +
  "       -f REGEX, --filter=REGEX\n" +
  "           Filter source files by regular expression.\n" +
  "\n" +
  "       --features=FEATURES\n" +
  "           Restrict the current package to comma-separated features.\n" +
  "\n" +
  "       -n, --no-timing\n" +
  "           Disable output timing.\n" +
  "\n" +
  "       --prod\n" +
  "           Skip development dependencies and sources.\n" +
  "\n" +
  "       -q, --quiet\n" +
  "           Decrease logging verbosity.\n" +
  "\n" +
  "       -v, --verbose\n" +
  "           Increase logging verbosity.\n" +
  "\n" +
  "       --warn-error=WARNINGS\n" +
  "           Override warning configuration from rescript.json.\n" +
  "\n" +
  "COMMON OPTIONS\n" +
  "       --help[=FMT] (default=auto)\n" +
  "           Show this help in format FMT. The value FMT must be one of auto,\n" +
  "           pager, groff or plain. With auto, the format is pager or plain\n" +
  "           whenever the TERM env var is dumb or undefined.\n" +
  "\n" +
  "       --version\n" +
  "           Show version information.\n" +
  "\n" +
  "EXIT STATUS\n" +
  "       rescript build exits with:\n" +
  "\n" +
  "       0   on success.\n" +
  "\n" +
  "       1   on build, configuration, or file system errors.\n" +
  "\n" +
  "       2   on command-line usage errors and invalid package dependencies.\n" +
  "\n" +
  "       129-143\n" +
  "           when interrupted by a signal (128 plus the signal number).\n" +
  "\n" +
  "SEE ALSO\n" +
  "       rescript(1)\n" +
  "\n";

const cleanHelp =
  "NAME\n" +
  "       rescript-clean - Clean build artifacts.\n" +
  "\n" +
  "SYNOPSIS\n" +
  "       rescript clean [--prod] [--quiet] [--verbose] [OPTION]… [FOLDER]\n" +
  "\n" +
  "ARGUMENTS\n" +
  "       FOLDER (absent=.)\n" +
  "           Path to the project or subproject containing rescript.json.\n" +
  "\n" +
  "OPTIONS\n" +
  "       --prod\n" +
  "           Skip development dependencies and sources.\n" +
  "\n" +
  "       -q, --quiet\n" +
  "           Decrease logging verbosity.\n" +
  "\n" +
  "       -v, --verbose\n" +
  "           Increase logging verbosity.\n" +
  "\n" +
  "COMMON OPTIONS\n" +
  "       --help[=FMT] (default=auto)\n" +
  "           Show this help in format FMT. The value FMT must be one of auto,\n" +
  "           pager, groff or plain. With auto, the format is pager or plain\n" +
  "           whenever the TERM env var is dumb or undefined.\n" +
  "\n" +
  "       --version\n" +
  "           Show version information.\n" +
  "\n" +
  "EXIT STATUS\n" +
  "       rescript clean exits with:\n" +
  "\n" +
  "       0   on success.\n" +
  "\n" +
  "       1   on build, configuration, or file system errors.\n" +
  "\n" +
  "       2   on command-line usage errors and invalid package dependencies.\n" +
  "\n" +
  "       129-143\n" +
  "           when interrupted by a signal (128 plus the signal number).\n" +
  "\n" +
  "SEE ALSO\n" +
  "       rescript(1)\n" +
  "\n";

const formatHelp =
  "NAME\n" +
  "       rescript-format - Format ReScript files.\n" +
  "\n" +
  "SYNOPSIS\n" +
  "       rescript format [OPTION]… [FILES]…\n" +
  "\n" +
  "OPTIONS\n" +
  "       -c, --check\n" +
  "           Check formatting without modifying files.\n" +
  "\n" +
  "       -q, --quiet\n" +
  "           Decrease logging verbosity.\n" +
  "\n" +
  "       -s EXTENSION, --stdin=EXTENSION\n" +
  "           Read stdin and write formatted source to stdout.\n" +
  "\n" +
  "       -v, --verbose\n" +
  "           Increase logging verbosity.\n" +
  "\n" +
  "COMMON OPTIONS\n" +
  "       --help[=FMT] (default=auto)\n" +
  "           Show this help in format FMT. The value FMT must be one of auto,\n" +
  "           pager, groff or plain. With auto, the format is pager or plain\n" +
  "           whenever the TERM env var is dumb or undefined.\n" +
  "\n" +
  "       --version\n" +
  "           Show version information.\n" +
  "\n" +
  "EXIT STATUS\n" +
  "       rescript format exits with:\n" +
  "\n" +
  "       0   on success.\n" +
  "\n" +
  "       1   on build, configuration, or file system errors.\n" +
  "\n" +
  "       2   on command-line usage errors and invalid package dependencies.\n" +
  "\n" +
  "       129-143\n" +
  "           when interrupted by a signal (128 plus the signal number).\n" +
  "\n" +
  "SEE ALSO\n" +
  "       rescript(1)\n" +
  "\n";

const compilerArgsHelp =
  "NAME\n" +
  "       rescript-compiler-args - Print compiler arguments for a ReScript\n" +
  "       source file.\n" +
  "\n" +
  "SYNOPSIS\n" +
  "       rescript compiler-args [--quiet] [--verbose] [OPTION]… PATH\n" +
  "\n" +
  "ARGUMENTS\n" +
  "       PATH (required)\n" +
  "           ReScript source file (.res or .resi).\n" +
  "\n" +
  "OPTIONS\n" +
  "       -q, --quiet\n" +
  "           Decrease logging verbosity.\n" +
  "\n" +
  "       -v, --verbose\n" +
  "           Increase logging verbosity.\n" +
  "\n" +
  "COMMON OPTIONS\n" +
  "       --help[=FMT] (default=auto)\n" +
  "           Show this help in format FMT. The value FMT must be one of auto,\n" +
  "           pager, groff or plain. With auto, the format is pager or plain\n" +
  "           whenever the TERM env var is dumb or undefined.\n" +
  "\n" +
  "       --version\n" +
  "           Show version information.\n" +
  "\n" +
  "EXIT STATUS\n" +
  "       rescript compiler-args exits with:\n" +
  "\n" +
  "       0   on success.\n" +
  "\n" +
  "       1   on build, configuration, or file system errors.\n" +
  "\n" +
  "       2   on command-line usage errors and invalid package dependencies.\n" +
  "\n" +
  "       129-143\n" +
  "           when interrupted by a signal (128 plus the signal number).\n" +
  "\n" +
  "SEE ALSO\n" +
  "       rescript(1)\n" +
  "\n";

/**
 * @param {string[]} params
 * @param {{ stdout: string; stderr: string; status: number; }} expected
 */
async function test(params, expected) {
  // A color-capable TERM makes Cmdliner style its output. The streams are
  // pipes, so the CLI must still write plain text, as clap does.
  /** @type {NodeJS.ProcessEnv} */
  const env = { ...process.env, TERM: "xterm" };
  delete env.NO_COLOR;
  delete env.CLICOLOR_FORCE;
  const out = await rescript("", params, { env });
  assert.equal(stripVTControlCharacters(out.stdout), out.stdout);
  assert.equal(stripVTControlCharacters(out.stderr), out.stderr);
  const stdout = normalizeNewlines(out.stdout);
  const stderr = normalizeNewlines(out.stderr);

  assert.equal(stdout, expected.stdout);
  assert.equal(stderr, expected.stderr);
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
    "Usage: rescript build [--help] [OPTION]… [FOLDER]\n" +
    "rescript: unknown option --foo. Did you mean -f?\n",
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
    "Usage: rescript build [--help] [OPTION]… [FOLDER]\n" +
    "rescript: unknown option --foo. Did you mean -f?\n",
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
    "Usage: rescript clean [--help] [--prod] [--quiet] [--verbose] [OPTION]…\n" +
    "       [FOLDER]\n" +
    "rescript: unknown option --foo\n",
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
