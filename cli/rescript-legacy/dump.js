// @ts-check

import * as child_process from "node:child_process";
import * as path from "node:path";

import * as arg from "#cli/args";

const dump_usage = `Usage: rescript dump <options> [target]
\`rescript dump\` dumps the information for the target
`;

/**
 * @type {arg.specs}
 */
const specs = [];

/**
 * @param {string[]} argv
 * @param {string} rescript_legacy_exe
 * @param {string} bsc_exe
 */
export function main(argv, rescript_legacy_exe, bsc_exe) {
  let target;
  arg.parse_exn(dump_usage, argv, specs, xs => {
    if (xs.length !== 1) {
      arg.bad_arg(`Expect only one target, ${xs.length} found`);
    }
    target = xs[0];
  });

  const { ext } = path.parse(target);
  if (ext !== ".cmi") {
    console.error("Only .cmi target allowed");
    process.exit(2);
  }

  let output = child_process.spawnSync(
    rescript_legacy_exe,
    ["build", "--", target],
    {
      encoding: "utf-8",
    },
  );
  if (output.status !== 0) {
    console.log(output.stdout);
    console.error(output.stderr);
    process.exit(2);
  }
  output = child_process.spawnSync(bsc_exe, [path.join("lib", "bs", target)], {
    encoding: "utf-8",
  });
  console.log(output.stdout.trimEnd());
  if (output.status !== 0) {
    console.error(output.stderr);
    process.exit(2);
  }
}
