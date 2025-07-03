#!/usr/bin/env node

// @ts-check

// This script is supposed to be running in project root directory
// It matters since we need read .sourcedirs(location)
// and its content are file/directories with regard to project root

import * as fs from "node:fs";
import * as tty from "node:tty";

import { bsc_exe, rescript_legacy_exe } from "./common/bins.js";
import * as bsb from "./common/bsb.js";

const cwd = process.cwd();
process.env.BSB_PROJECT_ROOT = cwd;

if (process.env.FORCE_COLOR === undefined) {
  if (tty.isatty(1)) {
    process.env.FORCE_COLOR = "1";
    process.env.NINJA_ANSI_FORCED = "1";
  }
} else {
  if (
    process.env.FORCE_COLOR === "1" &&
    process.env.NINJA_ANSI_FORCED === undefined
  ) {
    process.env.NINJA_ANSI_FORCED = "1";
  }
  if (process.argv.includes("-verbose")) {
    console.log(`FORCE_COLOR: "${process.env.FORCE_COLOR}"`);
  }
}

const helpMessage = `Usage: rescript <options> <subcommand>

\`rescript\` is equivalent to \`rescript build\`

Options:
  -v, -version  display version number
  -h, -help     display help

Subcommands:
  build
  clean
  format
  dump
  help

Run \`rescript <subcommand> -h\` for subcommand help. Examples:
  rescript build -h
  rescript format -h`;

function onUncaughtException(err) {
  console.error("Uncaught Exception", err);
  bsb.releaseBuild();
  process.exit(1);
}

function exitProcess() {
  bsb.releaseBuild();
  process.exit(0);
}

process.on("uncaughtException", onUncaughtException);

// OS signal handlers
// Ctrl+C
process.on("SIGINT", exitProcess);
// kill pid
try {
  process.on("SIGUSR1", exitProcess);
  process.on("SIGUSR2", exitProcess);
  process.on("SIGTERM", exitProcess);
  process.on("SIGHUP", exitProcess);
} catch (_e) {
  // Deno might throw an error here, see https://github.com/denoland/deno/issues/9995
  // TypeError: Windows only supports ctrl-c (SIGINT) and ctrl-break (SIGBREAK).
}

const args = process.argv.slice(2);
const argPatterns = {
  help: ["help", "-h", "-help", "--help"],
  version: ["version", "-v", "-version", "--version"],
};

const helpArgIndex = args.findIndex(arg => argPatterns.help.includes(arg));
const firstPositionalArgIndex = args.findIndex(arg => !arg.startsWith("-"));

if (
  helpArgIndex !== -1 &&
  (firstPositionalArgIndex === -1 || helpArgIndex <= firstPositionalArgIndex)
) {
  console.log(helpMessage);
} else if (argPatterns.version.includes(args[0])) {
  const packageSpec = JSON.parse(
    fs.readFileSync(new URL("../package.json", import.meta.url), "utf-8"),
  );

  console.log(packageSpec.version);
} else if (firstPositionalArgIndex !== -1) {
  const subcmd = args[firstPositionalArgIndex];
  const subcmdArgs = args.slice(firstPositionalArgIndex + 1);

  switch (subcmd) {
    case "info": {
      bsb.info(subcmdArgs);
      break;
    }
    case "clean": {
      bsb.clean(subcmdArgs);
      break;
    }
    case "build": {
      bsb.build(subcmdArgs);
      break;
    }
    case "format": {
      const mod = await import("./rescript-legacy/format.js");
      await mod.main(subcmdArgs, rescript_legacy_exe, bsc_exe);
      break;
    }
    case "dump": {
      const mod = await import("./rescript-legacy/dump.js");
      mod.main(subcmdArgs, rescript_legacy_exe, bsc_exe);
      break;
    }
    default: {
      console.error(`Error: Unknown command "${subcmd}".\n${helpMessage}`);
      process.exit(2);
    }
  }
} else {
  bsb.build(args);
}
