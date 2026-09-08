#!/usr/bin/env node

// @ts-check

import { rescript_ocaml_exe } from "./common/bins.js";
import { runBuildSystem } from "./common/runBuildSystem.js";

if (rescript_ocaml_exe === undefined) {
  console.error(
    "The experimental OCaml build system is not available on Windows yet.",
  );
  process.exit(1);
} else {
  runBuildSystem(rescript_ocaml_exe);
}
