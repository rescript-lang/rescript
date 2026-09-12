#!/usr/bin/env node

// @ts-check

import { rescript_ocaml_exe } from "./common/bins.js";
import { runBuildSystem } from "./common/runBuildSystem.js";

if (rescript_ocaml_exe === undefined) {
  console.error(
    "The OCaml build-system binary is not available for this platform.",
  );
  process.exit(1);
} else {
  runBuildSystem(rescript_ocaml_exe);
}
