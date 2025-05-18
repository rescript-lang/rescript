#!/usr/bin/env node

// @ts-check

import * as child_process from "node:child_process";

import { rescript_editor_analysis_exe } from "./common/bins.js";

const args = process.argv.slice(2);

child_process.spawnSync(rescript_editor_analysis_exe, args, {
  stdio: "inherit",
});
