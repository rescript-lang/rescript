#!/usr/bin/env node

// @ts-check

import * as child_process from "node:child_process";
import { rewatch_exe } from "./common/bins.js";

const args = process.argv.slice(2);

child_process.spawnSync(rewatch_exe, args, { stdio: "inherit" });
