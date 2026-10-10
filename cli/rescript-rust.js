#!/usr/bin/env node

// @ts-check

import { rescript_rust_exe } from "./common/bins.js";
import { runBuildSystem } from "./common/runBuildSystem.js";

runBuildSystem(rescript_rust_exe);
