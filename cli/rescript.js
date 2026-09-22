#!/usr/bin/env node

import { rescript_exe } from "./common/bins.js";
import { runBuildSystem } from "./common/runBuildSystem.js";

runBuildSystem(rescript_exe);
