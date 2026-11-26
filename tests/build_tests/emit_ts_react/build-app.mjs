import { build as bundle } from "vite";
import { build } from "./build.mjs";

await build();
await bundle({ root: import.meta.dirname, configFile: false });
