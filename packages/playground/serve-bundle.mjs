import { stat, readFile } from "node:fs/promises";
import * as path from "node:path";
import { H3, serve, serveStatic } from "h3";

import { rescript_compiler } from "./compiler.js";

const compilerVersion = rescript_compiler.version;

const app = new H3()
  .get("/versions.json", () => {
    return [`${compilerVersion}-local`];
  })
  .use("**", event => {
    return serveStatic(event, {
      getContents: id => {
        const basePath = id === "/compiler.js"
          ? import.meta.dirname
          : path.join(import.meta.dirname, "packages");
        return readFile(path.join(basePath, id));
      },
      getMeta: async id => {
        const basePath = id === "/compiler.js"
          ? import.meta.dirname
          : path.join(import.meta.dirname, "packages");
        const stats = await stat(path.join(basePath, id)).catch(() => {});
        if (stats?.isFile()) {
          return {
            size: stats.size,
            mtime: stats.mtimeMs,
          };
        }
      },
    });
  });

serve(app, { port: 8888 });
