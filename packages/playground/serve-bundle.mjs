import { stat, readFile } from "node:fs/promises";
import * as path from "node:path";
import { H3, serve, serveStatic } from "h3";

import { rescript_compiler } from "./compiler.js";

const compilerVersion = rescript_compiler.version;
const localVersion = `${compilerVersion}-local`;

/**
 * @param {string} id
 */
function toLocalPath(id) {
  if (id === "/compiler.js") {
    return path.join(import.meta.dirname, id);
  }
  if (id.startsWith(`/${localVersion}`)) {
    return path.join(
      import.meta.dirname,
      id.replace(`/${localVersion}`, "/packages"),
    );
  }
  return undefined;
}

const app = new H3()
  .get("/versions.json", () => {
    return [localVersion];
  })
  // Supported paths
  // - /compiler.js
  // - /{compilerVersion}-local/{library}/cmij.js
  .use("**", event => {
    return serveStatic(event, {
      getContents: id => {
        const localPath = toLocalPath(id);
        return localPath && readFile(path.join(basePath, id));
      },
      getMeta: async id => {
        const localPath = toLocalPath(id);
        if (!localPath) {
          return undefined;
        }
        const stats = await stat(localPath).catch(() => {});
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
