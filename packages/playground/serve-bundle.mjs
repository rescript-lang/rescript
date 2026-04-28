import { stat, readFile } from "node:fs/promises";
import * as path from "node:path";
import { H3, serve, serveStatic } from "h3";

import compilerBundle from "./compiler.js";

const compilerVersion = compilerBundle.rescript_compiler.version;
const localVersion = "v" + compilerVersion.toString();
/**
 * @param {string} id
 */
function toLocalPath(id) {
  const originalId = id.slice(localVersion.length + 1);
  if (originalId === "/compiler.js") {
    return path.join(import.meta.dirname, originalId);
  }
  return path.join(
    import.meta.dirname,
    "packages",
    originalId,
  );
}

const versionContent = new H3()
  .get("/**", event => {
    return serveStatic(event, {
      getContents: id => {
        const localPath = toLocalPath(id);
        return localPath && readFile(localPath);
      },
      getMeta: async id => {
        const localPath = toLocalPath(id);
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

const app = new H3()
  .get("/playground-bundles/versions.json", () => [localVersion])
  .mount(`/${localVersion}`, versionContent);

serve(app, { port: 8888 });
