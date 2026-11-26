import { createServer } from "vite";
import { build } from "./build.mjs";

await build();
const server = await createServer({
  root: import.meta.dirname,
  configFile: false,
  server: { host: "127.0.0.1" },
});
await server.listen();
server.printUrls();
