import * as path from "node:path";

export default {
  base: process.env.VITE_BASE ?? "/",
  resolve: {
    alias: {
      "@rescript/runtime": path.join(
        import.meta.dirname,
        "node_modules",
        "@rescript",
        "runtime",
      ),
    },
  },
  server: {
    host: "127.0.0.1",
    port: 5174,
  },
  preview: {
    host: "127.0.0.1",
    port: 4174,
  },
};
