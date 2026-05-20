import * as path from "node:path";

export default {
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
