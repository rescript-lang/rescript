import { promises as fs } from "node:fs";
import { createRequire } from "node:module";
import path from "node:path";
import { fileURLToPath } from "node:url";

/**
 * 🚨 Why this hack exists:
 *
 * Unlike Node or Bun, Deno's `import.meta.resolve("npm:...")` does NOT return a
 * filesystem path. It just echoes back the npm: specifier. The actual package
 * tarballs are unpacked into `node_modules/.deno/...` when you use
 * `--node-modules-dir`, and normal `node_modules/<pkg>` symlinks only exist for
 * *direct* dependencies. Transitive deps (like @rescript/runtime in our case)
 * only live inside `.deno/` and have no symlink.
 *
 * Because Deno doesn't expose an API for “give me the absolute path of this npm
 * package”, the only way to emulate Node’s/Bun’s `require.resolve` behaviour is
 * to glob inside `.deno/` and reconstruct the path manually.
 *
 * TL;DR: This function exists to compensate for the fact that Deno deliberately hides its
 * npm cache layout. If you want a stable on‑disk path for a package in Deno,
 * you have to spelunk `node_modules/.deno/>pkg@version>/node_modules/<pkg>`.
 *
 * If Deno ever ships a proper API for this, replace this hack immediately.
 */
async function resolvePackageInDeno(pkgName) {
  const base = path.resolve("node_modules/.deno");
  const pkgId = pkgName.startsWith("@") ? pkgName.replace("/", "+") : pkgName;

  const { expandGlob } = await import("https://deno.land/std/fs/mod.ts");
  for await (const entry of expandGlob(
    path.join(base, `${pkgId}@*/node_modules/${pkgName}`),
  )) {
    if (entry.isDirectory) {
      return await fs.realpath(entry.path);
    }
  }

  throw new Error(
    `Could not resolve ${pkgName} in Deno. Did you enable --node-modules-dir?`,
  );
}

async function resolvePackageRoot(pkgName) {
  const specifier = `${pkgName}/package.json`;

  if (typeof import.meta.resolve === "function") {
    const url = import.meta.resolve(specifier);

    if (url.startsWith("file://")) {
      // Node & Bun: real local file
      const abs = path.dirname(fileURLToPath(url));
      return await fs.realpath(abs);
    }

    if (typeof globalThis.Deno !== "undefined") {
      return await resolvePackageInDeno(pkgName);
    }

    throw new Error(
      `Could not resolve ${pkgName} (no physical path available)`,
    );
  }

  // Node fallback
  const require = createRequire(import.meta.url);
  try {
    const abs = path.dirname(require.resolve(`${pkgName}/package.json`));
    return await fs.realpath(abs);
  } catch {
    throw new Error(`Could not resolve ${pkgName} in Node runtime`);
  }
}

export const runtimePath = await resolvePackageRoot("@rescript/runtime");
