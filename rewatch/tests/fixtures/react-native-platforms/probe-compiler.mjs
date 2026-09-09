import { spawnSync } from "node:child_process";
import {
  copyFileSync,
  mkdirSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { dirname, join, relative, resolve } from "node:path";
import { fileURLToPath, pathToFileURL } from "node:url";

export const root = dirname(fileURLToPath(import.meta.url));
export const repository = resolve(root, "../../../..");
export const config = JSON.parse(
  readFileSync(join(root, "rescript.json"), "utf8"),
);
export const bsc =
  process.env.RESCRIPT_BSC_EXE ??
  join(
    repository,
    "packages/@rescript",
    `${process.platform}-${process.arch}`,
    "bin/bsc.exe",
  );
export const runtime =
  process.env.RESCRIPT_RUNTIME ??
  join(repository, "packages/@rescript/runtime");

export function compile(
  args,
  { includes = [], output, crossModuleOpt = false } = {},
) {
  return spawnSync(
    bsc,
    [
      "-runtime-path",
      runtime,
      "-bs-project-root",
      root,
      "-bs-package-name",
      config.name,
      ...(config.jsx?.version
        ? ["-bs-jsx", String(config.jsx.version)]
        : []),
      ...includes.flatMap(path => ["-I", path]),
      ...(output
        ? ["-bs-package-output", `commonjs:${relative(root, output)}:`]
        : []),
      crossModuleOpt ? "-bs-cross-module-opt" : "-bs-no-cross-module-opt",
      ...args,
    ],
    { cwd: root, encoding: "utf8" },
  );
}

export function expectSuccess(result) {
  if (result.error) throw result.error;
  if (result.status !== 0) throw new Error(result.stderr + result.stdout);
  return result;
}

// Deliberately a fixed, tiny build graph, not an alternative build system.
// Only this experimental driver understands rescript.json's proposed platforms field.
export function build({ crossModuleOpt = false, tag = "default" } = {}) {
  if (!/^[a-z-]+$/.test(tag)) throw new Error("Invalid experiment build tag");
  const work = join(root, ".build", tag);
  const output =
    tag === "default" ? join(root, "generated") : join(work, "generated");
  rmSync(work, { recursive: true, force: true });
  rmSync(output, { recursive: true, force: true });
  const shared = join(work, "shared");
  const sharedJs = join(work, "shared-js");
  for (const dir of [shared, sharedJs, output])
    mkdirSync(dir, { recursive: true });

  for (const name of ["React", "ReactNative"]) {
    expectSuccess(
      compile(
        ["-o", join(shared, `${name}.cmj`), join(root, "src", `${name}.res`)],
        { includes: [shared], output: sharedJs, crossModuleOpt },
      ),
    );
  }
  // Compile the one shared contract exactly once; both implementations consume
  // byte-identical Button.cmi files, including the identity of abstract token.
  expectSuccess(
    compile(["-o", join(shared, "Button.cmi"), join(root, "src/Button.resi")], {
      includes: [shared],
    }),
  );

  for (const platform of config.platforms) {
    if (!/^[a-z][a-z0-9_]*$/.test(platform))
      throw new Error("Invalid platform");
    const context = join(work, platform);
    const staging = join(context, "js");
    mkdirSync(staging, { recursive: true });
    copyFileSync(join(shared, "Button.cmi"), join(context, "Button.cmi"));
    const options = {
      includes: [context, shared],
      output: staging,
      crossModuleOpt,
    };
    for (const name of ["Button", "App"]) {
      const filename = name === "Button" ? `Button.${platform}.res` : "App.res";
      const result = expectSuccess(
        compile(
          [
            "-drawlambda",
            ...(name === "Button" ? ["-bs-read-cmi"] : []),
            "-o",
            join(context, `${name}.cmj`),
            join(root, "src", filename),
          ],
          options,
        ),
      );
      writeFileSync(join(context, `${name}.lambda.txt`), result.stderr);
      // An empty compiler suffix makes imports extensionless. Publish the
      // corresponding JS under Metro's platform convention, without rewriting JS.
      copyFileSync(join(staging, name), join(output, `${name}.${platform}.js`));
    }
  }
  return { work, output, shared };
}

if (
  process.argv[1] &&
  import.meta.url === pathToFileURL(resolve(process.argv[1])).href
) {
  const { output } = build();
  console.log(
    `Built ${config.platforms.join(", ")} into ${relative(root, output)}/`,
  );
}
