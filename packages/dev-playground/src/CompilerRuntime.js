function pathFromBase(relativePath) {
  const origin = globalThis.location?.origin ?? "http://localhost";
  const base = new URL(import.meta.env.BASE_URL || "/", origin);
  return new URL(relativePath, base).pathname.replace(/\/$/, "");
}

function parseCompilerVersions(defaultVersion) {
  const raw = import.meta.env.VITE_COMPILER_VERSIONS;
  if (raw == null || raw === "") {
    return [{ id: defaultVersion, label: defaultVersion }];
  }

  try {
    const versions = JSON.parse(raw);
    if (
      Array.isArray(versions) &&
      versions.every(
        version =>
          typeof version?.id === "string" &&
          typeof version?.label === "string",
      )
    ) {
      return versions;
    }
  } catch {
    // Fall through to the default list below.
  }

  return [{ id: defaultVersion, label: defaultVersion }];
}

const compilerRoot = pathFromBase("playground-bundles");
const loadedScripts = new Map();
const compilerApis = new Map();
const compilers = new Map();
const loadedLibrariesByVersion = new Map();
let activeLibraryVersion = null;

export const defaultWarnFlags = "+a-4-9-20-40-41-42-50-61-102-109";
export const defaultCompilerVersion =
  import.meta.env.VITE_DEFAULT_COMPILER_VERSION ?? "local";
export const availableCompilerVersions = parseCompilerVersions(
  defaultCompilerVersion,
);

function loadScript(src, { cache = true } = {}) {
  if (cache && loadedScripts.has(src)) {
    return loadedScripts.get(src);
  }

  const promise = new Promise((resolve, reject) => {
    const script = document.createElement("script");
    script.src = src;
    script.async = true;
    script.onload = () => resolve();
    script.onerror = () => reject(new Error(`Could not load ${src}`));
    document.head.appendChild(script);
  });

  if (cache) {
    loadedScripts.set(src, promise);
  }
  return promise;
}

function hasFunction(value, name) {
  return value != null && typeof value[name] === "function";
}

function versionRoot(version) {
  return `${compilerRoot}/${version || defaultCompilerVersion}`;
}

function applyConfig(instance, config) {
  if (instance == null || config == null) {
    return;
  }

  if (hasFunction(instance, "setModuleSystem")) {
    instance.setModuleSystem(config.moduleSystem || "esmodule");
  }
  if (hasFunction(instance, "setWarnFlags")) {
    instance.setWarnFlags(config.warnFlags || defaultWarnFlags);
  }
  if (hasFunction(instance, "setFilename")) {
    instance.setFilename("Playground.res");
  }
  if (hasFunction(instance, "setJsxPreserveMode")) {
    instance.setJsxPreserveMode(Boolean(config.jsxPreserveMode));
  }
  if (hasFunction(instance, "setExperimentalFeatures")) {
    instance.setExperimentalFeatures(config.experimentalFeatures || []);
  }
}

function normalizeConfig(rawConfig) {
  const rawModuleSystem = rawConfig?.module_system ?? "esmodule";
  const moduleSystem =
    rawModuleSystem === "es6"
      ? "esmodule"
      : rawModuleSystem === "nodejs"
        ? "commonjs"
        : rawModuleSystem;

  return {
    moduleSystem,
    warnFlags: rawConfig?.warn_flags ?? defaultWarnFlags,
    jsxPreserveMode: Boolean(rawConfig?.jsx_preserve_mode),
    experimentalFeatures: rawConfig?.experimental_features ?? [],
  };
}

function formatLocation(item) {
  const row = item?.row ?? 0;
  const column = item?.column ?? 0;
  return row > 0 ? `Line ${row}, ${column}` : "Compiler";
}

function warningToText(item) {
  const prefix = item?.isError ? "error" : "warning";
  const warnNumber = item?.warnNumber == null ? "" : ` ${item.warnNumber}`;
  const message = item?.shortMsg ?? item?.fullMsg ?? "Unknown warning";
  return `${formatLocation(item)}: ${prefix}${warnNumber}: ${message}`;
}

function errorToText(item) {
  const message = item?.shortMsg ?? item?.fullMsg ?? "Unknown compiler error";
  return `${formatLocation(item)}: ${message}`;
}

function normalizeFailure(result, elapsedMs) {
  const errors = Array.isArray(result?.errors) ? result.errors.map(errorToText) : [];
  const warnings = Array.isArray(result?.warnings) ? result.warnings.map(warningToText) : [];
  const message =
    result?.msg ??
    result?.shortMsg ??
    result?.fullMsg ??
    (errors.length > 0 ? errors[0] : "Compilation failed");

  return {
    ok: false,
    kind: result?.type ?? "error",
    jsCode: "",
    parsetree: "",
    typedtree: "",
    lambda: "",
    lam: "",
    errors,
    warnings,
    message,
    time: elapsedMs,
  };
}

function normalizeSuccess(result, elapsedMs) {
  const fallback =
    "This local compiler bundle does not expose this debug dump yet.";

  return {
    ok: true,
    kind: "success",
    jsCode: result?.js_code ?? "",
    parsetree: result?.parsetree ?? fallback,
    typedtree: result?.typedtree ?? fallback,
    lambda: result?.lambda ?? fallback,
    lam: result?.lam ?? fallback,
    errors: [],
    warnings: Array.isArray(result?.warnings) ? result.warnings.map(warningToText) : [],
    message: "Compiled successfully",
    time: elapsedMs,
  };
}

async function loadRuntimeLibraries(version) {
  const selectedVersion = version || defaultCompilerVersion;
  if (activeLibraryVersion === selectedVersion) {
    return;
  }

  const root = versionRoot(selectedVersion);
  const libraries = ["compiler-builtins"];
  await loadScript(`${root}/compiler-builtins/cmij.js`, { cache: false });

  try {
    await loadScript(`${root}/@rescript/react/cmij.js`, { cache: false });
    libraries.push("@rescript/react");
  } catch {
    // React is optional for the developer shell.
  }

  loadedLibrariesByVersion.set(selectedVersion, libraries);
  activeLibraryVersion = selectedVersion;
}

async function ensureCompilerApi(version) {
  const selectedVersion = version || defaultCompilerVersion;
  if (compilerApis.has(selectedVersion)) {
    await loadRuntimeLibraries(selectedVersion);
    return compilerApis.get(selectedVersion);
  }

  const root = versionRoot(selectedVersion);
  await loadScript(`${root}/compiler.js`);
  await loadRuntimeLibraries(selectedVersion);

  const api = globalThis.rescript_compiler;
  if (api == null || typeof api.make !== "function") {
    throw new Error("rescript_compiler global was not registered by compiler.js");
  }

  compilerApis.set(selectedVersion, api);
  return api;
}

async function ensureCompiler(version) {
  const selectedVersion = version || defaultCompilerVersion;
  const api = await ensureCompilerApi(selectedVersion);

  if (compilers.has(selectedVersion)) {
    return compilers.get(selectedVersion);
  }

  const instance = api.make();
  applyConfig(instance, {
    moduleSystem: "esmodule",
    warnFlags: defaultWarnFlags,
    jsxPreserveMode: false,
    experimentalFeatures: [],
  });

  compilers.set(selectedVersion, instance);
  return instance;
}

export async function init(version) {
  const selectedVersion = version || defaultCompilerVersion;
  const instance = await ensureCompiler(selectedVersion);
  const config = normalizeConfig(instance.getConfig?.());

  return {
    bundleId: selectedVersion,
    version: instance.version ?? instance.rescript?.version ?? "unknown",
    apiVersion: compilerApis.get(selectedVersion)?.api_version ?? "unknown",
    moduleSystem: config.moduleSystem,
    warnFlags: config.warnFlags,
    jsxPreserveMode: config.jsxPreserveMode,
    experimentalFeatures: config.experimentalFeatures,
    libraries: loadedLibrariesByVersion.get(selectedVersion) ?? ["compiler-builtins"],
  };
}

export async function compile(source, config) {
  const selectedVersion = config?.compilerVersion || defaultCompilerVersion;
  const instance = await ensureCompiler(selectedVersion);
  applyConfig(instance, config);

  const start = performance.now();
  const result = hasFunction(instance.rescript, "compileWithDebug")
    ? instance.rescript.compileWithDebug(source, [
      "parsetree",
      "typedtree",
      "lambda",
      "lam",
    ])
    : instance.rescript.compile(source);
  const elapsedMs = performance.now() - start;

  if (result?.type === "success") {
    return normalizeSuccess(result, elapsedMs);
  }

  return normalizeFailure(result, elapsedMs);
}
