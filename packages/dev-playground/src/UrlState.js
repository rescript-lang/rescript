const textEncoder = new TextEncoder();
const textDecoder = new TextDecoder();
const maxEncodedCodeLength = 300 * 1024;
const maxDecodedSourceLength = 200 * 1024;
let replaceSequence = 0;

function supportsDecompression() {
  return typeof DecompressionStream !== "undefined";
}

function normalizeModuleSystem(value, fallback) {
  if (value === "esmodule" || value === "es6") {
    return "esmodule";
  }
  if (value === "commonjs" || value === "nodejs") {
    return "commonjs";
  }
  return fallback;
}

function bytesToBase64Url(bytes) {
  let binary = "";
  const chunkSize = 0x8000;
  for (let index = 0; index < bytes.length; index += chunkSize) {
    const chunk = bytes.subarray(index, index + chunkSize);
    binary += String.fromCharCode(...chunk);
  }
  return btoa(binary)
    .replace(/\+/g, "-")
    .replace(/\//g, "_")
    .replace(/=+$/g, "");
}

function base64UrlToBytes(value) {
  const base64 = value.replace(/-/g, "+").replace(/_/g, "/");
  const padded = base64.padEnd(Math.ceil(base64.length / 4) * 4, "=");
  const binary = atob(padded);
  const bytes = new Uint8Array(binary.length);
  for (let index = 0; index < binary.length; index += 1) {
    bytes[index] = binary.charCodeAt(index);
  }
  return bytes;
}

async function gunzip(bytes) {
  const stream = new Blob([bytes])
    .stream()
    .pipeThrough(new DecompressionStream("gzip"));
  return new Uint8Array(await new Response(stream).arrayBuffer());
}

async function encodeCode(source) {
  const sourceBytes = textEncoder.encode(source);
  // Keep newly generated share links browser-portable. Older z: links are still
  // decoded when the browser exposes DecompressionStream.
  return `b:${bytesToBase64Url(sourceBytes)}`;
}

async function decodeCode(encoded) {
  if (encoded.startsWith("z:")) {
    if (!supportsDecompression()) {
      throw new Error(
        "Compressed shared links require browser DecompressionStream support",
      );
    }

    const compressedBytes = base64UrlToBytes(encoded.slice(2));
    return textDecoder.decode(await gunzip(compressedBytes));
  }

  if (encoded.startsWith("b:")) {
    return textDecoder.decode(base64UrlToBytes(encoded.slice(2)));
  }

  return encoded;
}

function currentParams() {
  return new URLSearchParams(window.location.search);
}

function applyUrlState(
  encoded,
  compilerVersion,
  moduleSystem,
  warnFlags,
  jsxPreserveMode,
  experimentalFeatures,
) {
  const params = currentParams();
  params.set("code", encoded);
  params.set("version", compilerVersion);
  params.set("module", moduleSystem);
  params.set("warn", warnFlags);

  if (jsxPreserveMode) {
    params.set("jsxPreserve", "true");
  } else {
    params.delete("jsxPreserve");
  }

  if (experimentalFeatures.length > 0) {
    params.set("experimental", experimentalFeatures.join(","));
  } else {
    params.delete("experimental");
  }

  const query = params.toString();
  const nextUrl = `${window.location.pathname}${query === "" ? "" : `?${query}`}${window.location.hash}`;
  window.history.replaceState(null, "", nextUrl);
}

async function copyText(value) {
  if (navigator.clipboard?.writeText != null && window.isSecureContext) {
    await navigator.clipboard.writeText(value);
    return;
  }

  const textarea = document.createElement("textarea");
  textarea.value = value;
  textarea.setAttribute("readonly", "");
  textarea.style.position = "fixed";
  textarea.style.top = "-9999px";
  textarea.style.left = "-9999px";
  document.body.appendChild(textarea);
  textarea.select();

  try {
    if (!document.execCommand("copy")) {
      throw new Error("Copy command failed");
    }
  } finally {
    document.body.removeChild(textarea);
  }
}

export async function initialSource(defaultSource) {
  const encoded = currentParams().get("code");
  if (
    encoded == null ||
    encoded === "" ||
    encoded.length > maxEncodedCodeLength
  ) {
    return defaultSource;
  }

  try {
    const decoded = await decodeCode(encoded);
    return decoded.length <= maxDecodedSourceLength ? decoded : defaultSource;
  } catch (error) {
    console.warn("Could not restore shared playground source", error);
    return defaultSource;
  }
}

export function queryCompilerVersion(defaultVersion) {
  return currentParams().get("version") || defaultVersion;
}

export function queryModuleSystem(defaultModuleSystem) {
  return normalizeModuleSystem(
    currentParams().get("module"),
    defaultModuleSystem,
  );
}

export function queryWarnFlags(defaultWarnFlags) {
  return currentParams().get("warn") || defaultWarnFlags;
}

export function queryJsxPreserveMode(defaultValue) {
  const value = currentParams().get("jsxPreserve");
  if (value == null) {
    return defaultValue;
  }
  return value === "true" || value === "1";
}

export function queryExperimentalFeatures() {
  const value = currentParams().get("experimental");
  if (value == null || value === "") {
    return [];
  }
  return value.split(",").filter(Boolean);
}

export async function replaceUrlState(
  source,
  compilerVersion,
  moduleSystem,
  warnFlags,
  jsxPreserveMode,
  experimentalFeatures,
) {
  const sequence = ++replaceSequence;
  const encoded = await encodeCode(source);
  if (sequence !== replaceSequence) {
    return;
  }

  applyUrlState(
    encoded,
    compilerVersion,
    moduleSystem,
    warnFlags,
    jsxPreserveMode,
    experimentalFeatures,
  );
}

export async function copyUrlState(
  source,
  compilerVersion,
  moduleSystem,
  warnFlags,
  jsxPreserveMode,
  experimentalFeatures,
) {
  replaceSequence += 1;
  const encoded = await encodeCode(source);
  applyUrlState(
    encoded,
    compilerVersion,
    moduleSystem,
    warnFlags,
    jsxPreserveMode,
    experimentalFeatures,
  );

  const href = window.location.href;
  await copyText(href);
  return href;
}
