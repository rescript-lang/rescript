open PlaygroundConfig

let maxEncodedCodeLength = 300 * 1024
let maxDecodedSourceLength = 200 * 1024
let replaceSequence = ref(0)

type urlSearchParams

module UrlSearchParams = {
  @new external make: string => urlSearchParams = "URLSearchParams"
  @send @return(nullable) external get: (urlSearchParams, string) => option<string> = "get"
}

module Location = {
  @val external search: string = "window.location.search"
}

let getParam = name => {
  UrlSearchParams.make(Location.search)->UrlSearchParams.get(name)
}

let encodeCode = async source => {
  ignore(source)
  let encoded: string = %raw(`
    (() => {
      const bytes = new TextEncoder().encode(source);
      let binary = "";
      const chunkSize = 0x8000;
      for (let index = 0; index < bytes.length; index += chunkSize) {
        const chunk = bytes.subarray(index, index + chunkSize);
        binary += String.fromCharCode(...chunk);
      }
      return "b:" + btoa(binary)
        .replace(/\+/g, "-")
        .replace(/\//g, "_")
        .replace(/=+$/g, "");
    })()
  `)
  encoded
}

let decodeCode = encoded => {
  ignore(encoded)
  let decoded: promise<string> = %raw(`
    (async () => {
      const base64UrlToBytes = value => {
        const base64 = value.replace(/-/g, "+").replace(/_/g, "/");
        const padded = base64.padEnd(Math.ceil(base64.length / 4) * 4, "=");
        const binary = atob(padded);
        const bytes = new Uint8Array(binary.length);
        for (let index = 0; index < binary.length; index += 1) {
          bytes[index] = binary.charCodeAt(index);
        }
        return bytes;
      };

      if (encoded.startsWith("z:")) {
        if (typeof DecompressionStream === "undefined") {
          throw new Error(
            "Compressed shared links require browser DecompressionStream support",
          );
        }

        const compressedBytes = base64UrlToBytes(encoded.slice(2));
        const stream = new Blob([compressedBytes])
          .stream()
          .pipeThrough(new DecompressionStream("gzip"));
        return new TextDecoder().decode(
          new Uint8Array(await new Response(stream).arrayBuffer()),
        );
      }

      if (encoded.startsWith("b:")) {
        return new TextDecoder().decode(base64UrlToBytes(encoded.slice(2)));
      }

      return encoded;
    })()
  `)
  decoded
}

let applyUrlState = (
  encoded,
  compilerVersion,
  moduleSystem: moduleSystem,
  warnFlags,
  jsxPreserveMode,
  experimentalFeatures: array<experimentalFeature>,
) => {
  let moduleSystem = (moduleSystem :> string)
  let experimentalFeatures = experimentalFeatures->Array.map(feature => (feature :> string))
  ignore(encoded)
  ignore(compilerVersion)
  ignore(moduleSystem)
  ignore(warnFlags)
  ignore(jsxPreserveMode)
  ignore(experimentalFeatures)
  let _applied: unit = %raw(`
    (() => {
      const params = new URLSearchParams(window.location.search);
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
      const nextUrl =
        window.location.pathname +
        (query === "" ? "" : "?" + query) +
        window.location.hash;
      window.history.replaceState(null, "", nextUrl);
    })()
  `)
  ignore(_applied)
}

let copyText = value => {
  ignore(value)
  let promise: promise<unit> = %raw(`
    (async () => {
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
    })()
  `)
  promise
}

let initialSource = async defaultSource => {
  switch getParam("code") {
  | None => defaultSource
  | Some(encoded)
    if encoded === "" || encoded->String.length > maxEncodedCodeLength => defaultSource
  | Some(encoded) =>
    try {
      let decoded = await decodeCode(encoded)
      decoded->String.length <= maxDecodedSourceLength ? decoded : defaultSource
    } catch {
    | error =>
      Console.warn2("Could not restore shared playground source", error)
      defaultSource
    }
  }
}

let queryCompilerVersion = defaultVersion =>
  switch getParam("version") {
  | Some(version) if version !== "" => version
  | _ => defaultVersion
  }

let queryModuleSystem = defaultModuleSystem =>
  switch getParam("module") {
  | Some(value) =>
    switch value->parseModuleSystem {
    | Some(moduleSystem) => moduleSystem
    | None => defaultModuleSystem
    }
  | None => defaultModuleSystem
  }

let queryWarnFlags = defaultWarnFlags =>
  switch getParam("warn") {
  | Some(warnFlags) if warnFlags !== "" => warnFlags
  | _ => defaultWarnFlags
  }

let queryJsxPreserveMode = defaultValue =>
  switch getParam("jsxPreserve") {
  | None => defaultValue
  | Some(value) => value === "true" || value === "1"
  }

let queryExperimentalFeatures = () =>
  switch getParam("experimental") {
  | Some(value) if value !== "" =>
    value->String.split(",")->Array.filterMap(parseExperimentalFeature)
  | _ => []
  }

let replaceUrlState = async (
  source,
  compilerVersion,
  moduleSystem,
  warnFlags,
  jsxPreserveMode,
  experimentalFeatures,
) => {
  replaceSequence := replaceSequence.contents + 1
  let sequence = replaceSequence.contents
  let encoded = await encodeCode(source)
  if sequence === replaceSequence.contents {
    applyUrlState(
      encoded,
      compilerVersion,
      moduleSystem,
      warnFlags,
      jsxPreserveMode,
      experimentalFeatures,
    )
  }
}

let windowHref = (): string => %raw(`window.location.href`)

let copyUrlState = async (
  source,
  compilerVersion,
  moduleSystem,
  warnFlags,
  jsxPreserveMode,
  experimentalFeatures,
): result<unit, string> => {
  try {
    replaceSequence := replaceSequence.contents + 1
    let sequence = replaceSequence.contents
    let encoded = await encodeCode(source)
    if sequence === replaceSequence.contents {
      applyUrlState(
        encoded,
        compilerVersion,
        moduleSystem,
        warnFlags,
        jsxPreserveMode,
        experimentalFeatures,
      )

      let href = windowHref()
      let _ = await copyText(href)
      Ok()
    } else {
      Error("Link changed before it could be copied")
    }
  } catch {
  | _ => Error("Could not copy link")
  }
}
