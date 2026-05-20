type info = {
  bundleId: string,
  version: string,
  apiVersion: string,
  moduleSystem: string,
  warnFlags: string,
  jsxPreserveMode: bool,
  experimentalFeatures: array<string>,
  libraries: array<string>,
}

type config = {
  compilerVersion: string,
  moduleSystem: string,
  warnFlags: string,
  jsxPreserveMode: bool,
  experimentalFeatures: array<string>,
}

type compilerVersion = {
  id: string,
  label: string,
}

type result = {
  ok: bool,
  kind: string,
  jsCode: string,
  parsetree: string,
  typedtree: string,
  @as("lambda")
  lambda_: string,
  lam: string,
  errors: array<string>,
  warnings: array<string>,
  message: string,
  time: float,
}

@module("./CompilerRuntime.js") external defaultWarnFlags: string = "defaultWarnFlags"
@module("./CompilerRuntime.js") external defaultCompilerVersion: string = "defaultCompilerVersion"
@module("./CompilerRuntime.js")
external availableCompilerVersions: array<compilerVersion> = "availableCompilerVersions"
@module("./CompilerRuntime.js") external init: string => promise<info> = "init"
@module("./CompilerRuntime.js") external compile: (string, config) => promise<result> = "compile"
