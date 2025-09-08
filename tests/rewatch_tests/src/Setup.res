open Node

type processOptions = {cwd: string}

type processUtils = {
  rescript: (string, array<string>, processOptions) => promise<unit>,
  rewatch: (string, array<string>, processOptions) => promise<unit>,
  execBin: (string, array<string>, processOptions) => promise<unit>,
  shell: (string, array<string>, processOptions) => promise<unit>,
  node: (string, array<string>, processOptions) => promise<unit>,
  git: (array<string>, processOptions) => promise<unit>,
  npm: (array<string>, processOptions) => promise<unit>,
}

// Import process utilities
let processUtils: processUtils = await Path.resolve(
  Path.dirname(Url.fileURLToPath(Node.importMetaUrl)),
  "../../../lib_dev/process.js",
)->import_

type commands = {
  rescript: {
    build: unit => promise<unit>,
    clean: unit => promise<unit>,
  },
  npm: {
    install: unit => promise<unit>,
  },
  git: {
    checkout: unit => promise<unit>,
  },
}

/// Returns a set of helpers to invoke cli/rescript.js in a working directory
let commands = async (workingDirectory: string): commands => {
  // Ensure working directory exists before invoking any process
  let pwdExists = await FsPromises.access(workingDirectory)
  ->Promise.thenResolve(_ => true)
  ->Promise.catch(_ => Promise.resolve(false))

  if !pwdExists {
    throw(Failure("Working directory does not exist: " ++ workingDirectory))
  }

  let rescript = {
    let build = async () => {
      let _ = await processUtils.rewatch("build", [], {cwd: workingDirectory})
    }
    let clean = async () => {
      let _ = await processUtils.rewatch("clean", [], {cwd: workingDirectory})
    }
    {build, clean}
  }

  // npm
  let install = async () => {
    let _ = await processUtils.npm(["install"], {cwd: workingDirectory})
  }

  // git
  let checkout = async () => {
    let _ = await processUtils.git(["checkout", "."], {cwd: workingDirectory})
  }

  {
    rescript,
    npm: {
      install: install,
    },
    git: {
      checkout: checkout,
    },
  }
}

let sleep = (ms: int) => {Promise.make((res, _) => setTimeout(res, ms)->ignore)}
