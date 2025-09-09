open Node

type processOptions = {cwd: string, stdio?: string}

type processUtils = {
  rescript: (string, array<string>, processOptions) => promise<unit>,
  rewatch: (string, array<string>, processOptions) => promise<unit>,
  execBin: (string, array<string>, processOptions) => promise<unit>,
  shell: (string, array<string>, processOptions) => promise<unit>,
  node: (string, array<string>, processOptions) => promise<unit>,
  git: (array<string>, processOptions) => promise<unit>,
  npm: (array<string>, processOptions) => promise<unit>,
  deno: (array<string>, processOptions) => promise<unit>,
}

// Import process utilities (Windows-safe file URL)
let processUtils: processUtils = {
  let base = Node.importMetaUrl
  let rel = "../../../lib_dev/process.js"
  let fileUrl = Url.make(rel, base)->Url.toString
  await import_(fileUrl)
}

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
  deno: {
    install: unit => promise<unit>,
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
      let _ = await processUtils.rewatch("build", [], {cwd: workingDirectory, stdio: "inherit"})
    }
    let clean = async () => {
      let _ = await processUtils.rewatch("clean", [], {cwd: workingDirectory, stdio: "inherit"})
    }
    {build, clean}
  }

  // npm
  let npm: \"commands.npm" = {
    let install = async () => {
      let _ = await processUtils.npm(["install"], {cwd: workingDirectory, stdio: "inherit"})
    }
    {install: install}
  }

  // deno
  let deno: \"commands.deno" = {
    let install = async () => {
      let _ = await processUtils.deno(["install"], {cwd: workingDirectory, stdio: "inherit"})
    }
    {install: install}
  }

  // git
  let checkout = async () => {
    let _ = await processUtils.git(["checkout", "."], {cwd: workingDirectory, stdio: "inherit"})
  }

  {
    rescript,
    npm,
    deno,
    git: {
      checkout: checkout,
    },
  }
}

let sleep = (ms: int) => {Promise.make((res, _) => setTimeout(res, ms)->ignore)}
