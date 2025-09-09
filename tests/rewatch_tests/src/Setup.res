open Node

type processOptions = {cwd: string, stdio?: string}

@module("../../../lib_dev/process.js")
external deno: (array<string>, processOptions) => promise<unit> = "deno"
@module("../../../lib_dev/process.js")
external node: (string, array<string>, processOptions) => promise<unit> = "node"
@module("../../../lib_dev/process.js")
external git: (array<string>, processOptions) => promise<unit> = "git"
@module("../../../lib_dev/process.js")
external npm: (array<string>, processOptions) => promise<unit> = "npm"

let rescriptJs = Path.resolve(Node.importMetaDirname, "../../../cli/rescript.js")

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

type runtime =
  | @as("deno") Deno
  | @as("node") Node
  | @as("bun") Bun

/// Returns a set of helpers to invoke cli/rescript.js in a working directory
let commands = async (workingDirectory: string, ~runtime: runtime=Node): commands => {
  // Ensure working directory exists before invoking any process
  let pwdExists = await FsPromises.access(workingDirectory)
  ->Promise.thenResolve(_ => true)
  ->Promise.catch(_ => Promise.resolve(false))

  if !pwdExists {
    throw(Failure("Working directory does not exist: " ++ workingDirectory))
  }

  let rescript = {
    let build = async () => {
      switch runtime {
      | Deno => {
          let _ = await deno(
            ["--allow-read", "--allow-env", "--allow-run", rescriptJs],
            {cwd: workingDirectory, stdio: "inherit"},
          )
        }
      | Node => {
          let _ = await node(rescriptJs, ["build"], {cwd: workingDirectory, stdio: "inherit"})
        }

      | Bun => throw(Failure("Bun is not supported yet"))
      }
    }
    let clean = async () => {
      switch runtime {
      | Deno => {
          let _ = await deno(
            ["--allow-read", "--allow-env", "--allow-run", rescriptJs, "clean"],
            {cwd: workingDirectory, stdio: "inherit"},
          )
        }
      | Node => {
          let _ = await node(rescriptJs, ["clean"], {cwd: workingDirectory, stdio: "inherit"})
        }
      | Bun => throw(Failure("Bun is not supported yet"))
      }
    }
    {build, clean}
  }

  // npm
  let npm: \"commands.npm" = {
    let install = async () => {
      let _ = await npm(["install"], {cwd: workingDirectory, stdio: "inherit"})
    }
    {install: install}
  }

  // deno
  let deno: \"commands.deno" = {
    let install = async () => {
      let _ = await deno(["install"], {cwd: workingDirectory, stdio: "inherit"})
    }
    {install: install}
  }

  // git
  let checkout = async () => {
    let _ = await git(["checkout", "."], {cwd: workingDirectory, stdio: "inherit"})
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
