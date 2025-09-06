open Node
open Dax

type binPaths = {rescript_exe: string}

let binPaths: binPaths = await Path.resolve(
  Path.dirname(Url.fileURLToPath(Node.importMetaUrl)),
  "../../../cli/common/bins.js",
)->import_

type commands = {
  rescript: {
    build: unit => promise<unit>,
  },
  npm: {
    install: unit => promise<unit>,
  },
}

/// Returns a set of helpers to invoke cli/rescript.js in a working directory
let commands = (workingDirectory: string): commands => {
  // rescript
  let build = async () => {
    let _ = await (sh`${binPaths.rescript_exe} build`)->ShellPromise.cwd(workingDirectory)
  }

  // npm
  let install = async () => {
    let _ = await (sh`npm install`)->ShellPromise.cwd(workingDirectory)
  }

  {
    rescript: {build: build},
    npm: {
      install: install,
    },
  }
}
