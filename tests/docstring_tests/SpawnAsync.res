open Node

type t = {
  stdout: array<Buffer.t>,
  stderr: array<Buffer.t>,
  code: Null.t<float>,
}
let run = async (~command, ~args, ~options=?) => {
  let spawn = ChildProcess.spawn(command, args, ~options?)
  let stdout = []
  let stderr = []
  spawn.stdout->ChildProcess.on("data", data => {
    Array.push(stdout, data)
  })
  spawn.stderr->ChildProcess.on("data", data => {
    Array.push(stderr, data)
  })
  await Promise.make((resolve, reject) => {
    spawn->ChildProcess.once("error", (_, _) => {
      reject({stdout, stderr, code: Null.make(1.0)})
    })
    spawn->ChildProcess.once("close", (code, _signal) => {
      resolve({stdout, stderr, code})
    })
  })
}
