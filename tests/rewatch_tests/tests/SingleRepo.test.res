open Node
open Node.Test

let repo = Path.resolve(
  Path.dirname(Url.fileURLToPath(Node.importMetaUrl)),
  "../repos/npm/single-project",
)
let commands = Setup.commands(repo)

describe("A single ReScript project using npm as package manager", () => {
  beforeEach(commands.npm.install)

  test("should always pass", async () => {
    Console.log(repo)
    ()
  })

  test("should build", async () => {
    await commands.rescript.build()
  })
})
