open Node
open Node.Test

let repo = Path.resolve(
  Path.dirname(Url.fileURLToPath(Node.importMetaUrl)),
  "../repos/npm/single-project",
)
let commands = await Setup.commands(repo)

describe("A single ReScript project using npm as package manager", () => {
  before(async () => {
    await commands.npm.install()
  })

  beforeEach(async () => {
    await commands.git.checkout()
  })

  test("should always pass", async () => {
    Console.log(repo)
    ()
  })

  test("should clean", async () => {
    await commands.rescript.clean()
  })

  test("should build", async () => {
    await commands.rescript.build()
  })
})
