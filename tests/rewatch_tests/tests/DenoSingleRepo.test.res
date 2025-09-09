open Node
open Node.Test

let repo = Path.resolve(
  Path.dirname(Url.fileURLToPath(Node.importMetaUrl)),
  "../repos/deno/single-project",
)
let commands = await Setup.commands(repo, ~runtime=Setup.Deno)

describe("A single ReScript project using deno as package manager", () => {
  before(async () => {
    await commands.deno.install()
  })

  beforeEach(async () => {
    await commands.git.checkout()
  })

  test("should clean", async () => {
    await commands.rescript.clean()
  })

  test("should build", async () => {
    await commands.rescript.build()
  })
})
