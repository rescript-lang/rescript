# LSP

# Why rewrite the server in OCaml?

# Some known problems

- https://github.com/rescript-lang/rescript/discussions/8099

# PR Description

> Very experimental, WIP!!

This branch introduces a standalone ReScript LSP server (`rescript-language-server`) built on top of the existing `analysis` library. It's a separate, OCaml-side exploration alongside the Rust/rewatch-based experiment in #8243, the two share the same goal (a LSP server for ReScript) but approach it from different ends of the toolchain.

## What's here

- New `lsp/` package split into `lsp/bin` (entrypoint) and `lsp/src` (library), with its own opam file (`rescript-language-server.opam`). Depends on `lsp` (>= 1.22.0), `eio`/`eio_main`, and the in-tree `analysis` library.

## Status / what's not here yet

> This Language server implementation require ReScript >=v12.1.0

The main objective is to first maintain resource parity with the current server. Below is a list of some requests and notifications. Some are outside the scope because they don't make sense.

- [x] `initialize` - client request
- [x] `initialized` - client notification
- [x] `shutdown` - client request
- [x] `exit` - client notification
- [x] `textDocument/didOpen` - client notification
- [x] `textDocument/didChange` - client notification
  - The server receive the full text
- [x] `textDocument/didClose` - client notification
- [ ] `textDocument/didSave` - client notification - **It will not be implemented for now.**
- [x] `textDocument/hover` - client request
- [x] `textDocument/publishDiagnostics` - server notification
  - [x] Need more work with some kind of erros. How publish circular dependency errors?
    - Circular dependency diagnostics are special-cased because the compiler log does not point at a precise source range. When the document is open, we expand the diagnostic range to cover the whole document so the editor can display a file-level diagnostic. If the document is not open, we keep the range parsed from the compiler log, i.e, `{start: {line: 0: character: 0}, end: {line: 0, character: 0}}`
  - [x] Use `Analysis` for syntax errors and ignore them in the compiler log parser.
    - This lets the server publish syntax errors on `TextDocumentDidChange` and provide instant feedback.
  - [x] Compiler-log diagnostics from `.compiler.log` - server feature
    - [ ] Add more tests cases, see `compiler_log.ml`. See `tests/build_tests` for more examples.
  - [x] Monorepo diagnostics via `.sourcedirs.json` - server feature
    - Require ReScript v12.1.0. In this version `.sourcedirs.json` is always generated with `build_root` field for each subpackage.
  - [ ] Add warning number on message diagnostic.
- [x] `workspace/didChangeWatchedFiles` - client notification
  - The server uses this notification to detect changes to generated `.compiler.log` files, which usually means a ReScript build has finished. When a watched log changes, the server re-reads every known compiler log and republishes diagnostics so stale errors are cleared and monorepo diagnostics stay in sync.
  - Use GlobPattern with baseUri (workspace root)?
  - We should watch all `rescript.json` in workspace?
    - Some functionalities require data defined in `rescript.json`. For example, `suffix` and `package-specs` are needed to create the code action `Open compiled js file`. Changes to `dependencies` impact various functionalities because they modify the state in `Analysis.Shared_types.state`.
    - User can restart the server when change some config
- [x] `client/registerCapability` - server request
  - We use this because `workspace/didChangeWatchedFiles` is commonly registered dynamically. The server does not know all file-watch patterns during the static `initialize` response. In this LSP, the watcher list depends on project state, especially `.sourcedirs.json`, which tells us where each ReScript build root lives. In monorepos, that means the server needs to register watchers for compiler logs after initialization, once it has workspace context. That means watching generated `.compiler.log` files. When the client sees one change, it sends `workspace/didChangeWatchedFiles`, and the server refreshes diagnostics.
- [ ] `textDocument/diagnostic` - client request - **It will not be implemented for now.**
- [x] `textDocument/completion` - client request
- [x] `completionItem/resolve` - client request
- [x] `textDocument/signatureHelp` - client request
- [x] `textDocument/definition` - client request
- [ ] `textDocument/declaration` - client request - **It will not be implemented for now.**
- [x] `textDocument/typeDefinition` - client request
- [ ] `textDocument/implementation` - client request - **It will not be implemented for now.**
- [x] `textDocument/references` - client request
  - 🐛 Neovim and Zed kill the server. I need to investigate.
- [ ] `textDocument/documentHighlight` - client request - **It will not be implemented for now.**
- [ ] `textDocument/documentSymbol` - client request
  - 🐛 Zed doesn't show the symbols on the panel. It works on Neovim
- [ ] `workspace/symbol` - client request - **It will not be implemented for now.**
- [x] `textDocument/codeAction` - client request
  - [x] Code actions from analysis
  - [x] Code actions from diagnostics (refactor, quick fixes, etc)
    - https://github.com/rescript-lang/rescript-vscode/pull/373
  - [x] Open compiled js file (Trigger workspace execute command `rescript/openCompiled`)
    - Available if client support `window/showDocument`
  - [x] Create interface file (Trigger workspace execute command `rescript/createInterface`)
    - Available if interface dont exists
  - [x] Switch to implementation/interface (Trigger workspace execute command `rescript/switchImplementationInterface`)
    - Avaliable if client support `window/showDocument` request
- [ ] `codeAction/resolve` - client request - **It will not be implemented for now.**
- [x] `textDocument/codeLens` - client request
- [ ] `workspace/codeLens/refresh` - server request
  - https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#codeLens_refresh
- [ ] `codeLens/resolve` - client request - **It will not be implemented for now.**
- [x] `textDocument/inlayHint` - client request
- [ ] `inlayHint/resolve` - client request - **It will not be implemented for now.**
- [x] `textDocument/semanticTokens/full` - client request
- [ ] `textDocument/semanticTokens/full/delta` - client request - **It will not be implemented for now.**
- [ ] `textDocument/semanticTokens/range` - client request - **It will not be implemented for now.**
- [x] `textDocument/rename` - client request
- [x] `textDocument/prepareRename` - client request
- [x] `textDocument/formatting` - client request
- [ ] `textDocument/rangeFormatting` - client request - **It will not be implemented for now.**
- [ ] `textDocument/onTypeFormatting` - client request - **It will not be implemented for now.**
- [ ] `textDocument/foldingRange` - client request - **It will not be implemented for now.**
- [ ] `textDocument/selectionRange` - client request - **It will not be implemented for now.**
- [ ] `textDocument/documentLink` - client request - **It will not be implemented for now.**
- [ ] `documentLink/resolve` - client request - **It will not be implemented for now.**
- [ ] `textDocument/documentColor` - client request - **It will not be implemented for now.**
- [ ] `textDocument/linkedEditingRange` - client request - **It will not be implemented for now.**
- [x] `workspace/didChangeConfiguration` - client notification
- [x] `workspace/configuration` - server request
- [x] `workspace/executeCommand` - client request
  - [x] `rescript/dumpServerState`
    - Dump the `State.t` (diagnostics, document store, status, analysis state (`Analysis.Shared_types.state`) and compiler config).
  - [x] `rescript/openCompiled`: Trigged by a code action
    - Only work if client support `window/showDocument`
  - [x] `rescript/createInterface`: Trigged by a code action
    - Server create interface if dont exists and open the interface file if client support `window/showDocument` request.
    - If interface file already exists server send a error
  - [x] `rescript/switchImplementationInterface`: Trigged by a code action
    - Only avaliable if client support `window/showDocument` request
  - Flow to execute a command:
    - ```
      CodeAction.command = "rescript/commandName"
                ↓
      workspace/executeCommand handler
                ↓
      server sends window/showDocument
                ↓
      client opens file
      ```
    - Zed does not support the `window/showDocument` request. https://github.com/zed-industries/zed/discussions/58099.
- [x] Custom requests
  - [x] `textDocument/openCompiled` - client request
  - [x] `textDocument/createInterface` - client request
  - [x] `textDocument/switchImplementationInterface`: Currently it's done on the client side, but it could be a command trigged by a code action.
    - Move to code action feature
  - [ ] `rescript/startBuild` - client request - **How the build integration will be done**
    - Some questions
      - Build should be managed by the server?
      - Create a setting to automatically start the build watcher on initilization and drop this custom request?
      - When kill the build watcher? Shutdown/Exit?
      - Create work done progress?
      - Add a custom request to kill the watcher `rescript/stopBuild`?
- [ ]  Custom notifications
  - [ ] `rescript/compilationFinished` - server notification - Send from the server to client when compilation is finished
    - The server (server.ts) send this notification and VSCode client use to run Code Analysis. The server send when the `.compiler` log changes. Only VSCode use this notification.
    - We should use progress support `$/progress` server notification?
  - [ ] `rescript/compilationStatus` - server notification - Send compilation status from the server to client.
    - VSCode use this notification to show status of compilation on status bar. Only VSCode use this notification.
    - We should use progress support `$/progress` server notification?
- [ ] Incremental compilation - server feature
- [ ] Build integration managed by the LSP server - server feature
  - Build integration is still external: diagnostics come from compiler logs produced by an existing `rescript build`, rather than the LSP starting or managing builds itself.
- [ ] Support two channels: `stdio` and `socket`?

## Tests

`tests/lsp_tests/` adds a dune-driven integration test (`test.ml`) that boots the server against a real ReScript workspace (`basic-workspace/`), initializes the LSP session, sends the `initialized` notification, opens source documents, and snapshots responses for representative fixtures.

The LSP test target now also runs `dune runtest`, covering inline tests such as `.sourcedirs.json` build-root, `.compiler.log` parsing used by diagnostics and file-watcher setup.

## Server settings

Proposed interface. Some notes:

- Currently, `supportMarkdownLinks` is not a setting. It's a great feature, but some clients don't have good support; Neovim is an example. Therefore, I'm promoting it to a setting. This feature is great in VSCode and Zed.
- I think we should remove `signatureHelp.enable`. It's a basic feature on many servers.

```ts
/**
 * Server settings.
 * These configurations are request by the server using `workspace/configuration`
 */
interface Settings {
  hover?: {
    /** 
     * Enable markdown links in hover responses
     * @default false
     */
    supportMarkdownLinks?: boolean;
  };
  /**
   * Enable code lenses to function definitions, showing its full type above the definition
   * @default false
   */
  codeLens?: boolean;
  /**
   * Place annotations inline with text to display type hints
   */
  inlayHints?: {
    /**
     * Enable inlay hints
     * @default false
     */
    enable?: boolean;
    /**
     * Maximum length of character for inlay hints. Set to null to have an unlimited length.
     * Inlay hints that exceed the maximum length will not be shown
     * @default 25
     */
    maxLength?: number | null;
  };
  /**
   * Signature help
   */
  signatureHelp?: {
    /**
     * Enable signature help
     * @default true
     */
    enable?: boolean;
    /**
     * Enable signature help for variant constructors
     * @default true
     */
    forConstructorPayloads?: boolean;
  };
}
```

## Release and transition plan

> TODO: How publish the server? How dev use the new alpha version? Describe a transition plan.

Some observations for testing the alpha version.

#### VSCode/Zed users

- It should be a standalone package (`@rescript/language-server`?) so the user can install it as a development dependency or globally. VSCode/Zed can get it from `node_modules/.bin/rescript-language-server` or from the `$PATH`.
- It should be a basic configuration, for example `rescript.useExperimentalServer: bool` in VSCode and `rescript-language-server.useExperimentalServer: bool` under `lsp` section on Zed.
  - Clients can download and install the language server from npm?
- It shouldn't be bundled with the VSCode extensison client as a pre-release version. Switching between extension versions is annoying.

#### Neovim users

The new server require some changes on setup of lsp. Use `root_dir` handler function instead of `root_markers`.
  
```lua
--- Search for `node_modules/.bin/rescript-language-server` in current working directory
local new_rescript_ls_cmd = vim.fs.joinpath(
  vim.uv.cwd(),
  'node_modules',
  '.bin',
  'rescript-language-server'
)
local new_rescript_ls_installed = vim.fn.executable(new_rescript_ls_cmd) == 1

local capabilities = vim.lsp.protocol.make_client_capabilities()
-- Enable workspace.didChangeWatchedFiles capabilities
capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = true

if new_rescript_ls_installed then
  -- Dot notation defines a new configuration instead of extending one with
  -- vim.lsp.config(name, cfg).
  vim.lsp.config.rescriptls = {
    cmd = { new_rescript_ls_cmd },
    filestypes = { 'rescript' },
    -- Prefer root_dir over root_markers for monorepos. root_markers stops at
    -- the nearest rescript.json, which may be a package inside the monorepo.
    -- The ReScript LSP needs the workspace root instead, because package
    -- discovery, lock files, and build roots are resolved from the directory
    -- where Neovim was opened.
    -- This root_dir callback first finds the repository root, then falls back to
    -- the package-manager lockfile root. It only starts the server when that
    -- root contains a ReScript project.
    root_dir = function(bufnr, on_dir)
      local fname = vim.api.nvim_buf_get_name(bufnr)
      -- Find the repository root.
      local git_dir =
        vim.fs.dirname(vim.fs.find('.git', { path = fname, upward = true })[1])
      -- Monorepos usually keep one lock file at the workspace root.
      local lock_file_dir = vim.fs.dirname(
        vim.fs.find(
          { 'yarn.lock', 'package-lock.json' },
          { path = fname, upward = true }
        )[1]
      )

      if
        (git_dir and vim.fs.root(git_dir, 'rescript.json'))
        or (lock_file_dir and vim.fs.root(lock_file_dir, 'rescript.json'))
      then
        on_dir(git_dir or lock_file_dir)
      end
    end,
    on_attach = on_attach,
    capabilities = capabilities,
    settings = {
      rescript = {
        hover = {
          supportMarkdownLinks = true,
        },
        codeLens = true,
        inlayHints = {
          enable = true,
          maxLength = 25,
        },
        signatureHelp = {
          enable = true,
          forConstructorPayloads = true,
        },
      },
    },
  }
else
  vim.lsp.config('rescriptls', {on_attach = on_attach, capabilities = capabilities})
end
```

## Other topics related

## Refactor analysis to use on server side

- Parsing from source (not just files) / decouple I/O from core logic #8426 #8466
- Use `yojson` and `lsp` library for analysis library #8436
- Remove global state `Shared_types.state` #8465

### Relationship to #8243

#8243 collapses the build watcher and LSP into a single Rust process in rewatch, shelling out to `rescript-editor-analysis.exe` over stdin. This PR keeps the LSP on the OCaml side and uses the `analysis` library directly. Useful as a comparison point for the architecture discussion.

## Other TODO

- Add README.md for `lsp` folder
- Add CHANGELOG.md?
- Document custom requests/notification
- Configure CI to publish the server on npm
  - Trigger event: push commit message format on master branch?
