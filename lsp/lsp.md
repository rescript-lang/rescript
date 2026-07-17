> Very experimental, WIP!!

This branch introduces a standalone ReScript LSP server
(`rescript-language-server`) built on top of the existing `analysis` library.
It's a separate, OCaml-side exploration alongside the Rust/rewatch-based
experiment in #8243, the two share the same goal (a LSP server for ReScript) but
approach it from different ends of the toolchain.

## Why rewriting the server in OCaml is a good fit

The editor features are already implemented in the OCaml analysis library.
Hover, completion, references, rename, document symbols, code actions, and
diagnostics all depend on compiler data structures such as `.cmt` / `.cmti`,
typed trees, locations, package metadata, and compiler diagnostics. Running the
LSP server in OCaml lets the server call those APIs directly instead of shelling
out to `rescript-editor-analysis.exe` or passing JSON through stdin. That
removes a large amount of process orchestration and serialization glue, and
keeps the LSP closer to the compiler types it needs to understand. This also
makes the server easier to evolve with the compiler. When typedtree, diagnostic
formats, package discovery, or `.cmt` handling changes, the LSP can be updated
in the same language and build system as the analysis code. That reduces version
skew and makes bugs easier to reproduce with Dune/expect tests.

## What's here

- New `lsp/` package split into `lsp/bin` (entrypoint) and `lsp/src` (library),
  with its own opam file (`rescript-language-server.opam`). Depends on `lsp` (>=
  1.22.0), `eio`/`eio_main`, `ppx_deriving_yojson`, `ppx_expect` and the in-tree
  `analysis` library.

## Status / what's not here yet

> This language server implementation requires ReScript >= v12.1.0.

The main objective is to first maintain feature parity with the current server
(server.ts) as much as possible. Below is a list of some requests and
notifications. Some are out of scope because they don't make sense.

- [x] `initialize` - client request
- [x] `initialized` - client notification
- [x] `shutdown` - client request
- [x] `exit` - client notification
- [x] `textDocument/didOpen` - client notification
- [x] `textDocument/didChange` - client notification
  - The server receives the full text.
- [x] `textDocument/didClose` - client notification
- [ ] `textDocument/didSave` - client notification - **It will not be
      implemented for now.**
- [x] `textDocument/hover` - client request
- [x] `textDocument/publishDiagnostics` - server notification
  - [x] More work was needed for some kinds of errors. How should circular
        dependency errors be published?
    - Circular dependency diagnostics are special-cased because the compiler log
      does not point at a precise source range. When the document is open, we
      expand the diagnostic range to cover the whole document so the editor can
      display a file-level diagnostic. If the document is not open, we keep the
      range parsed from the compiler log, i.e.,
      `{start: {line: 0, character: 0}, end: {line: 0, character: 6}}`
      - The shortest possible code has at least 7 characters: `let a=1`.
        Protocol positions are zero-based, so the position is `length - 1`.
  - [x] Use `Analysis` for syntax errors and ignore them in the compiler log
        parser.
    - This lets the server publish syntax errors on `TextDocumentDidChange` and
      provide instant feedback.
  - [x] Compiler-log diagnostics from `.compiler.log` - server feature
    - [x] Add more test cases; see `compiler_log.ml`. See `tests/build_tests`
          for more examples.
    - [ ] Remove support for parsing OCaml messages?
  - [x] Monorepo diagnostics via `.sourcedirs.json` - server feature
    - Requires ReScript v12.1.0. In this version, `.sourcedirs.json` is always
      generated with `build_root` field for each subpackage.
  - [x] Add the warning number to the diagnostic message.
- [x] `workspace/didChangeWatchedFiles` - client notification
  - The server uses this notification to detect changes to generated
    `.compiler.log` files, which usually means a ReScript build has finished.
    When a watched log changes, the server re-reads every known compiler log and
    republishes diagnostics so stale errors are cleared and monorepo diagnostics
    stay in sync.
  - Use `GlobPattern` with `baseUri` (workspace root)?
  - Should we watch all `rescript.json` files in the workspace?
    - Some features require data defined in `rescript.json`. For example,
      `suffix` and `package-specs` are needed to create the code action
      `Open compiled JS file`. Changes in `dependencies` impact various
      functionalities because they modify the state in
      `Analysis.Shared_types.state`.
    - Users can restart the server after changing the configuration.
- [x] `client/registerCapability` - server request
  - We use this because `workspace/didChangeWatchedFiles` is commonly registered
    dynamically. The server does not know all file-watch patterns during the
    static `initialize` response. In this LSP, the watcher list depends on
    project state, especially `.sourcedirs.json`, which tells us where each
    ReScript build root lives. In monorepos, that means the server needs to
    register watchers for compiler logs after initialization, once it has
    workspace context. That means watching generated `.compiler.log` files. When
    the client sees one change, it sends `workspace/didChangeWatchedFiles`, and
    the server refreshes diagnostics.
- [x] `textDocument/diagnostic` - client request
- [x] `textDocument/completion` - client request
- [x] `completionItem/resolve` - client request
- [x] `textDocument/signatureHelp` - client request
- [x] `textDocument/definition` - client request
- [ ] `textDocument/declaration` - client request - **It will not be implemented
      for now.**
- [x] `textDocument/typeDefinition` - client request
- [ ] `textDocument/implementation` - client request - **It will not be
      implemented for now.**
- [x] `textDocument/references` - client request
  - 🐛 Neovim and Zed kill the server. I need to investigate. Fixed in
    https://github.com/rescript-lang/rescript/pull/8477
- [ ] `textDocument/documentHighlight` - client request - **It will not be
      implemented for now.**
- [x] `textDocument/documentSymbol` - client request
  - Zed needs the `"document_symbols": "on"` configuration. When enabled,
    tree-sitter is not used for document symbols.
- [ ] `workspace/symbol` - client request - **It will not be implemented for
      now.**
- [x] `textDocument/codeAction` - client request
  - [x] Code actions from analysis
  - [x] Code actions from diagnostics (refactor, quick fixes, etc)
    - https://github.com/rescript-lang/rescript-vscode/pull/373
  - [x] Open compiled js file (Trigger workspace execute command
        `rescript/openCompiled`)
    - Available if the client supports `window/showDocument`.
  - [x] Create interface file (Trigger workspace execute command
        `rescript/createInterface`)
    - Available if the interface does not exist.
  - [x] Switch to implementation/interface (Trigger workspace execute command
        `rescript/switchImplementationInterface`)
    - Available if the client supports the `window/showDocument` request.
- [ ] `codeAction/resolve` - client request - **It will not be implemented for
      now.**
- [x] `textDocument/codeLens` - client request
- [x] `workspace/codeLens/refresh` - server request
- [ ] `codeLens/resolve` - client request - **It will not be implemented for
      now.**
- [x] `textDocument/inlayHint` - client request
- [ ] `inlayHint/resolve` - client request - **It will not be implemented for
      now.**
- [x] `textDocument/semanticTokens/full` - client request
- [ ] `textDocument/semanticTokens/full/delta` - client request - **It will not
      be implemented for now.**
- [ ] `textDocument/semanticTokens/range` - client request - **It will not be
      implemented for now.**
- [x] `textDocument/rename` - client request
- [x] `textDocument/prepareRename` - client request
- [x] `textDocument/formatting` - client request
- [ ] `textDocument/rangeFormatting` - client request - **It will not be
      implemented for now.**
- [ ] `textDocument/onTypeFormatting` - client request - **It will not be
      implemented for now.**
- [ ] `textDocument/foldingRange` - client request - **It will not be
      implemented for now.**
- [ ] `textDocument/selectionRange` - client request - **It will not be
      implemented for now.**
- [ ] `textDocument/documentLink` - client request - **It will not be
      implemented for now.**
- [ ] `documentLink/resolve` - client request - **It will not be implemented for
      now.**
- [ ] `textDocument/documentColor` - client request - **It will not be
      implemented for now.**
- [ ] `textDocument/linkedEditingRange` - client request - **It will not be
      implemented for now.**
- [x] `workspace/didChangeConfiguration` - client notification
- [x] `workspace/configuration` - server request
- [x] `workspace/executeCommand` - client request
  - [x] `rescript/dumpServerState`
    - Dump the `State.t` (diagnostics, document store, status, configuration,
      analysis state (`Analysis.Shared_types.state`) and compiler config).
  - [x] `rescript/openCompiled`: Triggered by a code action
    - Only works if the client supports `window/showDocument`.
  - [x] `rescript/createInterface`: Triggered by a code action
    - The server creates the interface if it does not exist and opens the
      interface file if the client supports the `window/showDocument` request.
    - If the interface file already exists, the server sends an error.
  - [x] `rescript/switchImplementationInterface`: Triggered by a code action
    - Only available if the client supports the `window/showDocument` request.
  - [x] `rescript/dumpCmt`: Dump cmt file content
  - [ ] `rescript/dumpParseTree` **It will not be implemented for now.**
    - Dump parsed tree
  - [ ] `rescript/dumpTypedTree` **It will not be implemented for now.**
    - Dump typed tree
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
    - Zed does not support the `window/showDocument` request.
      https://github.com/zed-industries/zed/discussions/58099.
- [x] Custom requests
  - [x] `textDocument/openCompiled` - client request
  - [x] `textDocument/createInterface` - client request
  - [x] `textDocument/switchImplementationInterface`: Currently it's done on the
        client side, but it could be a command triggered by a code action.
    - Move to code action feature
  - [ ] `rescript/startBuild` - client request - **How the build integration
        will be done. See last point**. **It will not be implemented for now**
    - Some questions
      - Create a setting to automatically start the build watcher during
        initialization and drop this custom request?
      - When should the build watcher be stopped? On shutdown/exit?
      - Create work-done progress?
      - Add a custom request to kill the watcher `rescript/stopBuild`?
- [ ] Custom notifications
  - [x] `rescript/compilationFinished` - server notification - Sent from the
        server to the client when compilation is finished
    - The server (`server.ts`) sends this notification, and the VSCode client
      uses it to run Code Analysis. The server sends it when `.compiler.log`
      changes. Only VSCode uses this notification.
    - We can add this notification for compatibility, but in the future,
      reanalyze may be integrated into the server instead of running on the
      client side.
    - We should use progress support `$/progress` server notification?
      - https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workDoneProgress
  - [ ] `rescript/compilationStatus` - server notification - Sends the
        compilation status from the server to the client.
    - VSCode uses this notification to show the compilation status in the status
      bar. Only VSCode uses this notification.
    - We should use progress support `$/progress` server notification?
      - https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workDoneProgress
- [x] Channels to support
  - The current server (`server.ts`) supports stdio and node-ipc. The VSCode
    client uses node-ipc. This implementation supports only stdio.
  - [x] stdio
  - [ ] socket - **It will not be implemented for now**
- [ ] Support package managers: Deno, Bun, Yarn Plug'n'Play, npm, and pnpm
- [ ] Incremental compilation - server feature
- [ ] Build integration managed by the LSP server - server feature - **It will
      not be implemented for now.**
  - Build integration is still external: diagnostics come from compiler logs
    produced by an existing `rescript build`, rather than the LSP starting or
    managing builds itself.
- [ ] First-class compiler support on the server. **It will not be implemented
      for now. Idea to explore.**
  - Instead of running a `rescript watch` process, it's much harder to compile
    in memory?

## Tests

`tests/lsp_tests/` adds a dune-driven integration test (`test.ml`) that boots
the server against a real ReScript workspace (`basic-workspace/`), initializes
the LSP session, sends the `initialized` notification, opens source documents,
and snapshots responses for representative fixtures.

The LSP test target now also runs `dune runtest`, covering inline tests such as
`.sourcedirs.json` build-root, `.compiler.log` parsing used by diagnostics and
file-watcher setup.

## Breaking Changes

- No configuration/settings in `initializationOptions`. This server ignores
  settings coming from `initializationOptions`. See
  https://github.com/microsoft/language-server-protocol/issues/567#issuecomment-448538082.
  We use `workspace/configuration`.
- Signature help is enabled by default.

## Server settings

Proposed interface.

- Currently (server.ts), `supportMarkdownLinks` is not a setting. It's a great feature, but
  some clients don't have good support; Neovim are a example. Therefore,
  I'm promoting it to a setting so users can enable or disable it.

```ts
/**
 * Server settings.
 * These configurations are requested by the server using `workspace/configuration`.
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
   * Enable code lenses for function definitions, showing the full type above each definition
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
     * Maximum number of characters for inlay hints. Set to null to have an unlimited length.
     * Inlay hints that exceed the maximum length will not be shown
     * @default 25
     */
    maxLength?: number | null;
  };
}
```

## Release and transition plan

> How should the server be published? How should developers use the server?

### Some considerations

- It should be a standalone package (`@rescript/language-server` or `@rescript/experimental-language-server`) so
  the user can install it as a development dependency or globally.
- There should be a basic configuration for testing the experimental server,
  such as setting the binary path.
- It shouldn't be bundled with the VSCode extension client as a pre-release
  version. Switching between extension versions is annoying.

### Release Proposal

- Merge the PR into `master` or `lsp` branch?
- Update the CI to publish the language server to npm
  - Trigger CI job by commit message
    (`${{ startsWith(github.event.head_commit.message, 'publish language-server') && (github.ref == 'refs/heads/<BRANCH_NAME>') }}`)
- Users install the language server as a development dependency or globally.
  - The server is just a native binary, so we won't have any dependency
    conflicts.
- Update the VSCode and Zed clients to support the experimental server
  - VSCode: https://github.com/rescript-lang/rescript-vscode/pull/1183
  - Zed: https://github.com/rescript-lang/rescript-zed/pull/24
- Neovim client
  - Neovim users need to make an adjustment to their LSP setup. See
    [Neovim setup](#neovim-setup)

Some points I have questions about.

- Should we ship the server with the compiler?
  - My first impression is no. The compiler release cycle is slower.
  - Another point is that the server should ideally support different versions
    of the compiler. Sending it along with the compiler imposes many
    restrictions.

### Neovim setup

This section describes how Neovim users can configure the experimental server.
The new server requires some changes to the LSP setup. Use the `root_dir`
handler function instead of `root_markers`.

```lua
local use_experimental_server = true

--- You can change the path of binary
--- Search for `node_modules/.bin/rescript-language-server` in the current working directory
--- If you installed it as a development dependency.
local new_rescript_ls_cmd = vim.fs.joinpath(
  vim.uv.cwd(),
  'node_modules',
  '.bin',
  'rescript-language-server'
)
local new_rescript_ls_available = vim.fn.executable(new_rescript_ls_cmd) == 1 and use_experimental_server

local capabilities = vim.lsp.protocol.make_client_capabilities()
-- Enable workspace.didChangeWatchedFiles capabilities
capabilities.workspace.didChangeWatchedFiles.dynamicRegistration = true

---@param client vim.lsp.Client
---@param bufnr integer
local dump_server_state = function(client, bufnr)
  client:exec_cmd({
    title = 'ReScript Dump server state',
    command = 'rescript/dumpServerState',
  }, { bufnr = bufnr }, function(err, result)
    if err then
      vim.notify(tostring(err), vim.log.levels.ERROR)
      return
    end

    if not result or type(result.content) ~= 'string' then
      vim.notify('Invalid server response', vim.log.levels.ERROR)
      return
    end

    local content = result.content

    -- Create a listed scratch buffer.
    local dump_buf = vim.api.nvim_create_buf(true, true)

    vim.api.nvim_buf_set_name(
      dump_buf,
      'rescriptls://rescript-dump-server-state'
    )

    -- Do not specify the file type to avoid freezing with syntax highlighting using Tree-sitter.
    -- The state is a large JSON file.
    vim.bo[dump_buf].buftype = 'nofile'
    vim.bo[dump_buf].bufhidden = 'wipe'
    vim.bo[dump_buf].swapfile = false

    -- Open it in the current window.
    vim.api.nvim_set_current_buf(dump_buf)

    -- Fill buffer with content.
    local lines = vim.split(content, '\n', { plain = true })
    vim.api.nvim_buf_set_lines(dump_buf, 0, -1, false, lines)

    -- Optional: make it read-only after writing.
    vim.bo[dump_buf].modifiable = false
    vim.bo[dump_buf].readonly = true
  end)
end

if new_rescript_ls_available then
  -- Dot notation defines a new configuration instead of extending one with
  -- vim.lsp.config(name, cfg).
  ---@types vim.lsp.Config
  vim.lsp.config.rescriptls = {
    cmd = { new_rescript_ls_cmd },
    filetypes = { 'rescript' },
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
          { 'yarn.lock', 'package-lock.json', 'deno.lock', 'bun.lock', 'pnpm-lock.yaml' },
          { path = fname, upward = true }
        )[1]
      )


      if git_dir and vim.fs.root(git_dir, 'rescript.json') then
        on_dir(git_dir)
      elseif lock_file_dir and vim.fs.root(lock_file_dir, 'rescript.json') then
        on_dir(lock_file_dir)
      end
    end,
    ---@param client vim.lsp.Client
    ---@param bufnr integer
    on_attach = function(client, bufnr)
      vim.api.nvim_buf_create_user_command(
        bufnr,
        'LspDumpServerState',
        function()
          dump_server_state(client, bufnr)
        end,
        { desc = 'rescriptls: Dump server state' }
      )
      on_attach(client, bufnr)
    end,
    capabilities = capabilities,
    settings = {
      rescript = {
        hover = {
          supportMarkdownLinks = false,
        },
        codeLens = false,
        inlayHints = {
          enable = false,
          maxLength = 25,
        },
      },
    },
  }
else
  vim.lsp.config('rescriptls', {
    init_options = {
      extensionConfiguration = {
        askToStartBuild = false,
        codeLens = false,
        signatureHelp = {
          enable = true,
        },
        inlayHints = {
          enable = true,
        },
        incrementalTypechecking = {
          enabled = true,
        },
      },
    },
    on_attach = function(client, bufnr)
      on_attach(client, bufnr)
    end,
  })
end
```

## Other related topics

### Refactor analysis for use on the server side

- Parsing from source (not just files) / decouple I/O from core logic [#8426](https://github.com/rescript-lang/rescript/pull/8426)
  [#8466](https://github.com/rescript-lang/rescript/pull/8466) [#8478](https://github.com/rescript-lang/rescript/pull/8478)
- Use the `yojson` and `lsp` libraries in the analysis library [##8436](https://github.com/rescript-lang/rescript/pull/8436)
- Remove global state `Shared_types.state` [#8465](https://github.com/rescript-lang/rescript/pull/8465)

### Relationship to #8243

#8243 collapses the build watcher and LSP into a single Rust process in rewatch,
shelling out to `rescript-editor-analysis.exe` over stdin. This PR keeps the LSP
on the OCaml side and uses the `analysis` library directly. Useful as a
comparison point for the architecture discussion.
