> Very experimental, WIP!!

This branch introduces a standalone ReScript LSP server (`rescript-language-server`) built on top of the existing `analysis` library. It's a separate, OCaml-side exploration alongside the Rust/rewatch-based experiment in #8243, the two share the same goal (a LSP server for ReScript) but approach it from different ends of the toolchain.

# Why an OCaml LSP server is a good fit?

The editor features are already implemented in the OCaml analysis library. Hover,
completion, references, rename, document symbols, code actions, and diagnostics
all depend on compiler data structures such as `.cmt` / `.cmti`, typed trees,
locations, package metadata, and compiler diagnostics.

Running the LSP server in OCaml lets the server call those APIs directly instead
of shelling out to `rescript-editor-analysis.exe` or passing JSON through stdin.
That removes a large amount of process orchestration and serialization glue, and
keeps the LSP closer to the compiler types it needs to understand.

This also makes the server easier to evolve with the compiler. When typedtree,
diagnostic formats, package discovery, or `.cmt` handling changes, the LSP can be
updated in the same language and build system as the analysis code. That reduces
version skew and makes bugs easier to reproduce with Dune/expect tests.

## What's here

- New `lsp/` package split into `lsp/bin` (entrypoint) and `lsp/src` (library), with its own opam file (`rescript-language-server.opam`). Depends on `lsp` (>= 1.22.0), `eio`/`eio_main`, `ppx_deriving_yojson`, `ppx_expect` and the in-tree `analysis` library.

## Status / what's not here yet

> This Language server implementation require ReScript >=v12.1.0

The main objective is to first maintain resource parity with the current server (server.ts) as much as possible. Below is a list of some requests and notifications. Some are out of scope because they don't make sense.

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
    - Circular dependency diagnostics are special-cased because the compiler log does not point at a precise source range. When the document is open, we expand the diagnostic range to cover the whole document so the editor can display a file-level diagnostic. If the document is not open, we keep the range parsed from the compiler log, i.e, `{start: {line: 0: character: 0}, end: {line: 0, character: 6}}`
      - The shortest possible code has at least 7 characters: `let a=1`. Protocol position is zero based so, length - 1.
  - [x] Use `Analysis` for syntax errors and ignore them in the compiler log parser.
    - This lets the server publish syntax errors on `TextDocumentDidChange` and provide instant feedback.
  - [x] Compiler-log diagnostics from `.compiler.log` - server feature
    - [x] Add more tests cases, see `compiler_log.ml`. See `tests/build_tests` for more examples.
    - [ ] Remove support to parse OCaml message?
  - [x] Monorepo diagnostics via `.sourcedirs.json` - server feature
    - Require ReScript v12.1.0. In this version `.sourcedirs.json` is always generated with `build_root` field for each subpackage.
  - [x] Add warning number on message diagnostic.
- [x] `workspace/didChangeWatchedFiles` - client notification
  - The server uses this notification to detect changes to generated `.compiler.log` files, which usually means a ReScript build has finished. When a watched log changes, the server re-reads every known compiler log and republishes diagnostics so stale errors are cleared and monorepo diagnostics stay in sync.
  - Use GlobPattern with baseUri (workspace root)?
  - We should watch all `rescript.json` in workspace?
    - Some functionalities require data defined in `rescript.json`. For example, `suffix` and `package-specs` are needed to create the code action `Open compiled js file`. Changes in `dependencies` impact various functionalities because they modify the state in `Analysis.Shared_types.state`.
    - User can restart the server when change some config
- [x] `client/registerCapability` - server request
  - We use this because `workspace/didChangeWatchedFiles` is commonly registered dynamically. The server does not know all file-watch patterns during the static `initialize` response. In this LSP, the watcher list depends on project state, especially `.sourcedirs.json`, which tells us where each ReScript build root lives. In monorepos, that means the server needs to register watchers for compiler logs after initialization, once it has workspace context. That means watching generated `.compiler.log` files. When the client sees one change, it sends `workspace/didChangeWatchedFiles`, and the server refreshes diagnostics.
- [x] `textDocument/diagnostic` - client request
- [x] `textDocument/completion` - client request
- [x] `completionItem/resolve` - client request
- [x] `textDocument/signatureHelp` - client request
- [x] `textDocument/definition` - client request
- [ ] `textDocument/declaration` - client request - **It will not be implemented for now.**
- [x] `textDocument/typeDefinition` - client request
- [ ] `textDocument/implementation` - client request - **It will not be implemented for now.**
- [x] `textDocument/references` - client request
  - 🐛 Neovim and Zed kill the server. I need to investigate. Fixed in https://github.com/rescript-lang/rescript/pull/8477
- [ ] `textDocument/documentHighlight` - client request - **It will not be implemented for now.**
- [x] `textDocument/documentSymbol` - client request
  - Zed need set the config `"document_symbols": "on"`. When enabled, tree-sitter is not used for document symbols.
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
- [x] `workspace/codeLens/refresh` - server request
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
    - Dump the `State.t` (diagnostics, document store, status, configuration, analysis state (`Analysis.Shared_types.state`) and compiler config).
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
  - [ ] `rescript/startBuild` - client request - **How the build integration will be done. See last point**
    - Some questions
      - Create a setting to automatically start the build watcher on initilization and drop this custom request?
      - When kill the build watcher? Shutdown/Exit?
      - Create work done progress?
      - Add a custom request to kill the watcher `rescript/stopBuild`?
- [ ]  Custom notifications
  - [x] `rescript/compilationFinished` - server notification - Send from the server to client when compilation is finished
    - The server (server.ts) send this notification and VSCode client use to run Code Analysis. The server send when the `.compiler.log` changes. Only VSCode use this notification.
    - We can add this notification for compatibility, but in the future, reanalyze may be integrated into the server instead of running on the client side.
    - We should use progress support `$/progress` server notification?
      - https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workDoneProgress
  - [ ] `rescript/compilationStatus` - server notification - Send compilation status from the server to client.
    - VSCode use this notification to show status of compilation on status bar. Only VSCode use this notification.
    - We should use progress support `$/progress` server notification?
      - https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#workDoneProgress
- [x] Channels to support
  - Current server (server.ts) support stdio and node-ipc. The VSCode client use node-ipc. This implementation support only stdio.
  - [x] stdio
  - [ ] socket - **It will not be implemented for now**
- [ ] Support package manager: Deno, Bun, Yarn Plug'n'Play, npm and pnpm
- [ ] Incremental compilation - server feature
- [ ] Build integration managed by the LSP server - server feature - **It will not be implemented for now.**
  - Build integration is still external: diagnostics come from compiler logs produced by an existing `rescript build`, rather than the LSP starting or managing builds itself.
- [ ] First-class compiler support on the server. **It will not be implemented for now. Ideia to explore**
  - Instead of running a `rescript watch` process, it's much harder to compile in memory?

## Tests

`tests/lsp_tests/` adds a dune-driven integration test (`test.ml`) that boots the server against a real ReScript workspace (`basic-workspace/`), initializes the LSP session, sends the `initialized` notification, opens source documents, and snapshots responses for representative fixtures.

The LSP test target now also runs `dune runtest`, covering inline tests such as `.sourcedirs.json` build-root, `.compiler.log` parsing used by diagnostics and file-watcher setup.

## Breaking Changes

- No configuration/setting in `initializationOptions`. This server ignore settings comming from `initializationOptions`. See https://github.com/microsoft/language-server-protocol/issues/567#issuecomment-448538082. We use `workspace/configuration`.

## Server settings

Proposed interface. Some notes:

- Currently, `supportMarkdownLinks` is not a setting. It's a great feature, but some clients don't have good support; Neovim and Zed is an example. Therefore, I'm promoting it to a setting, so VSCode users can enable/disable.
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

> How publish the server? How dev use the server?

### Some considerations

- It should be a standalone package (`@rescript/language-server` or another) so the user can install it as a development dependency or globally.
- It should be a basic configuration to test the experimental server, example, set the binary path.
- It shouldn't be bundled with the VSCode extensison client as a pre-release version. Switching between extension versions is annoying.

### Release Proposal

- Merge the PR into `lsp` branch
- Update the CI to publish the language server to npm package `@rescript/language-server`
  - Starting from `2.0.0-dev-SHA-.0` with tag `dev`. The current version is `1.72.0`
  - Trigger CI job by commit message (`${{ startsWith(github.event.head_commit.message, 'publish language-server') && (github.ref == 'refs/heads/lsp') }}`)
    - Update the `lsp/src/version.ml` and `package.json` version.
- Users install the language server as development dependency or globally.
  - The server is just a native binary, so we won't have any dependency conflicts.
  - Users must update the server; clients will not perform server updates or installations.
- Update clients VSCode and Zed to support the experimental server
  - VSCode: https://github.com/rescript-lang/rescript-vscode/pull/1183
  - Zed: https://github.com/rescript-lang/rescript-zed/pull/24
- Neovim client
  - Users using `mason.nvim` can install the server using `MasonInstall rescript-language-server@alpha --force`
  - Neovim users need to make an adjustment to their LSP setup. See [Neovim setup](#neovim-setup)
- Features/fixes are merged into `lsp` until we get a stable language server. When we have a stable version we merge `lsp` into `master`.

Some points I have doubts about.

- Curently we publish `@rescript/language-server` with tag `next` in vscode-repo. I think it can be confusing? No?
- Should we ship the server with the compiler?
  - My first impression is no. The compiler release cycle is slower.
  - Another point is that the server should ideally support different versions of the compiler. Sending it along with the compiler imposes many restrictions.


### Neovim setup

This section descrive how Neovim user the experimantal server. The new server require some changes on setup of lsp. Use `root_dir` handler function instead of `root_markers`.

```lua
local use_experimental_server = true

--- You can change the path of binary
--- Search for `node_modules/.bin/rescript-language-server` in current working directory
--- If you installed as dev dependecie.
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

    -- Create an listed scratch buffer.
    local dump_buf = vim.api.nvim_create_buf(true, true)

    vim.api.nvim_buf_set_name(
      dump_buf,
      'rescriptls://rescript-dump-server-state'
    )

    -- Do not specify the file type to avoid freezing with syntax highlighting using Treesitter. The state is a large JSON file.
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
        signatureHelp = {
          enable = true,
          forConstructorPayloads = true,
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
      local ok, rescript_tools = pcall(require, 'rescript-tools')
      if ok then
        local commands = {
          ResOpenCompiled = rescript_tools.open_compiled,
          ResCreateInterface = rescript_tools.create_interface,
          ResSwitchImplInt = rescript_tools.switch_impl_intf,
        }
        for name, fn in pairs(commands) do
          vim.api.nvim_buf_create_user_command(
            bufnr,
            name,
            fn,
            { desc = 'ReScript LSP: ' .. name }
          )
        end
      end
      on_attach(client, bufnr)
    end,
  })
end
```

## Other topics related

### Refactor analysis to use on server side

- Parsing from source (not just files) / decouple I/O from core logic #8426 #8466 #8478
- Use `yojson` and `lsp` library for analysis library #8436
- Remove global state `Shared_types.state` #8465

### Relationship to #8243

#8243 collapses the build watcher and LSP into a single Rust process in rewatch, shelling out to `rescript-editor-analysis.exe` over stdin. This PR keeps the LSP on the OCaml side and uses the `analysis` library directly. Useful as a comparison point for the architecture discussion.

## Others TODOs

- Add README.md for `lsp` folder
- Add CHANGELOG.md?

## Some strange behaviors with diagnostics when testing the server

Below I describe the scenarios. I don't know if this is the expected behavior.

1. I have three files: A, B, and C. Only A is open in the editor. I changed the type of a variable in A that B and C use. The compiler reported the errors in files B and C. All good. I edited A to introduce a syntax error but did not save it. The server now reports three errors: the syntax error in A and the two type errors in B and C. Then I saved file A with the syntax error. The server publishes only the syntax diagnostic. The other two are cleared.
  - This happens because, when a syntax error is introduced, `.compiler.log` is changed, and the compiler emits only the syntax error.
  - How can we improve this?

2. Considering the example described above, I now fixed the syntax error in A. The server publishes the two type errors in B and C. I close A, and the server does not clear the diagnostics. In this case, all files are closed. The compiler is running.
  - Should we clear diagnostics when closing a document?

3. I have an open file. I added a syntax error, and the server reports it. All good. I closed the file, and the server clears the syntax error diagnostic. This is the opposite behavior from the case above. The compiler is running.
  - This happens because we ignore syntax errors coming from the compiler.

### Codex suggestion

This is mostly a diagnostic ownership problem. Right now the server mixes two different sources:

1. **Editor syntax diagnostics** from open, unsaved documents.
2. **Compiler diagnostics** from `.compiler.log`.

Those sources have different freshness guarantees, so they should be tracked separately.

**1. Saved Syntax Error Clears B/C Type Errors**
This happens because `.compiler.log` becomes incomplete when the compiler stops at the syntax error. If the server treats that log as a full compiler snapshot, it overwrites previous compiler diagnostics and clears B/C.

I would fix this by making compiler-log updates conditional:

```text
If compiler log contains syntax errors:
  parse syntax diagnostics, but do not replace previous compiler diagnostics.

If compiler log contains non-syntax diagnostics:
  replace previous compiler diagnostics with the new snapshot.

If compiler log is empty / successful:
  clear previous compiler diagnostics.
```

Since you already ignore compiler syntax errors, the important part is: **do not let a syntax-only compiler log clear the previous type-check diagnostics**.

Tradeoff: B/C diagnostics may be stale while A has a syntax error. But that is better than flickering/clearing useful errors. Once syntax is fixed and compiler produces a real type-check log, replace them.

**2. Close A While B/C Diagnostics Remain**
Do not clear compiler diagnostics on document close. Compiler diagnostics are workspace/build diagnostics, not open-document diagnostics.

If B and C have errors from the compiler, those should remain even when no files are open, because the workspace still has build errors.

So the rule should be:

```text
On didClose:
  clear only diagnostics owned by the open-document syntax checker for that URI.
  keep compiler diagnostics unchanged.
```

**3. Closing File Clears Syntax Error**
This behavior is reasonable if the syntax error only exists in the unsaved buffer. Once the document closes, the server no longer owns the in-memory text, so it should remove in-memory syntax diagnostics.

But if the file was saved with the syntax error, the compiler should own that diagnostic. Since you currently ignore compiler syntax diagnostics, saved syntax errors disappear on close.

The fix is to distinguish syntax source:

```text
Open unsaved file:
  use in-memory syntax diagnostics.

Saved file / closed file:
  allow compiler syntax diagnostics to be published.
```

Or more simply:

```text
Ignore compiler syntax diagnostics only for currently open documents.
Keep compiler syntax diagnostics for closed documents.
```

**Suggested Model**
Maintain separate diagnostic maps:

```ocaml
type diagnostics = {
  syntax_by_uri: Diagnostic.t list Uri_map.t;
  compiler_by_uri: Diagnostic.t list Uri_map.t;
}
```

Publishing becomes:

```text
published(uri) = syntax_by_uri(uri) + compiler_by_uri(uri)
```

Then events become predictable:

```text
didChange:
  update syntax_by_uri for that open document
  publish merged diagnostics

compiler log changed:
  if syntax-only/incomplete log:
    update compiler syntax diagnostics for closed files only
    keep previous compiler type diagnostics
  else:
    replace compiler_by_uri with new compiler diagnostics
  publish merged diagnostics

didClose:
  remove syntax_by_uri for that document
  keep compiler_by_uri
  publish merged diagnostics
```

Main practical fix: stop treating every `.compiler.log` change as a complete replacement. Syntax-error logs are incomplete build results, so they need special handling.
