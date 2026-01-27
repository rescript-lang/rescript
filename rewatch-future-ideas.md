# Rewatch Future: The Tower of Babylon

## Executive Summary

This document describes evolving rewatch from a build tool into the **central nervous system** of ReScript development - a gRPC daemon that unifies all tooling around one source of truth.

**Core Insight**: The daemon exposes a Protobuf API. Adapter layers (LSP for editors, MCP for AI agents) translate their protocols to gRPC. Build tools (Vite, Bun) and CLI tools connect directly via generated gRPC clients.

**Key Benefits**:
- Solves "can't build while watching" limitation
- Eliminates duplicate state across tools
- Enables seamless Vite/Bun integration via generated clients
- Type-safe contracts via Protobuf
- Native streaming for watch mode and progress reporting

## Current Problems

### Problem 1: Fragmented State

Today we have multiple processes maintaining overlapping state:

```
┌─────────────────────┐       ┌─────────────────────┐
│   rewatch (Rust)    │       │  analysis (OCaml)   │
│                     │       │                     │
│  BuildState:        │       │  State:             │
│  - modules          │       │  - packagesByRoot   │
│  - dependencies     │       │  - cmtCache         │
│  - dependents       │       │  - fileReferences   │
│  - parse state      │       │  - locItems         │
└─────────────────────┘       └─────────────────────┘
         ↓                              ↓
    ┌────────────────────────────────────┐
    │    Filesystem (.cmt/.cmi files)    │
    └────────────────────────────────────┘
```

Both processes read the same `.cmt/.cmi` files independently, with no coordination.

### Problem 2: Can't Build While Watching

The lock mechanism (`lib/bs/rescript.lock`) prevents concurrent builds:

```bash
# Terminal 1
$ rescript watch
[Watching...]

# Terminal 2
$ rescript build
Error: A ReScript build is already running (PID 12345)
```

Watch mode holds the lock, blocking all other build commands.

### Problem 3: Tool Integration is Indirect

Current Vite integration:
```
Vite → reads .js files → serves to browser
         ↑
         └── ReScript watch mode writes .js files
```

No direct communication. Changes propagate only through the filesystem.

## The Solution: gRPC Daemon

```
                  ┌────────────────────────────────────┐
                  │      Rewatch Daemon (Rust)         │
                  │           (gRPC Server)            │
                  │                                    │
                  │  ┌──────────────────────────────┐  │
                  │  │  Unified State               │  │
                  │  │  - BuildState (modules)      │  │
                  │  │  - Dependency graph          │  │
                  │  │  - CMT cache                 │  │
                  │  └──────────────────────────────┘  │
                  └────────────────────────────────────┘
                     ↑        ↑         ↑         ↑
                     │ gRPC   │ gRPC    │ gRPC    │ gRPC
           ┌─────────┘        │         │         └─────────┐
           │                  │         │                   │
    ┌──────┴──────┐  ┌────────┴───┐ ┌──┴──────┐  ┌─────────┴────────┐
    │ LSP Adapter │  │   Vite     │ │   MCP   │  │   DevTools UI    │
    │  (speaks    │  │  (native   │ │ Adapter │  │   (gRPC client)  │
    │   LSP to    │  │   gRPC)    │ │         │  │                  │
    │  VS Code)   │  │            │ │         │  │                  │
    └─────────────┘  └────────────┘ └─────────┘  └──────────────────┘
           │
    ┌──────┴──────┐
    │  rescript   │
    │  CLI        │
    │(gRPC client)│
    └─────────────┘
```

**Benefits:**
- Single source of truth - one daemon owns the state
- Type-safe contracts - Protobuf schema defines the API
- Generated clients - Rust, TypeScript, Python, etc.
- Native streaming - perfect for watch mode
- No "build while watching" problem - all commands are gRPC clients

## File Watching Architecture

**File watching is a client responsibility, not a daemon responsibility.**

Different clients have different mechanisms for detecting file changes, and the daemon should not duplicate this work:

1. **CLI Watch Command** (`rescript watch`):
   - Uses its own file watcher (notify-rs or similar)
   - When files change, sends `CompileFile` or `Build` requests to daemon
   - Displays build events via streaming `Watch` RPC

2. **LSP Client** (VS Code extension):
   - Editor already tracks file changes via LSP `didChange` notifications
   - When user saves a file, LSP adapter sends `CompileFile` to daemon
   - No separate file watcher needed

3. **Vite Plugin**:
   - Vite's own HMR system watches files
   - When `.res` file changes, plugin sends `CompileFile` to daemon
   - Returns compiled JS for HMR update

4. **Build Tool Plugins** (Bun, Astro, etc.):
   - Each build tool has its own file watching mechanism
   - Plugin translates tool's change notifications to daemon requests

**Daemon's Role:**
- **Does NOT watch files** - relies on clients to report changes
- **Does maintain state** - knows which modules need recompilation based on dependency graph
- **Does broadcast events** - when a compilation completes, all watching clients get notified via streaming RPCs

This design avoids duplicate file watchers, respects each tool's native file watching, and keeps the daemon focused on build orchestration rather than I/O polling.

### Client Coordination to Avoid Duplicate Builds

While multiple clients may watch files independently, the LSP adapter intelligently defers to "full watcher" clients (CLI watch, Vite) to avoid triggering duplicate builds:

**LSP Adapter Logic:**
```rust
impl LspAdapter {
    async fn handle_did_save(&self, params: DidSaveParams) -> Result<()> {
        // Query daemon: are there any "full watcher" clients connected?
        let status = self.daemon.get_status().await?;
        
        if status.has_full_watcher {
            // CLI watch or Vite is running - they'll trigger the build
            // LSP just waits for the BuildComplete event to update diagnostics
            log::debug!("Full watcher active, skipping LSP-triggered build");
            return Ok(());
        }
        
        // No other watcher - LSP must trigger the build
        self.daemon.compile_file(CompileFileRequest {
            path: params.text_document.uri.path().to_string(),
            ..Default::default()
        }).await?;
        
        Ok(())
    }
}
```

**Daemon Tracks Client Capabilities:**
```rust
struct DaemonState {
    connected_clients: HashMap<ClientId, ClientInfo>,
}

struct ClientInfo {
    client_type: ClientType,
    watch_scope: WatchScope,
}

enum WatchScope {
    None,           // Just subscribing to events
    OpenFiles,      // LSP: only watching files open in editor
    AllSources,     // CLI watch, Vite: watching all project files
}
```

**Timeout Safety:**
If the LSP detects a file save but no build event arrives within a reasonable timeout (e.g., 2-3 seconds), it logs a warning that the full watcher client may not be functioning correctly and can optionally trigger a fallback build. This ensures builds always happen even if a watcher client becomes unresponsive.

**Example Flow (VS Code + Vite both running):**
1. User saves `App.res` in editor
2. LSP's `didSave` fires → queries daemon → sees Vite is connected with `WatchScope::AllSources`
3. LSP: "Vite will handle this, waiting for build event..." (starts timeout)
4. Vite detects change via its watcher → sends `CompileFile(App.res)` to daemon
5. Build happens, both LSP and Vite receive `BuildComplete` event (LSP timeout cancelled)
6. LSP updates diagnostics, Vite triggers HMR

This approach eliminates duplicate builds through smart coordination at the LSP adapter level, while maintaining safety through timeout-based fallbacks.

## Client Types

### 1. Editor (VS Code via LSP Adapter)
The LSP adapter is part of the rewatch binary. It speaks LSP to VS Code and translates to internal gRPC calls.

**Current:** Separate `rescript-editor-analysis` binary spawned per request  
**Future:** LSP adapter in rewatch daemon, analysis as long-running subprocess

### 2. Build Tools (Vite, Bun, Astro)
Native gRPC clients generated from Protobuf.

**Current:** `vite-plugin-rescript` starts separate rewatch process  
**Future:** Generated TypeScript gRPC client connects to daemon

### 3. AI Agents (MCP Adapter)
MCP adapter translates Model Context Protocol to gRPC calls.

**Current:** Would need to spawn own process  
**Future:** Thin adapter layer (~200 lines)

### 4. CLI Tools
The CLI becomes a gRPC client to the daemon.

**Current:** Each invocation spawns new process, initializes state  
**Future:** Connect to running daemon (or start one)

### 5. DevTools UI
Web UI or TUI for inspecting build state, module graph, diagnostics.

**Current:** Doesn't exist  
**Future:** gRPC-web client for browser, or native client for TUI

## Protobuf Schema

```protobuf
syntax = "proto3";
package rescript.daemon;

service RescriptDaemon {
  // Build operations
  rpc Build(BuildRequest) returns (BuildResult);
  rpc Clean(CleanRequest) returns (CleanResult);
  rpc Watch(WatchRequest) returns (stream BuildEvent);
  
  // File operations
  rpc CompileFile(CompileFileRequest) returns (CompileFileResult);
  rpc Format(FormatRequest) returns (FormatResult);
  
  // Tools (currently in rescript-tools)
  rpc GenerateDocs(GenerateDocsRequest) returns (GenerateDocsResult);
  rpc FormatCodeblocks(FormatCodeblocksRequest) returns (FormatCodeblocksResult);
  rpc ExtractCodeblocks(ExtractCodeblocksRequest) returns (ExtractCodeblocksResult);
  
  // Dependency queries
  rpc GetDependents(DependentsRequest) returns (DependentsResult);
  rpc GetDependencies(DependenciesRequest) returns (DependenciesResult);
  
  // State inspection
  rpc GetBuildStatus(BuildStatusRequest) returns (BuildStatus);
  rpc GetModuleGraph(ModuleGraphRequest) returns (ModuleGraph);
  rpc ListModules(ListModulesRequest) returns (ListModulesResult);
  
  // Analysis (proxied to OCaml subprocess)
  rpc Hover(HoverRequest) returns (HoverResult);
  rpc Completion(CompletionRequest) returns (CompletionResult);
  rpc Definition(DefinitionRequest) returns (DefinitionResult);
  rpc References(ReferencesRequest) returns (ReferencesResult);
  
  // Lifecycle
  rpc Ping(PingRequest) returns (PingResult);
  rpc Shutdown(ShutdownRequest) returns (ShutdownResult);
}

message BuildRequest {
  optional string filter = 1;
  optional bool clean = 2;
}

message BuildResult {
  bool success = 1;
  int32 module_count = 2;
  int32 error_count = 3;
  int32 warning_count = 4;
  double duration_seconds = 5;
  repeated Diagnostic diagnostics = 6;
}

message WatchRequest {
  optional string filter = 1;
}

message BuildEvent {
  oneof event {
    BuildProgress progress = 1;
    BuildComplete complete = 2;
    FileChanged file_changed = 3;
  }
}

message BuildProgress {
  enum Stage { PARSING = 0; DEPENDENCIES = 1; COMPILING = 2; }
  Stage stage = 1;
  int32 current = 2;
  int32 total = 3;
}

message CompileFileRequest {
  string path = 1;
  bool return_in_memory = 2;
}

message CompileFileResult {
  bool success = 1;
  optional string js = 2;
  optional string source_map = 3;
  repeated string dependencies = 4;
  repeated Diagnostic diagnostics = 5;
}

message Diagnostic {
  string path = 1;
  int32 line = 2;
  int32 column = 3;
  string message = 4;
  bool is_error = 5;
}

// ... remaining message types follow similar patterns
```

From this schema, clients are automatically generated for Rust, TypeScript, Python, Go, etc.

## CLI as gRPC Client

```rust
pub async fn run_build(args: BuildArgs) -> Result<()> {
    let mut client = connect_or_start_daemon().await?;
    
    let response = client.build(BuildRequest {
        filter: args.filter,
        clean: Some(args.clean),
    }).await?;
    
    let result = response.into_inner();
    if result.success {
        println!("✅ Compiled {} modules in {:.2}s", 
            result.module_count, result.duration_seconds);
    } else {
        for d in &result.diagnostics {
            println!("{}:{}:{}: {}", d.path, d.line, d.column, d.message);
        }
    }
    Ok(())
}

pub async fn run_watch(args: WatchArgs) -> Result<()> {
    let mut client = connect_or_start_daemon().await?;
    
    // Subscribe to build events from daemon
    let mut event_stream = client.subscribe_events(SubscribeEventsRequest {}).await?.into_inner();
    
    // Set up file watcher in the CLI client
    let (tx, mut rx) = mpsc::channel(100);
    let watcher = notify::recommended_watcher(move |res: Result<notify::Event, _>| {
        if let Ok(event) = res {
            let _ = tx.blocking_send(event);
        }
    })?;
    
    // Watch the project directory
    watcher.watch(&project_root, RecursiveMode::Recursive)?;
    
    // Initial build
    client.build(BuildRequest { filter: args.filter, clean: None }).await?;
    
    loop {
        tokio::select! {
            // File changed - trigger rebuild
            Some(fs_event) = rx.recv() => {
                if is_rescript_file(&fs_event.paths) {
                    println!("📝 File changed, rebuilding...");
                    client.build(BuildRequest { filter: args.filter, clean: None }).await?;
                }
            }
            
            // Build event from daemon - display progress
            Some(event) = event_stream.message() => {
                match event?.event {
                    Some(Event::Progress(p)) => print!("\rCompiling [{}/{}]", p.current, p.total),
                    Some(Event::Complete(c)) => println!("\r✅ Built in {:.2}s", c.duration_seconds),
                    None => {}
                }
            }
        }
    }
}
```

With this architecture:
```bash
# Terminal 1: Watch mode
$ rescript watch
✅ Watching for changes...

# Terminal 2: One-shot build (no conflict!)
$ rescript build
✅ Compiled in 0.1s  # Fast - state already warm

# Terminal 3: Format
$ rescript format src/MyFile.res
✅ Formatted

# Terminal 4: Generate docs
$ rescript doc src/MyLib.res
✅ Documentation generated (JSON output)
```

## Incorporating rescript-tools Commands

The commands currently in `rescript-tools` (`doc`, `format-codeblocks`, `extract-codeblocks`) will be incorporated into the main rewatch CLI, backed by the daemon.

**Current problem with `rescript-tools doc`:**
The doc command reads `.cmt` files to extract documentation. If the source file was modified but not recompiled, the `.cmt` is stale and the docs are wrong.

**With daemon integration:**
The daemon knows if a file needs recompilation. When you run `rescript doc src/MyLib.res`:
1. CLI sends `GenerateDocs` request to daemon
2. Daemon checks if `MyLib.res` (or its dependencies) need recompilation
3. If stale, daemon recompiles first
4. Then generates docs from fresh `.cmt` files
5. Returns documentation JSON

```rust
pub async fn run_doc(args: DocArgs) -> Result<()> {
    let mut client = connect_or_start_daemon().await?;
    
    let response = client.generate_docs(GenerateDocsRequest {
        entry_point_file: args.path,
    }).await?;
    
    let result = response.into_inner();
    if result.success {
        println!("{}", result.docs_json);
    } else {
        for err in &result.errors {
            eprintln!("{}", err);
        }
    }
    Ok(())
}
```

This applies to all tools that depend on compiled artifacts - the daemon ensures freshness automatically.

## LSP Adapter (in Rust)

The LSP adapter runs as part of the rewatch binary:

```rust
pub struct LspAdapter {
    daemon: RescriptDaemonClient,
}

impl LspAdapter {
    // Translate LSP hover → gRPC Hover
    async fn handle_hover(&self, params: HoverParams) -> Option<Hover> {
        let result = self.daemon.hover(HoverRequest {
            path: params.text_document.uri.path().to_string(),
            line: params.position.line as i32,
            character: params.position.character as i32,
        }).await.ok()?;
        
        result.into_inner().contents.map(|c| Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: c,
            }),
            range: None,
        })
    }
    
    // Similar translations for completion, definition, references, etc.
}
```

The adapter is ~500 lines of protocol translation. All logic stays in the daemon.

## Daemon Architecture

```rust
pub struct RescriptDaemonService {
    build_state: Arc<RwLock<BuildCommandState>>,
    analysis_process: Arc<RwLock<AnalysisProcess>>,
    event_tx: broadcast::Sender<BuildEvent>,
}

#[tonic::async_trait]
impl RescriptDaemon for RescriptDaemonService {
    async fn build(&self, request: Request<BuildRequest>) -> Result<Response<BuildResult>, Status> {
        let mut state = self.build_state.write().await;
        let result = build::incremental_build(&mut state, ...)?;
        
        // Broadcast to all watching clients
        let _ = self.event_tx.send(BuildEvent::complete(result.clone()));
        
        Ok(Response::new(result.into()))
    }
    
    type WatchStream = ReceiverStream<Result<BuildEvent, Status>>;
    
    async fn watch(&self, _request: Request<WatchRequest>) -> Result<Response<Self::WatchStream>, Status> {
        let mut rx = self.event_tx.subscribe();
        let (tx, rx_stream) = mpsc::channel(100);
        
        tokio::spawn(async move {
            while let Ok(event) = rx.recv().await {
                if tx.send(Ok(event)).await.is_err() { break; }
            }
        });
        
        Ok(Response::new(ReceiverStream::new(rx_stream)))
    }
}
```

### OCaml Analysis Integration

The existing `rescript-editor-analysis` binary is invoked as a one-shot process for each request (hover, completion, definition, etc.), exactly like the current VSCode LSP server does:

```rust
impl RescriptDaemonService {
    async fn hover(&self, path: &str, line: i32, col: i32) -> Result<Option<String>> {
        let output = Command::new("rescript-editor-analysis")
            .args(["hover", path, &line.to_string(), &col.to_string()])
            .output()?;
        
        let result: serde_json::Value = serde_json::from_slice(&output.stdout)?;
        Ok(result.get("contents").and_then(|v| v.as_str()).map(String::from))
    }
}
```

This mirrors the current approach in the VSCode extension where each analysis request spawns a new process. Future optimizations (long-running subprocess, embedding OCaml in Rust) can be explored later.

### Process Lifecycle

- **One daemon per project** - PID file in `lib/bs/.rescript-daemon.pid`
- **Auto-start on first client** - `rescript build` starts daemon if not running
- **Auto-stop** - Shutdown when no clients connected for N minutes
- **Crash recovery** - Clients detect dead daemon and restart

## Implementation Phases

### Phase 1: Basic gRPC Daemon ⭐ START HERE

```bash
rescript daemon --port 50051
```

**Goals:**
1. Add `rescript daemon` command
2. Define `proto/rescript.proto`
3. Implement `Build`, `Clean`, `Ping` with `tonic`
4. Test with `grpcurl`

**Success criteria:**
```bash
$ grpcurl -plaintext localhost:50051 rescript.daemon.RescriptDaemon/Build
{ "success": true, "moduleCount": 234, "durationSeconds": 1.2 }
```

### Phase 2: CLI as gRPC Client

**Goals:**
1. `rescript build` connects to daemon (or starts one)
2. `rescript watch` uses streaming `Watch`
3. `rescript clean`, `rescript format` use daemon

**Success criteria:**
```bash
# Watch in one terminal, build in another - no conflict
$ rescript watch &
$ rescript build  # Works!
```

### Phase 3: LSP Adapter

**Goals:**
1. Add `rescript lsp` command (runs adapter + daemon)
2. Translate LSP ↔ gRPC
3. Update rescript-vscode to use it

**Success criteria:**
- Editor and CLI share state
- Edit in VS Code, see change in `rescript watch` output, but of course would you still need to run watch mode separately? Perhaps there is a better way to communicate the current state, be it in the IDE or TUI app.

### Phase 4: Vite Plugin

**Goals:**
1. Generate TypeScript client from proto
2. Build `@rescript/vite-plugin`
3. Native gRPC for HMR notifications

### Phase 5: DevTools UI

**Goals:**
1. Module graph visualization
2. Real-time build status
3. Diagnostic browser

### Phase 6: MCP Adapter

**Goals:**
1. Thin MCP → gRPC translation
2. Expose build, modules, diagnostics to AI agents

## Conclusion

This architecture transforms rewatch into the central nervous system of ReScript development:

- **Single source of truth** - One daemon, one BuildState
- **Type-safe API** - Protobuf schema is the contract
- **Generated clients** - Any language can integrate
- **Native streaming** - Real-time updates for watch mode
- **Thin adapters** - LSP and MCP are just protocol translators

The foundation (BuildState, file watching, dependency graph) already exists. Phase 1 wraps it in a gRPC service. Everything else builds on that.
