use std::io::{self, Stdout};
use std::path::Path;

use anyhow::Result;
use crossterm::{
    event::{self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEventKind},
    execute,
    terminal::{EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode, enable_raw_mode},
};
use ratatui::{
    Frame, Terminal,
    backend::CrosstermBackend,
    layout::{Constraint, Layout, Rect},
    style::{Color, Modifier, Style, Stylize},
    text::{Line, Span},
    widgets::{Block, Borders, List, ListItem, Paragraph, Wrap},
};
use tokio::sync::mpsc;
use tokio_stream::StreamExt;

use super::connection;
use crate::daemon::proto::{
    BuildType, ClientType, DaemonEvent, DebugRequest, FileChangeType, GetClientsRequest,
    daemon_event::Event as DaemonEventVariant,
};

/// Represents a connected client for display
#[derive(Clone)]
struct ConnectedClient {
    id: u64,
    client_type: ClientType,
    connected_at: String,
}

impl ConnectedClient {
    fn type_name(&self) -> &'static str {
        match self.client_type {
            ClientType::ClientBuild => "build",
            ClientType::ClientWatch => "watch",
            ClientType::ClientClean => "clean",
            ClientType::ClientDebug => "debug",
            ClientType::ClientFormat => "format",
        }
    }

    fn type_color(&self) -> Color {
        match self.client_type {
            ClientType::ClientBuild => Color::Cyan,
            ClientType::ClientWatch => Color::Magenta,
            ClientType::ClientClean => Color::Yellow,
            ClientType::ClientDebug => Color::Green,
            ClientType::ClientFormat => Color::LightBlue,
        }
    }
}

/// Event type for display purposes
#[derive(Clone, Copy)]
enum EventKind {
    ClientConnected,
    ClientDisconnected,
    BuildStarted,
    BuildFinished,
    FileChange,
    Info,
    FormatStarted,
    FormatFinished,
    CircularDependency,
    UnallowedDependency,
    PackageTreeError,
    ModuleNotFound,
    InitializationError,
    CompilerWarning,
    CompilerError,
    ConfigWarning,
    DuplicatedPackage,
    MissingImplementation,
    PackageNameMismatch,
    Cleaned,
    Parsed,
    Compiling,
    Compiled,
    GeneratingAst,
    CleanedCompilerAssets,
    CleanedJsFiles,
    FormatProgress,
    FormatCheckFailed,
    FormattedStdin,
    JsPostBuildOutput,
}

impl EventKind {
    fn color(&self) -> Color {
        match self {
            EventKind::ClientConnected => Color::Green,
            EventKind::ClientDisconnected => Color::Red,
            EventKind::BuildStarted => Color::Yellow,
            EventKind::BuildFinished => Color::Blue,
            EventKind::FileChange => Color::LightYellow,
            EventKind::Info => Color::White,
            EventKind::FormatStarted => Color::LightBlue,
            EventKind::FormatFinished => Color::Cyan,
            EventKind::CircularDependency => Color::Red,
            EventKind::UnallowedDependency => Color::Red,
            EventKind::PackageTreeError => Color::Red,
            EventKind::ModuleNotFound => Color::Red,
            EventKind::InitializationError => Color::Red,
            EventKind::CompilerWarning => Color::Yellow,
            EventKind::CompilerError => Color::Red,
            EventKind::ConfigWarning => Color::Yellow,
            EventKind::DuplicatedPackage => Color::Yellow,
            EventKind::MissingImplementation => Color::Yellow,
            EventKind::PackageNameMismatch => Color::Yellow,
            EventKind::Cleaned => Color::Gray,
            EventKind::Parsed => Color::Gray,
            EventKind::Compiling => Color::Gray,
            EventKind::Compiled => Color::Gray,
            EventKind::GeneratingAst => Color::Gray,
            EventKind::CleanedCompilerAssets => Color::Gray,
            EventKind::CleanedJsFiles => Color::Gray,
            EventKind::FormatProgress => Color::Gray,
            EventKind::FormatCheckFailed => Color::Yellow,
            EventKind::FormattedStdin => Color::Cyan,
            EventKind::JsPostBuildOutput => Color::Magenta,
        }
    }

    fn label(&self) -> &'static str {
        match self {
            EventKind::ClientConnected => "CONNECT",
            EventKind::ClientDisconnected => "DISCONNECT",
            EventKind::BuildStarted => "BUILD_START",
            EventKind::BuildFinished => "BUILD_DONE",
            EventKind::FileChange => "FILE_CHANGE",
            EventKind::Info => "INFO",
            EventKind::FormatStarted => "FMT_START",
            EventKind::FormatFinished => "FMT_DONE",
            EventKind::CircularDependency => "CIRC_DEP",
            EventKind::UnallowedDependency => "BAD_DEP",
            EventKind::PackageTreeError => "PKG_ERROR",
            EventKind::ModuleNotFound => "MOD_NOTFND",
            EventKind::InitializationError => "INIT_ERR",
            EventKind::CompilerWarning => "COMP_WARN",
            EventKind::CompilerError => "COMP_ERR",
            EventKind::ConfigWarning => "CFG_WARN",
            EventKind::DuplicatedPackage => "DUP_PKG",
            EventKind::MissingImplementation => "MISS_IMPL",
            EventKind::PackageNameMismatch => "NAME_MSMTCH",
            EventKind::Cleaned => "CLEANED",
            EventKind::Parsed => "PARSED",
            EventKind::Compiling => "COMPILING",
            EventKind::Compiled => "COMPILED",
            EventKind::GeneratingAst => "GEN_AST",
            EventKind::CleanedCompilerAssets => "CLEAN_COMP",
            EventKind::CleanedJsFiles => "CLEAN_JS",
            EventKind::FormatProgress => "FMT_PROG",
            EventKind::FormatCheckFailed => "FMT_FAIL",
            EventKind::FormattedStdin => "FMT_STDIN",
            EventKind::JsPostBuildOutput => "JS_POST",
        }
    }
}

/// Application state for the TUI
struct App {
    /// Log messages to display
    logs: Vec<LogEntry>,
    /// Currently connected clients
    clients: Vec<ConnectedClient>,
    /// Scroll offset for logs
    scroll_offset: usize,
    /// Whether to auto-scroll to bottom
    auto_scroll: bool,
}

/// A formatted log entry
struct LogEntry {
    timestamp: String,
    event_kind: EventKind,
    message: String,
}

impl App {
    fn new() -> Self {
        Self {
            logs: Vec::new(),
            clients: Vec::new(),
            scroll_offset: 0,
            auto_scroll: true,
        }
    }

    fn set_clients(&mut self, clients: Vec<ConnectedClient>) {
        self.clients = clients;
    }

    fn add_event(&mut self, event: DaemonEvent) {
        let timestamp = event.timestamp;
        let Some(event_variant) = event.event else {
            return;
        };

        // Exhaustive match on ALL event variants - ensures we handle new events
        let (event_kind, message) = match event_variant {
            DaemonEventVariant::ClientConnected(e) => {
                let client_type = ClientType::try_from(e.client_type).unwrap_or(ClientType::ClientBuild);
                // Add to clients list
                if !self.clients.iter().any(|c| c.id == e.client_id) {
                    self.clients.push(ConnectedClient {
                        id: e.client_id,
                        client_type,
                        connected_at: timestamp.clone(),
                    });
                }
                let type_name = client_type_name(client_type);
                let msg = if e.working_directory.is_empty() || e.working_directory == "debug" {
                    format!("#{} {}", e.client_id, type_name)
                } else {
                    format!("#{} {} from {}", e.client_id, type_name, e.working_directory)
                };
                (EventKind::ClientConnected, msg)
            }
            DaemonEventVariant::ClientDisconnected(e) => {
                let client_type = ClientType::try_from(e.client_type).unwrap_or(ClientType::ClientBuild);
                // Remove from clients list
                self.clients.retain(|c| c.id != e.client_id);
                let type_name = client_type_name(client_type);
                (
                    EventKind::ClientDisconnected,
                    format!("#{} {}", e.client_id, type_name),
                )
            }
            DaemonEventVariant::BuildStarted(e) => {
                let build_type = BuildType::try_from(e.build_type).unwrap_or(BuildType::BuildFull);
                let type_str = match build_type {
                    BuildType::BuildFull => "full",
                    BuildType::BuildIncremental => "incremental",
                };
                let mut msg = format!("#{} {} build", e.client_id, type_str);
                if e.is_initial {
                    msg.push_str(" (initial)");
                }
                if let Some(count) = e.file_change_count {
                    msg.push_str(&format!(" - {} file(s)", count));
                }
                (EventKind::BuildStarted, msg)
            }
            DaemonEventVariant::BuildFinished(e) => {
                let status = if e.success { "success" } else { "failed" };
                let mut msg = format!("#{} {} in {:.2}s", e.client_id, status, e.duration_seconds);
                if e.is_clean {
                    msg.push_str(" (clean)");
                }
                if let Some(count) = e.module_count {
                    msg.push_str(&format!(", {} modules", count));
                }
                if let Some(ref error) = e.error {
                    msg.push_str(&format!(": {}", error));
                }
                (EventKind::BuildFinished, msg)
            }
            DaemonEventVariant::FileChanged(e) => {
                let change_type = FileChangeType::try_from(e.change_type).unwrap_or(FileChangeType::Modified);
                let type_str = match change_type {
                    FileChangeType::Modified => "modified",
                    FileChangeType::Created => "created",
                    FileChangeType::Deleted => "deleted",
                    FileChangeType::Renamed => "renamed",
                };
                (EventKind::FileChange, format!("{} ({})", e.path, type_str))
            }
            DaemonEventVariant::WatchPaths(e) => (
                EventKind::Info,
                format!(
                    "watch paths: {} source dirs, {} config files",
                    e.source_paths.len(),
                    e.config_paths.len()
                ),
            ),
            DaemonEventVariant::CompilerWarning(e) => {
                // Truncate long warnings for display
                let msg = if e.message.len() > 80 {
                    format!("{}...", &e.message[..77])
                } else {
                    e.message
                };
                (EventKind::CompilerWarning, msg)
            }
            DaemonEventVariant::CompilerError(e) => {
                // Truncate long errors for display
                let msg = if e.message.len() > 80 {
                    format!("{}...", &e.message[..77])
                } else {
                    e.message
                };
                (EventKind::CompilerError, msg)
            }
            DaemonEventVariant::InitializationError(e) => {
                // Truncate long errors for display
                let msg = if e.message.len() > 80 {
                    format!("{}...", &e.message[..77])
                } else {
                    e.message
                };
                (EventKind::InitializationError, msg)
            }
            DaemonEventVariant::ConfigWarning(e) => (
                EventKind::ConfigWarning,
                format!("{}: {}", e.package_name, e.field_name),
            ),
            DaemonEventVariant::DuplicatedPackage(e) => (
                EventKind::DuplicatedPackage,
                format!("{} ({})", e.package_name, e.chosen_path),
            ),
            DaemonEventVariant::MissingImplementation(e) => {
                (EventKind::MissingImplementation, e.interface_file)
            }
            DaemonEventVariant::ModuleNotFound(e) => (EventKind::ModuleNotFound, e.module_name),
            DaemonEventVariant::PackageNameMismatch(e) => (
                EventKind::PackageNameMismatch,
                format!(
                    "{} ({} vs {})",
                    e.package_path, e.package_json_name, e.rescript_json_name
                ),
            ),
            DaemonEventVariant::Cleaned(e) => {
                let mut msg = format!("{}/{}", e.cleaned_count, e.total_count);
                if e.due_to_compiler_update {
                    msg.push_str(" (compiler update)");
                }
                (EventKind::Cleaned, msg)
            }
            DaemonEventVariant::Parsed(e) => (
                EventKind::Parsed,
                format!("{} files in {:.2}s", e.parsed_count, e.duration_seconds),
            ),
            DaemonEventVariant::Compiling(e) => (
                EventKind::Compiling,
                format!("{}/{}", e.current_count, e.total_count),
            ),
            DaemonEventVariant::Compiled(e) => (
                EventKind::Compiled,
                format!("{} modules in {:.2}s", e.compiled_count, e.duration_seconds),
            ),
            DaemonEventVariant::GeneratingAst(e) => (EventKind::GeneratingAst, e.module_name),
            DaemonEventVariant::CleanedCompilerAssets(e) => (
                EventKind::CleanedCompilerAssets,
                format!("in {:.2}s", e.duration_seconds),
            ),
            DaemonEventVariant::CleanedJsFiles(e) => (
                EventKind::CleanedJsFiles,
                format!("{} in {:.2}s", e.suffix, e.duration_seconds),
            ),
            DaemonEventVariant::CircularDependency(e) => {
                // Show a truncated version of the cycle
                let desc = if e.cycle_description.len() > 60 {
                    format!("{}...", &e.cycle_description[..57])
                } else {
                    e.cycle_description
                };
                (EventKind::CircularDependency, desc)
            }
            DaemonEventVariant::UnallowedDependency(e) => {
                let deps: Vec<String> = e.groups.iter().map(|g| g.deps_type.clone()).collect();
                (
                    EventKind::UnallowedDependency,
                    format!("{} ({})", e.package_name, deps.join(", ")),
                )
            }
            DaemonEventVariant::PackageTreeError(e) => (
                EventKind::PackageTreeError,
                format!("{}: {}", e.package_name, e.error),
            ),
            DaemonEventVariant::FormatStarted(e) => {
                let mut msg = format!("#{}", e.client_id);
                if e.is_stdin {
                    msg.push_str(" stdin");
                } else if e.is_check {
                    msg.push_str(" check");
                } else {
                    msg.push_str(" format");
                }
                if e.file_count > 0 {
                    msg.push_str(&format!(" - {} file(s)", e.file_count));
                }
                (EventKind::FormatStarted, msg)
            }
            DaemonEventVariant::FormatFinished(e) => {
                let status = if e.success { "success" } else { "failed" };
                let mut msg = format!("#{} {} in {:.2}s", e.client_id, status, e.duration_seconds);
                if e.formatted_count > 0 {
                    msg.push_str(&format!(", {} formatted", e.formatted_count));
                }
                if e.failed_count > 0 {
                    msg.push_str(&format!(", {} need formatting", e.failed_count));
                }
                (EventKind::FormatFinished, msg)
            }
            DaemonEventVariant::FormatProgress(e) => (
                EventKind::FormatProgress,
                format!("{}/{}", e.formatted_count, e.total_count),
            ),
            DaemonEventVariant::FormatCheckFailed(e) => (EventKind::FormatCheckFailed, e.file),
            DaemonEventVariant::FormattedStdin(e) => {
                let preview = if e.content.len() > 40 {
                    format!("{}...", &e.content[..37])
                } else {
                    e.content.replace('\n', "\\n")
                };
                (EventKind::FormattedStdin, preview)
            }
            DaemonEventVariant::JsPostBuildOutput(e) => {
                let has_stdout = e.stdout.is_some();
                let has_stderr = e.stderr.is_some();
                let output_info = match (has_stdout, has_stderr) {
                    (true, true) => "stdout+stderr",
                    (true, false) => "stdout",
                    (false, true) => "stderr",
                    (false, false) => "no output",
                };
                (
                    EventKind::JsPostBuildOutput,
                    format!("{} ({})", e.command, output_info),
                )
            }
        };

        self.logs.push(LogEntry {
            timestamp,
            event_kind,
            message,
        });

        // Auto-scroll to bottom if enabled
        if self.auto_scroll {
            self.scroll_to_bottom();
        }
    }

    fn scroll_to_bottom(&mut self) {
        if !self.logs.is_empty() {
            self.scroll_offset = self.logs.len().saturating_sub(1);
        }
    }

    fn scroll_up(&mut self) {
        self.auto_scroll = false;
        self.scroll_offset = self.scroll_offset.saturating_sub(1);
    }

    fn scroll_down(&mut self) {
        self.auto_scroll = false;
        if self.scroll_offset < self.logs.len().saturating_sub(1) {
            self.scroll_offset += 1;
        }
    }

    fn page_up(&mut self, page_size: usize) {
        self.auto_scroll = false;
        self.scroll_offset = self.scroll_offset.saturating_sub(page_size);
    }

    fn page_down(&mut self, page_size: usize) {
        self.auto_scroll = false;
        self.scroll_offset = (self.scroll_offset + page_size).min(self.logs.len().saturating_sub(1));
    }

    fn toggle_auto_scroll(&mut self) {
        self.auto_scroll = !self.auto_scroll;
        if self.auto_scroll {
            self.scroll_to_bottom();
        }
    }
}

fn client_type_name(client_type: ClientType) -> &'static str {
    match client_type {
        ClientType::ClientBuild => "build",
        ClientType::ClientWatch => "watch",
        ClientType::ClientClean => "clean",
        ClientType::ClientDebug => "debug",
        ClientType::ClientFormat => "format",
    }
}

/// Setup terminal for TUI
fn setup_terminal() -> Result<Terminal<CrosstermBackend<Stdout>>> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let terminal = Terminal::new(backend)?;
    Ok(terminal)
}

/// Restore terminal to normal state
fn restore_terminal(terminal: &mut Terminal<CrosstermBackend<Stdout>>) -> Result<()> {
    disable_raw_mode()?;
    execute!(terminal.backend_mut(), LeaveAlternateScreen, DisableMouseCapture)?;
    terminal.show_cursor()?;
    Ok(())
}

/// Draw the UI
fn draw(frame: &mut Frame, app: &App) {
    let area = frame.area();

    // Create two-column layout: 2/3 for logs, 1/3 for clients
    let chunks = Layout::horizontal([Constraint::Percentage(67), Constraint::Percentage(33)]).split(area);

    draw_logs(frame, app, chunks[0]);
    draw_clients(frame, app, chunks[1]);
}

/// Draw the logs panel (left side, 2/3 width)
fn draw_logs(frame: &mut Frame, app: &App, area: Rect) {
    let scroll_indicator = if app.auto_scroll {
        " [AUTO-SCROLL] "
    } else {
        " [MANUAL] "
    };

    let block = Block::default()
        .title(" Debug Events ")
        .title_bottom(Line::from(vec![
            Span::raw(" "),
            Span::styled("q", Style::default().fg(Color::Yellow)),
            Span::raw(":quit "),
            Span::styled("a", Style::default().fg(Color::Yellow)),
            Span::raw(":auto-scroll "),
            Span::styled("\u{2191}\u{2193}", Style::default().fg(Color::Yellow)),
            Span::raw(":scroll "),
            Span::styled(
                scroll_indicator,
                Style::default().fg(if app.auto_scroll {
                    Color::Green
                } else {
                    Color::Gray
                }),
            ),
        ]))
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::Cyan));

    let inner_area = block.inner(area);
    frame.render_widget(block, area);

    // Calculate visible log entries
    let visible_height = inner_area.height as usize;
    let start_idx = app.scroll_offset.saturating_sub(visible_height.saturating_sub(1));
    let end_idx = (start_idx + visible_height).min(app.logs.len());

    let log_items: Vec<ListItem> = app.logs[start_idx..end_idx]
        .iter()
        .map(|entry| {
            let color = entry.event_kind.color();
            let label = entry.event_kind.label();

            let line = Line::from(vec![
                Span::styled(
                    format!("{} ", entry.timestamp),
                    Style::default().fg(Color::DarkGray),
                ),
                Span::styled(
                    format!("{:12} ", label),
                    Style::default().fg(color).add_modifier(Modifier::BOLD),
                ),
                Span::raw(&entry.message),
            ]);

            ListItem::new(line)
        })
        .collect();

    let logs_list = List::new(log_items);
    frame.render_widget(logs_list, inner_area);
}

/// Draw the clients panel (right side, 1/3 width)
fn draw_clients(frame: &mut Frame, app: &App, area: Rect) {
    let block = Block::default()
        .title(format!(" Connected Clients ({}) ", app.clients.len()))
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::Magenta));

    let inner_area = block.inner(area);
    frame.render_widget(block, area);

    if app.clients.is_empty() {
        let empty_msg = Paragraph::new("No clients connected")
            .style(Style::default().fg(Color::DarkGray).italic())
            .wrap(Wrap { trim: true });
        frame.render_widget(empty_msg, inner_area);
    } else {
        let client_items: Vec<ListItem> = app
            .clients
            .iter()
            .map(|client| {
                let line = Line::from(vec![
                    Span::styled(
                        format!("#{:<3} ", client.id),
                        Style::default().fg(Color::DarkGray),
                    ),
                    Span::styled(
                        format!("{:<6}", client.type_name()),
                        Style::default()
                            .fg(client.type_color())
                            .add_modifier(Modifier::BOLD),
                    ),
                    Span::styled(
                        format!(" @ {}", client.connected_at),
                        Style::default().fg(Color::DarkGray),
                    ),
                ]);

                ListItem::new(line)
            })
            .collect();

        let clients_list = List::new(client_items);
        frame.render_widget(clients_list, inner_area);
    }
}

/// Message types for the event loop
enum AppEvent {
    DebugEvent(DaemonEvent),
    Tick,
    Quit,
    ScrollUp,
    ScrollDown,
    PageUp,
    PageDown,
    ToggleAutoScroll,
}

/// Run debug mode - connects to daemon and displays TUI
pub async fn run(root: &Path) -> Result<()> {
    let mut client = connection::connect(root).await?;

    // First call debug() to register ourselves as a debug client
    let stream = client.debug(DebugRequest {}).await?.into_inner();

    // Now query existing clients (including ourselves)
    let existing_clients = client.get_clients(GetClientsRequest {}).await?.into_inner();
    let initial_clients: Vec<ConnectedClient> = existing_clients
        .clients
        .into_iter()
        .map(|c| {
            let client_type = ClientType::try_from(c.client_type).unwrap_or(ClientType::ClientBuild);
            ConnectedClient {
                id: c.id,
                client_type,
                connected_at: c.connected_at,
            }
        })
        .collect();

    // Set up terminal
    let mut terminal = setup_terminal()?;

    // Create app state with existing clients
    let mut app = App::new();
    app.set_clients(initial_clients);

    // Create channel for events
    let (tx, mut rx) = mpsc::channel::<AppEvent>(100);

    // Spawn task to handle keyboard input
    let tx_input = tx.clone();
    tokio::spawn(async move {
        loop {
            if event::poll(std::time::Duration::from_millis(100)).unwrap_or(false)
                && let Ok(Event::Key(key)) = event::read()
                && key.kind == KeyEventKind::Press
            {
                let event = match key.code {
                    KeyCode::Char('c') if key.modifiers.contains(crossterm::event::KeyModifiers::CONTROL) => {
                        AppEvent::Quit
                    }
                    KeyCode::Char('q') | KeyCode::Esc => AppEvent::Quit,
                    KeyCode::Up | KeyCode::Char('k') => AppEvent::ScrollUp,
                    KeyCode::Down | KeyCode::Char('j') => AppEvent::ScrollDown,
                    KeyCode::PageUp => AppEvent::PageUp,
                    KeyCode::PageDown => AppEvent::PageDown,
                    KeyCode::Char('a') => AppEvent::ToggleAutoScroll,
                    KeyCode::Char('g') => AppEvent::ScrollUp, // Will scroll to top with repeated presses
                    KeyCode::Char('G') => AppEvent::ToggleAutoScroll, // Go to bottom and enable auto-scroll
                    _ => continue,
                };
                let should_quit = matches!(event, AppEvent::Quit);
                if tx_input.send(event).await.is_err() {
                    break;
                }
                if should_quit {
                    break;
                }
            }
        }
    });

    // Spawn task to receive debug events from daemon
    let tx_stream = tx.clone();
    let mut stream = stream;
    tokio::spawn(async move {
        while let Some(event) = stream.next().await {
            match event {
                Ok(e) => {
                    if tx_stream.send(AppEvent::DebugEvent(e)).await.is_err() {
                        break;
                    }
                }
                Err(_) => break,
            }
        }
    });

    // Spawn tick task for regular redraws
    let tx_tick = tx;
    tokio::spawn(async move {
        loop {
            tokio::time::sleep(std::time::Duration::from_millis(100)).await;
            if tx_tick.send(AppEvent::Tick).await.is_err() {
                break;
            }
        }
    });

    // Main event loop
    loop {
        // Draw UI
        terminal.draw(|f| draw(f, &app))?;

        // Handle events
        match rx.recv().await {
            Some(AppEvent::DebugEvent(event)) => {
                app.add_event(event);
            }
            Some(AppEvent::Tick) => {
                // Just redraw
            }
            Some(AppEvent::Quit) => {
                break;
            }
            Some(AppEvent::ScrollUp) => {
                app.scroll_up();
            }
            Some(AppEvent::ScrollDown) => {
                app.scroll_down();
            }
            Some(AppEvent::PageUp) => {
                let page_size = terminal.size()?.height as usize / 2;
                app.page_up(page_size);
            }
            Some(AppEvent::PageDown) => {
                let page_size = terminal.size()?.height as usize / 2;
                app.page_down(page_size);
            }
            Some(AppEvent::ToggleAutoScroll) => {
                app.toggle_auto_scroll();
            }
            None => {
                break;
            }
        }
    }

    // Restore terminal
    restore_terminal(&mut terminal)?;

    Ok(())
}
