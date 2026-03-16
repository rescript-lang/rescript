use std::collections::HashMap;
use std::convert::Infallible;
use std::net::SocketAddr;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use http_body_util::{BodyExt, Full};
use hyper::body::Bytes;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Request, Response, StatusCode};
use tokio::net::TcpListener;
use tower_lsp::lsp_types::{Position, Url};

use opentelemetry::trace::TraceContextExt;
use tracing_opentelemetry::OpenTelemetrySpanExt;

use super::diagnostic_store::DiagnosticStore;
use super::{ProjectMap, definition, document_symbol, hover, references, workspace_symbol};

const IDLE_TIMEOUT: Duration = Duration::from_secs(30);

/// Parse query parameters from a URI query string into a HashMap.
fn parse_query(uri: &hyper::Uri) -> HashMap<String, String> {
    uri.query()
        .map(|q| {
            q.split('&')
                .filter_map(|pair| {
                    let (key, value) = pair.split_once('=')?;
                    Some((key.to_string(), value.to_string()))
                })
                .collect()
        })
        .unwrap_or_default()
}

/// Build a JSON 200 response.
fn json_ok(body: &str) -> Response<Full<Bytes>> {
    Response::builder()
        .status(StatusCode::OK)
        .header("Content-Type", "application/json")
        .body(Full::new(Bytes::from(body.to_owned())))
        .unwrap()
}

/// Build a JSON 400 Bad Request response.
fn json_bad_request(message: &str) -> Response<Full<Bytes>> {
    let body = serde_json::json!({ "error": message }).to_string();
    Response::builder()
        .status(StatusCode::BAD_REQUEST)
        .header("Content-Type", "application/json")
        .body(Full::new(Bytes::from(body)))
        .unwrap()
}

/// Build a JSON 404 Not Found response (no result from handler).
fn json_not_found(message: &str) -> Response<Full<Bytes>> {
    let body = serde_json::json!({ "error": message }).to_string();
    Response::builder()
        .status(StatusCode::NOT_FOUND)
        .header("Content-Type", "application/json")
        .body(Full::new(Bytes::from(body)))
        .unwrap()
}

type HttpResult<T> = Result<T, Box<Response<Full<Bytes>>>>;

/// Extract required `file` query param, read source from disk, and build a Url.
/// Returns (file_path, uri, source) or an error response.
fn resolve_file_from_query(params: &HashMap<String, String>) -> HttpResult<(PathBuf, Url, String)> {
    let file = params
        .get("file")
        .ok_or_else(|| Box::new(json_bad_request("missing required query parameter: file")))?;
    let file_path = PathBuf::from(file);
    if !file_path.is_absolute() {
        return Err(Box::new(json_bad_request("file must be an absolute path")));
    }
    let uri = Url::from_file_path(&file_path)
        .map_err(|_| Box::new(json_bad_request("could not convert file path to URI")))?;
    let source = std::fs::read_to_string(&file_path)
        .map_err(|e| Box::new(json_not_found(&format!("could not read file: {e}"))))?;
    Ok((file_path, uri, source))
}

/// Extract `line` and `col` query params as a Position.
fn parse_position(params: &HashMap<String, String>) -> HttpResult<Position> {
    let line: u32 = params
        .get("line")
        .ok_or_else(|| Box::new(json_bad_request("missing required query parameter: line")))?
        .parse()
        .map_err(|_| Box::new(json_bad_request("line must be a non-negative integer")))?;
    let col: u32 = params
        .get("col")
        .ok_or_else(|| Box::new(json_bad_request("missing required query parameter: col")))?
        .parse()
        .map_err(|_| Box::new(json_bad_request("col must be a non-negative integer")))?;
    Ok(Position { line, character: col })
}

async fn handle_request(
    req: Request<hyper::body::Incoming>,
    store: Arc<DiagnosticStore>,
    projects: Arc<Mutex<ProjectMap>>,
    root_span: tracing::Span,
) -> Result<Response<Full<Bytes>>, Infallible> {
    let path = req.uri().path().to_string();
    let method = req.method().clone();

    if method != hyper::Method::GET && path != "/report" {
        return Ok(json_not_found("Not Found"));
    }

    match path.as_str() {
        "/diagnostics" if method == hyper::Method::GET => Ok(handle_diagnostics(store).await),
        "/report" if method == hyper::Method::POST => Ok(handle_report(req, root_span).await),
        "/hover" => Ok(handle_hover(req.uri(), &projects)),
        "/definition" => Ok(handle_definition(req.uri(), &projects)),
        "/references" => Ok(handle_references(req.uri(), &projects)),
        "/document-symbols" => Ok(handle_document_symbols(req.uri(), &projects)),
        "/workspace-symbols" => Ok(handle_workspace_symbols(req.uri(), &projects)),
        _ => Ok(json_not_found("Not Found")),
    }
}

async fn handle_diagnostics(store: Arc<DiagnosticStore>) -> Response<Full<Bytes>> {
    let is_idle = store.wait_for_idle(IDLE_TIMEOUT).await;
    let snapshot = store.snapshot();
    let build_status = if is_idle { "idle" } else { "timeout" };

    let json = serde_json::to_string_pretty(&snapshot).unwrap_or_else(|_| "{}".to_string());

    Response::builder()
        .status(StatusCode::OK)
        .header("Content-Type", "application/json")
        .header("X-Build-Status", build_status)
        .body(Full::new(Bytes::from(json)))
        .unwrap()
}

async fn handle_report(
    req: Request<hyper::body::Incoming>,
    root_span: tracing::Span,
) -> Response<Full<Bytes>> {
    let body_bytes = match req.into_body().collect().await {
        Ok(collected) => collected.to_bytes(),
        Err(e) => {
            tracing::warn!("HTTP report: failed to read body: {e}");
            return json_bad_request("failed to read body");
        }
    };

    let message = String::from_utf8_lossy(&body_bytes).into_owned();
    let span = tracing::info_span!(parent: &root_span, "lsp.llm_report", message = %message);
    let _entered = span.enter();

    // Extract the OTEL span ID so the caller can look it up in the trace viewer
    let otel_context = span.context();
    let span_ref = otel_context.span();
    let span_id = span_ref.span_context().span_id().to_string();

    let body = format!(r#"{{"status": "recorded", "span_id": "{span_id}"}}"#);
    json_ok(&body)
}

fn handle_hover(uri: &hyper::Uri, projects: &Mutex<ProjectMap>) -> Response<Full<Bytes>> {
    let params = parse_query(uri);
    let (file_path, file_uri, source) = match resolve_file_from_query(&params) {
        Ok(v) => v,
        Err(resp) => return *resp,
    };
    let position = match parse_position(&params) {
        Ok(v) => v,
        Err(resp) => return *resp,
    };

    match hover::handle_with_source(projects, &file_path, &file_uri, &source, position) {
        Some(result) => {
            let json = serde_json::to_string_pretty(&result).unwrap_or_else(|_| "null".to_string());
            json_ok(&json)
        }
        None => json_not_found("no hover information available"),
    }
}

fn handle_definition(uri: &hyper::Uri, projects: &Mutex<ProjectMap>) -> Response<Full<Bytes>> {
    let params = parse_query(uri);
    let (file_path, file_uri, source) = match resolve_file_from_query(&params) {
        Ok(v) => v,
        Err(resp) => return *resp,
    };
    let position = match parse_position(&params) {
        Ok(v) => v,
        Err(resp) => return *resp,
    };

    match definition::handle_with_source(projects, &file_path, &file_uri, &source, position) {
        Some(result) => {
            let json = serde_json::to_string_pretty(&result).unwrap_or_else(|_| "null".to_string());
            json_ok(&json)
        }
        None => json_not_found("no definition found"),
    }
}

fn handle_references(uri: &hyper::Uri, projects: &Mutex<ProjectMap>) -> Response<Full<Bytes>> {
    let params = parse_query(uri);
    let (file_path, file_uri, source) = match resolve_file_from_query(&params) {
        Ok(v) => v,
        Err(resp) => return *resp,
    };
    let position = match parse_position(&params) {
        Ok(v) => v,
        Err(resp) => return *resp,
    };

    match references::handle_with_source(projects, &file_path, &file_uri, &source, position) {
        Some(result) => {
            let json = serde_json::to_string_pretty(&result).unwrap_or_else(|_| "[]".to_string());
            json_ok(&json)
        }
        None => json_ok("[]"),
    }
}

fn handle_document_symbols(uri: &hyper::Uri, projects: &Mutex<ProjectMap>) -> Response<Full<Bytes>> {
    let params = parse_query(uri);
    let (file_path, file_uri, source) = match resolve_file_from_query(&params) {
        Ok(v) => v,
        Err(resp) => return *resp,
    };

    match document_symbol::handle_with_source(projects, &file_path, &file_uri, &source) {
        Some(result) => {
            let json = serde_json::to_string_pretty(&result).unwrap_or_else(|_| "[]".to_string());
            json_ok(&json)
        }
        None => json_ok("[]"),
    }
}

fn handle_workspace_symbols(uri: &hyper::Uri, projects: &Mutex<ProjectMap>) -> Response<Full<Bytes>> {
    let params = parse_query(uri);
    let query = params.get("query").map(|s| s.as_str()).unwrap_or("");

    match workspace_symbol::handle(projects, query) {
        Some(result) => {
            let json = serde_json::to_string_pretty(&result).unwrap_or_else(|_| "[]".to_string());
            json_ok(&json)
        }
        None => json_ok("[]"),
    }
}

/// Start the HTTP server on the given port.
pub async fn start(
    store: Arc<DiagnosticStore>,
    projects: Arc<Mutex<ProjectMap>>,
    root_span: tracing::Span,
    port: u16,
) -> anyhow::Result<()> {
    let listener = TcpListener::bind(SocketAddr::from(([127, 0, 0, 1], port))).await?;

    tokio::spawn(async move {
        loop {
            let (stream, _) = match listener.accept().await {
                Ok(conn) => conn,
                Err(e) => {
                    tracing::warn!("HTTP server: accept error: {e}");
                    continue;
                }
            };

            let store = Arc::clone(&store);
            let projects = Arc::clone(&projects);
            let root_span = root_span.clone();
            tokio::spawn(async move {
                let service = service_fn(move |req| {
                    let store = Arc::clone(&store);
                    let projects = Arc::clone(&projects);
                    let root_span = root_span.clone();
                    handle_request(req, store, projects, root_span)
                });
                let io = hyper_util::rt::TokioIo::new(stream);
                if let Err(e) = http1::Builder::new().serve_connection(io, service).await {
                    tracing::warn!("HTTP server: connection error: {e}");
                }
            });
        }
    });

    Ok(())
}
