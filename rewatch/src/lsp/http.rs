use std::convert::Infallible;
use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use http_body_util::{BodyExt, Full};
use hyper::body::Bytes;
use hyper::server::conn::http1;
use hyper::service::service_fn;
use hyper::{Request, Response, StatusCode};
use tokio::net::TcpListener;

use super::diagnostic_store::DiagnosticStore;

const IDLE_TIMEOUT: Duration = Duration::from_secs(30);

async fn handle_request(
    req: Request<hyper::body::Incoming>,
    store: Arc<DiagnosticStore>,
    root_span: tracing::Span,
) -> Result<Response<Full<Bytes>>, Infallible> {
    let path = req.uri().path().to_string();
    let method = req.method().clone();

    if path == "/diagnostics" && method == hyper::Method::GET {
        return Ok(handle_diagnostics(store).await);
    }

    if path == "/report" && method == hyper::Method::POST {
        return Ok(handle_report(req, root_span).await);
    }

    let response = Response::builder()
        .status(StatusCode::NOT_FOUND)
        .body(Full::new(Bytes::from("Not Found")))
        .unwrap();
    Ok(response)
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
            return Response::builder()
                .status(StatusCode::BAD_REQUEST)
                .header("Content-Type", "application/json")
                .body(Full::new(Bytes::from(
                    r#"{"status": "error", "message": "failed to read body"}"#,
                )))
                .unwrap();
        }
    };

    let message = String::from_utf8_lossy(&body_bytes).into_owned();
    let _span = tracing::info_span!(parent: &root_span, "lsp.llm_report", message = %message).entered();

    Response::builder()
        .status(StatusCode::OK)
        .header("Content-Type", "application/json")
        .body(Full::new(Bytes::from(r#"{"status": "recorded"}"#)))
        .unwrap()
}

/// Start the HTTP diagnostics server on the given port.
pub async fn start(store: Arc<DiagnosticStore>, root_span: tracing::Span, port: u16) -> anyhow::Result<()> {
    let listener = TcpListener::bind(SocketAddr::from(([127, 0, 0, 1], port))).await?;

    tokio::spawn(async move {
        loop {
            let (stream, _) = match listener.accept().await {
                Ok(conn) => conn,
                Err(e) => {
                    tracing::warn!("HTTP diagnostics: accept error: {e}");
                    continue;
                }
            };

            let store = Arc::clone(&store);
            let root_span = root_span.clone();
            tokio::spawn(async move {
                let service = service_fn(move |req| {
                    let store = Arc::clone(&store);
                    let root_span = root_span.clone();
                    handle_request(req, store, root_span)
                });
                let io = hyper_util::rt::TokioIo::new(stream);
                if let Err(e) = http1::Builder::new().serve_connection(io, service).await {
                    tracing::warn!("HTTP diagnostics: connection error: {e}");
                }
            });
        }
    });

    Ok(())
}
