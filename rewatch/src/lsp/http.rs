use std::convert::Infallible;
use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use http_body_util::Full;
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
) -> Result<Response<Full<Bytes>>, Infallible> {
    if req.uri().path() != "/diagnostics" || req.method() != hyper::Method::GET {
        let response = Response::builder()
            .status(StatusCode::NOT_FOUND)
            .body(Full::new(Bytes::from("Not Found")))
            .unwrap();
        return Ok(response);
    }

    let is_idle = store.wait_for_idle(IDLE_TIMEOUT).await;
    let snapshot = store.snapshot();
    let build_status = if is_idle { "idle" } else { "timeout" };

    let json = serde_json::to_string_pretty(&snapshot).unwrap_or_else(|_| "{}".to_string());

    let response = Response::builder()
        .status(StatusCode::OK)
        .header("Content-Type", "application/json")
        .header("X-Build-Status", build_status)
        .body(Full::new(Bytes::from(json)))
        .unwrap();

    Ok(response)
}

/// Start the HTTP diagnostics server on the given port.
pub async fn start(store: Arc<DiagnosticStore>, port: u16) -> anyhow::Result<()> {
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
            tokio::spawn(async move {
                let service = service_fn(move |req| {
                    let store = Arc::clone(&store);
                    handle_request(req, store)
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
