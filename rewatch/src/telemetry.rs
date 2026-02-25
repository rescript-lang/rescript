//! OpenTelemetry setup for rewatch.
//!
//! Provides optional tracing export via OTLP HTTP when OTEL_EXPORTER_OTLP_ENDPOINT is set.
//! When the environment variable is not set, tracing is disabled (no-op).

use opentelemetry::KeyValue;
use opentelemetry::trace::TracerProvider as _;
use opentelemetry_otlp::WithExportConfig;
use opentelemetry_sdk::Resource;
use opentelemetry_sdk::trace::TracerProvider;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

/// Guard that ensures telemetry is properly flushed and shut down.
/// Drop this at the end of main to flush all spans.
pub struct TelemetryGuard {
    provider: Option<TracerProvider>,
    // Keep the runtime alive for the batch exporter
    _runtime: Option<tokio::runtime::Runtime>,
    /// Whether OTEL tracing was initialized (affects logger setup in main)
    pub otel_enabled: bool,
}

impl Drop for TelemetryGuard {
    fn drop(&mut self) {
        if let Some(provider) = self.provider.take() {
            // Force flush - this exports any buffered spans
            for result in provider.force_flush() {
                if let Err(e) = result {
                    log::warn!("Error flushing tracer provider: {}", e);
                }
            }
            // Shutdown
            if let Err(e) = provider.shutdown() {
                log::warn!("Error shutting down tracer provider: {}", e);
            }
        }
        // Runtime is dropped after provider is shut down
    }
}

/// Initialize OpenTelemetry tracing if OTEL_EXPORTER_OTLP_ENDPOINT is set.
///
/// Returns a guard that must be kept alive for the duration of the program.
/// When the guard is dropped, it flushes all pending spans.
///
/// If OTEL_EXPORTER_OTLP_ENDPOINT is not set, returns a no-op guard and
/// tracing calls become no-ops.
pub fn init_telemetry() -> TelemetryGuard {
    let endpoint = std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT").ok();

    match endpoint {
        Some(endpoint) => init_with_otlp(&endpoint),
        None => init_noop(),
    }
}

/// Initialize with OTLP exporter.
fn init_with_otlp(endpoint: &str) -> TelemetryGuard {
    use opentelemetry_sdk::trace::BatchConfigBuilder;
    use std::time::Duration;

    // Create a Tokio runtime for the batch exporter
    let runtime = match tokio::runtime::Builder::new_multi_thread().enable_all().build() {
        Ok(rt) => rt,
        Err(e) => {
            log::warn!("Failed to create Tokio runtime: {}. Falling back to no-op.", e);
            return init_noop();
        }
    };

    // Enter the runtime context for creating the provider
    let _guard = runtime.enter();

    // Create resource to identify this service
    let resource = Resource::new(vec![KeyValue::new("service.name", "rewatch")]);

    // Configure trace with resource
    #[allow(deprecated)]
    let trace_config = opentelemetry_sdk::trace::Config::default().with_resource(resource);

    // Configure batch exporter with shorter interval for faster export during tests
    let batch_config = BatchConfigBuilder::default()
        .with_scheduled_delay(Duration::from_millis(100))
        .build();

    // Use HTTP exporter - endpoint should include /v1/traces path
    let traces_endpoint = format!("{}/v1/traces", endpoint.trim_end_matches('/'));

    // Create tracer provider using HTTP OTLP exporter
    let provider = match opentelemetry_otlp::new_pipeline()
        .tracing()
        .with_exporter(
            opentelemetry_otlp::new_exporter()
                .http()
                .with_endpoint(&traces_endpoint),
        )
        .with_trace_config(trace_config)
        .with_batch_config(batch_config)
        .install_batch(opentelemetry_sdk::runtime::Tokio)
    {
        Ok(provider) => provider,
        Err(e) => {
            log::warn!("Failed to create OTLP tracer: {}. Falling back to no-op.", e);
            return init_noop();
        }
    };

    // Get a tracer from the provider
    let tracer = provider.tracer("rewatch");

    // Create tracing-opentelemetry layer
    let telemetry_layer = tracing_opentelemetry::layer().with_tracer(tracer);

    // Set up tracing subscriber with OpenTelemetry layer.
    // Default to debug level so that diagnostic tracing::debug! events
    // (e.g. in the watcher) are captured as OTEL span events.
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("debug"));

    tracing_subscriber::registry()
        .with(filter)
        .with(telemetry_layer)
        .init();

    TelemetryGuard {
        provider: Some(provider),
        _runtime: Some(runtime),
        otel_enabled: true,
    }
}

/// Initialize with no-op tracing (no export).
fn init_noop() -> TelemetryGuard {
    // Don't set up any subscriber - let main.rs handle logging normally
    TelemetryGuard {
        provider: None,
        _runtime: None,
        otel_enabled: false,
    }
}
