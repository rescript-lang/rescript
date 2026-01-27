//! OpenTelemetry integration for the daemon.
//!
//! Telemetry is opt-in: set `OTEL_EXPORTER_OTLP_ENDPOINT` to enable OTLP export.
//! If not set, tracing is a no-op with minimal overhead.

use opentelemetry::KeyValue;
use opentelemetry::trace::TracerProvider as _;
use opentelemetry_otlp::WithExportConfig;
use opentelemetry_sdk::Resource;
use opentelemetry_sdk::trace::TracerProvider;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

/// Guard that ensures proper shutdown of the OpenTelemetry tracer on drop.
pub struct TelemetryGuard {
    provider: Option<TracerProvider>,
}

impl Drop for TelemetryGuard {
    fn drop(&mut self) {
        if let Some(provider) = self.provider.take() {
            log::debug!("Shutting down OpenTelemetry tracer provider");
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
    }
}

/// Initialize OpenTelemetry tracing.
///
/// If `OTEL_EXPORTER_OTLP_ENDPOINT` is set, configures OTLP HTTP export.
/// Otherwise, tracing is a no-op.
///
/// Returns a guard that must be kept alive for the duration of the daemon.
/// When dropped, it flushes and shuts down the tracer.
pub fn init() -> TelemetryGuard {
    let endpoint = std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT").ok();

    match endpoint {
        Some(endpoint) => {
            log::info!("Initializing OpenTelemetry with endpoint: {}", endpoint);
            init_with_otlp(&endpoint)
        }
        None => init_noop(),
    }
}

/// Initialize with OTLP exporter.
fn init_with_otlp(endpoint: &str) -> TelemetryGuard {
    use opentelemetry_sdk::trace::BatchConfigBuilder;
    use std::time::Duration;

    // Create resource to identify this service
    let resource = Resource::new(vec![KeyValue::new("service.name", "rescript-daemon")]);

    // Create trace config with resource
    #[allow(deprecated)]
    let trace_config = opentelemetry_sdk::trace::Config::default().with_resource(resource);

    // Configure batch exporter with shorter interval for faster export
    // Default is 5 seconds which is too long for tests
    let batch_config = BatchConfigBuilder::default()
        .with_scheduled_delay(Duration::from_millis(100))
        .build();

    // Use HTTP exporter (simpler than gRPC, works well with various collectors)
    // The endpoint should include /v1/traces path for HTTP
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
    let tracer = provider.tracer("rescript-daemon");

    // Create tracing-opentelemetry layer
    let telemetry_layer = tracing_opentelemetry::layer().with_tracer(tracer);

    // Set up tracing subscriber with OpenTelemetry layer
    // Use RUST_LOG for filtering, default to info level
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("info"));

    tracing_subscriber::registry()
        .with(filter)
        .with(telemetry_layer)
        .init();

    TelemetryGuard {
        provider: Some(provider),
    }
}

/// Initialize with no-op tracing (no export).
fn init_noop() -> TelemetryGuard {
    // Just set up a basic subscriber that does nothing
    // This allows tracing macros to be used without overhead
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("warn"));

    // Only initialize if not already initialized
    let _ = tracing_subscriber::registry().with(filter).try_init();

    TelemetryGuard { provider: None }
}
