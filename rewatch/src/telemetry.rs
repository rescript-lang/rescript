//! OpenTelemetry setup for rewatch.
//!
//! Provides optional tracing export via OTLP HTTP when OTEL_EXPORTER_OTLP_ENDPOINT is set.
//! When the environment variable is not set, tracing is disabled (no-op).

use opentelemetry::trace::TracerProvider as _;
use opentelemetry_otlp::{Protocol, SpanExporter, WithExportConfig};
use opentelemetry_sdk::Resource;
use opentelemetry_sdk::trace::SdkTracerProvider;
use tracing_subscriber::EnvFilter;
use tracing_subscriber::fmt;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

/// Guard that ensures telemetry is properly flushed and shut down.
/// Drop this at the end of main to flush all spans.
pub struct TelemetryGuard {
    provider: Option<SdkTracerProvider>,
    /// Whether OTEL tracing was initialized (affects logger setup in main).
    otel_enabled: bool,
}

impl TelemetryGuard {
    /// Returns whether OTLP span export was successfully initialized.
    pub fn otel_enabled(&self) -> bool {
        self.otel_enabled
    }
}

impl Drop for TelemetryGuard {
    fn drop(&mut self) {
        if let Some(provider) = self.provider.take() {
            // Force flush - this exports any buffered spans
            if let Err(e) = provider.force_flush() {
                log::warn!("Error flushing tracer provider: {}", e);
            }
            // Shutdown
            if let Err(e) = provider.shutdown() {
                log::warn!("Error shutting down tracer provider: {}", e);
            }
        }
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
    // Presence of either the general or trace-specific endpoint env var enables OTLP export.
    init_telemetry_for_endpoint_presence(
        std::env::var_os("OTEL_EXPORTER_OTLP_ENDPOINT").is_some(),
        std::env::var_os("OTEL_EXPORTER_OTLP_TRACES_ENDPOINT").is_some(),
    )
}

fn init_telemetry_for_endpoint_presence(endpoint_set: bool, traces_endpoint_set: bool) -> TelemetryGuard {
    if endpoint_set || traces_endpoint_set {
        init_with_otlp()
    } else {
        init_noop()
    }
}

/// Initialize with OTLP exporter.
fn init_with_otlp() -> TelemetryGuard {
    // Build a blocking reqwest-backed HTTP exporter. Blocking is fine here
    // because the batch span processor runs in its own background thread.
    //
    // The exporter reads OTEL_EXPORTER_OTLP_TRACES_ENDPOINT (used verbatim)
    // or falls back to OTEL_EXPORTER_OTLP_ENDPOINT (with /v1/traces appended),
    // matching the OTEL spec — so we pass no explicit endpoint here.
    let exporter = match SpanExporter::builder()
        .with_http()
        .with_protocol(Protocol::HttpBinary)
        .build()
    {
        Ok(exp) => exp,
        Err(e) => {
            log::warn!("Failed to create OTLP exporter: {}. Falling back to no-op.", e);
            return init_noop();
        }
    };

    // Resource::builder() applies the SdkProvidedResourceDetector and
    // EnvResourceDetector, which already pick up OTEL_SERVICE_NAME and
    // OTEL_RESOURCE_ATTRIBUTES from the environment. Only default
    // service.name = "rewatch" when the user hasn't supplied their own.
    let mut resource_builder = Resource::builder();
    if std::env::var_os("OTEL_SERVICE_NAME").is_none() {
        resource_builder = resource_builder.with_service_name("rewatch");
    }
    let resource = resource_builder.build();

    let provider = SdkTracerProvider::builder()
        .with_batch_exporter(exporter)
        .with_resource(resource)
        .build();

    // Bridge the `tracing` crate into this provider.
    let tracer = provider.tracer("rewatch");
    let telemetry_layer = tracing_opentelemetry::layer().with_tracer(tracer);

    // Default to debug level so that diagnostic tracing::debug! events
    // (e.g. in the watcher) are captured as OTEL span events.
    let filter = EnvFilter::try_from_default_env().unwrap_or_else(|_| EnvFilter::new("debug"));

    // Also write logs to stderr. Without this layer, `log::*` / `tracing::*`
    // output is silently swallowed when telemetry is enabled, since the
    // OTEL layer only exports spans — it does not print them.
    let fmt_layer = fmt::layer().with_writer(std::io::stderr);

    tracing_subscriber::registry()
        .with(filter)
        .with(telemetry_layer)
        .with(fmt_layer)
        .init();

    TelemetryGuard {
        provider: Some(provider),
        otel_enabled: true,
    }
}

/// Initialize with no-op tracing (no export).
fn init_noop() -> TelemetryGuard {
    // Don't set up any subscriber - let main.rs handle logging normally
    TelemetryGuard {
        provider: None,
        otel_enabled: false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn noop_guard_reports_otel_disabled() {
        let guard = init_noop();
        assert!(!guard.otel_enabled());
        // Explicit drop — must not panic with provider == None.
        drop(guard);
    }

    #[test]
    fn noop_guard_drops_cleanly_without_explicit_shutdown() {
        // Implicit drop at end of scope: exercises Drop with no provider.
        let _guard = init_noop();
    }

    #[test]
    fn init_telemetry_without_env_is_noop() {
        let guard = init_telemetry_for_endpoint_presence(false, false);
        assert!(
            !guard.otel_enabled(),
            "expected no-op guard when OTEL endpoint env vars are unset"
        );
    }
}
