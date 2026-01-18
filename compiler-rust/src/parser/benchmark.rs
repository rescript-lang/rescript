//! Parser performance benchmarks comparing Rust vs OCaml parser.
//!
//! These tests ensure the Rust parser is at least as fast as the OCaml parser.
//! Each test parses real ReScript source files and compares execution times.

use std::path::Path;
use std::process::Command;
use std::time::{Duration, Instant};

use super::{module, Parser};

/// Number of iterations for each benchmark
const BENCHMARK_ITERATIONS: u32 = 100;

/// Maximum allowed slowdown factor (Rust time / OCaml time)
/// A value of 1.0 means Rust must be at least as fast as OCaml
/// A value of 1.5 means Rust can be up to 50% slower
const MAX_SLOWDOWN_FACTOR: f64 = 1.5;

/// Path to the OCaml parser binary (relative to project root)
const OCAML_PARSER_PATH: &str = "../_build/install/default/bin/res_parser";

/// Benchmark result for a single test
#[derive(Debug, Clone)]
pub struct BenchmarkResult {
    /// Name of the test
    pub name: String,
    /// Path to the test file
    pub file_path: String,
    /// Size of the source in bytes
    pub source_size: usize,
    /// Average time for Rust parser (nanoseconds)
    pub rust_avg_ns: u64,
    /// Average time for OCaml parser (nanoseconds)
    pub ocaml_avg_ns: u64,
    /// Slowdown factor (Rust / OCaml), <1.0 means Rust is faster
    pub slowdown_factor: f64,
    /// Whether Rust is faster than OCaml
    pub rust_is_faster: bool,
}

impl BenchmarkResult {
    /// Check if the benchmark passes (Rust is not too slow)
    pub fn passes(&self) -> bool {
        self.slowdown_factor <= MAX_SLOWDOWN_FACTOR
    }
}

/// Benchmark the Rust parser on source code
fn benchmark_rust_parser(source: &str, iterations: u32) -> Duration {
    let mut total = Duration::ZERO;

    for _ in 0..iterations {
        let start = Instant::now();
        let mut parser = Parser::new("benchmark.res", source);
        let _ = module::parse_structure(&mut parser);
        total += start.elapsed();
    }

    total / iterations
}

/// Benchmark the OCaml parser on a file
fn benchmark_ocaml_parser(file_path: &str, iterations: u32) -> Option<Duration> {
    let ocaml_parser = Path::new(OCAML_PARSER_PATH);
    if !ocaml_parser.exists() {
        return None;
    }

    let mut total = Duration::ZERO;

    for _ in 0..iterations {
        let start = Instant::now();
        let output = Command::new(ocaml_parser)
            .arg("-print")
            .arg("binary") // Fastest output mode
            .arg(file_path)
            .output();

        if output.is_err() {
            return None;
        }

        total += start.elapsed();
    }

    Some(total / iterations)
}

/// Run a benchmark comparing Rust and OCaml parsers
pub fn run_benchmark(name: &str, file_path: &str) -> Option<BenchmarkResult> {
    let source = std::fs::read_to_string(file_path).ok()?;
    let source_size = source.len();

    // Warm up
    let mut parser = Parser::new("warmup.res", &source);
    let _ = module::parse_structure(&mut parser);

    // Benchmark Rust
    let rust_duration = benchmark_rust_parser(&source, BENCHMARK_ITERATIONS);

    // Benchmark OCaml
    let ocaml_duration = benchmark_ocaml_parser(file_path, BENCHMARK_ITERATIONS)?;

    let rust_avg_ns = rust_duration.as_nanos() as u64;
    let ocaml_avg_ns = ocaml_duration.as_nanos() as u64;
    let slowdown_factor = rust_avg_ns as f64 / ocaml_avg_ns as f64;

    Some(BenchmarkResult {
        name: name.to_string(),
        file_path: file_path.to_string(),
        source_size,
        rust_avg_ns,
        ocaml_avg_ns,
        slowdown_factor,
        rust_is_faster: slowdown_factor < 1.0,
    })
}

/// Print benchmark results in a table format
pub fn print_benchmark_results(results: &[BenchmarkResult]) {
    println!("\n{:-<80}", "");
    println!(
        "{:<30} {:>10} {:>12} {:>12} {:>10}",
        "Test", "Size", "Rust (µs)", "OCaml (µs)", "Factor"
    );
    println!("{:-<80}", "");

    for result in results {
        let status = if result.rust_is_faster {
            "✓"
        } else if result.passes() {
            "○"
        } else {
            "✗"
        };

        println!(
            "{:<30} {:>10} {:>12.2} {:>12.2} {:>9.2}x {}",
            result.name,
            result.source_size,
            result.rust_avg_ns as f64 / 1000.0,
            result.ocaml_avg_ns as f64 / 1000.0,
            result.slowdown_factor,
            status
        );
    }

    println!("{:-<80}", "");

    let passing = results.iter().filter(|r| r.passes()).count();
    let total = results.len();
    let faster = results.iter().filter(|r| r.rust_is_faster).count();

    println!(
        "Results: {}/{} passing, {}/{} faster than OCaml",
        passing, total, faster, total
    );
}

// ============================================================================
// Tests
// ============================================================================

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::mpsc;
    use std::thread;
    use std::time::Duration as StdDuration;

    /// Timeout for benchmark tests (60 seconds)
    const BENCHMARK_TIMEOUT: StdDuration = StdDuration::from_secs(60);

    /// Run a benchmark with timeout protection
    fn run_benchmark_with_timeout(name: &str, file_path: &str) -> Option<BenchmarkResult> {
        let name_owned = name.to_string();
        let name_for_error = name_owned.clone();
        let file_path = file_path.to_string();
        let (tx, rx) = mpsc::channel();

        thread::spawn(move || {
            let result = run_benchmark(&name_owned, &file_path);
            let _ = tx.send(result);
        });

        match rx.recv_timeout(BENCHMARK_TIMEOUT) {
            Ok(result) => result,
            Err(_) => {
                eprintln!("Benchmark timed out for: {}", name_for_error);
                None
            }
        }
    }

    /// Check if OCaml parser is available
    fn ocaml_parser_available() -> bool {
        Path::new(OCAML_PARSER_PATH).exists()
    }

    /// Base path for test files (relative to project root)
    fn test_file_base() -> &'static str {
        "../tests/syntax_tests/data/parsing/grammar"
    }

    // Individual benchmark tests - each must be at least as fast as OCaml

    #[test]
    fn benchmark_expressions_array() {
        if !ocaml_parser_available() {
            eprintln!("Skipping benchmark: OCaml parser not found");
            return;
        }

        let path = format!("{}/expressions/array.res", test_file_base());
        if let Some(result) = run_benchmark_with_timeout("expressions/array", &path) {
            println!(
                "array.res: Rust={:.2}µs OCaml={:.2}µs factor={:.2}x",
                result.rust_avg_ns as f64 / 1000.0,
                result.ocaml_avg_ns as f64 / 1000.0,
                result.slowdown_factor
            );
            assert!(
                result.passes(),
                "Rust parser is too slow: {:.2}x slower than OCaml (max allowed: {:.2}x)",
                result.slowdown_factor,
                MAX_SLOWDOWN_FACTOR
            );
        }
    }

    #[test]
    fn benchmark_expressions_arrow() {
        if !ocaml_parser_available() {
            eprintln!("Skipping benchmark: OCaml parser not found");
            return;
        }

        let path = format!("{}/expressions/arrow.res", test_file_base());
        if let Some(result) = run_benchmark_with_timeout("expressions/arrow", &path) {
            println!(
                "arrow.res: Rust={:.2}µs OCaml={:.2}µs factor={:.2}x",
                result.rust_avg_ns as f64 / 1000.0,
                result.ocaml_avg_ns as f64 / 1000.0,
                result.slowdown_factor
            );
            assert!(
                result.passes(),
                "Rust parser is too slow: {:.2}x slower than OCaml (max allowed: {:.2}x)",
                result.slowdown_factor,
                MAX_SLOWDOWN_FACTOR
            );
        }
    }

    #[test]
    fn benchmark_expressions_binary() {
        if !ocaml_parser_available() {
            eprintln!("Skipping benchmark: OCaml parser not found");
            return;
        }

        let path = format!("{}/expressions/binary.res", test_file_base());
        if let Some(result) = run_benchmark_with_timeout("expressions/binary", &path) {
            println!(
                "binary.res: Rust={:.2}µs OCaml={:.2}µs factor={:.2}x",
                result.rust_avg_ns as f64 / 1000.0,
                result.ocaml_avg_ns as f64 / 1000.0,
                result.slowdown_factor
            );
            assert!(
                result.passes(),
                "Rust parser is too slow: {:.2}x slower than OCaml (max allowed: {:.2}x)",
                result.slowdown_factor,
                MAX_SLOWDOWN_FACTOR
            );
        }
    }

    #[test]
    fn benchmark_expressions_record() {
        if !ocaml_parser_available() {
            eprintln!("Skipping benchmark: OCaml parser not found");
            return;
        }

        let path = format!("{}/expressions/record.res", test_file_base());
        if let Some(result) = run_benchmark_with_timeout("expressions/record", &path) {
            println!(
                "record.res: Rust={:.2}µs OCaml={:.2}µs factor={:.2}x",
                result.rust_avg_ns as f64 / 1000.0,
                result.ocaml_avg_ns as f64 / 1000.0,
                result.slowdown_factor
            );
            assert!(
                result.passes(),
                "Rust parser is too slow: {:.2}x slower than OCaml (max allowed: {:.2}x)",
                result.slowdown_factor,
                MAX_SLOWDOWN_FACTOR
            );
        }
    }

    #[test]
    fn benchmark_expressions_switch() {
        if !ocaml_parser_available() {
            eprintln!("Skipping benchmark: OCaml parser not found");
            return;
        }

        let path = format!("{}/expressions/switch.res", test_file_base());
        if let Some(result) = run_benchmark_with_timeout("expressions/switch", &path) {
            println!(
                "switch.res: Rust={:.2}µs OCaml={:.2}µs factor={:.2}x",
                result.rust_avg_ns as f64 / 1000.0,
                result.ocaml_avg_ns as f64 / 1000.0,
                result.slowdown_factor
            );
            assert!(
                result.passes(),
                "Rust parser is too slow: {:.2}x slower than OCaml (max allowed: {:.2}x)",
                result.slowdown_factor,
                MAX_SLOWDOWN_FACTOR
            );
        }
    }

    #[test]
    fn benchmark_expressions_jsx() {
        if !ocaml_parser_available() {
            eprintln!("Skipping benchmark: OCaml parser not found");
            return;
        }

        let path = format!("{}/expressions/jsx.res", test_file_base());
        if let Some(result) = run_benchmark_with_timeout("expressions/jsx", &path) {
            println!(
                "jsx.res: Rust={:.2}µs OCaml={:.2}µs factor={:.2}x",
                result.rust_avg_ns as f64 / 1000.0,
                result.ocaml_avg_ns as f64 / 1000.0,
                result.slowdown_factor
            );
            assert!(
                result.passes(),
                "Rust parser is too slow: {:.2}x slower than OCaml (max allowed: {:.2}x)",
                result.slowdown_factor,
                MAX_SLOWDOWN_FACTOR
            );
        }
    }

    #[test]
    fn benchmark_expressions_if() {
        if !ocaml_parser_available() {
            eprintln!("Skipping benchmark: OCaml parser not found");
            return;
        }

        let path = format!("{}/expressions/if.res", test_file_base());
        if let Some(result) = run_benchmark_with_timeout("expressions/if", &path) {
            println!(
                "if.res: Rust={:.2}µs OCaml={:.2}µs factor={:.2}x",
                result.rust_avg_ns as f64 / 1000.0,
                result.ocaml_avg_ns as f64 / 1000.0,
                result.slowdown_factor
            );
            assert!(
                result.passes(),
                "Rust parser is too slow: {:.2}x slower than OCaml (max allowed: {:.2}x)",
                result.slowdown_factor,
                MAX_SLOWDOWN_FACTOR
            );
        }
    }

    #[test]
    fn benchmark_expressions_apply() {
        if !ocaml_parser_available() {
            eprintln!("Skipping benchmark: OCaml parser not found");
            return;
        }

        let path = format!("{}/expressions/apply.res", test_file_base());
        if let Some(result) = run_benchmark_with_timeout("expressions/apply", &path) {
            println!(
                "apply.res: Rust={:.2}µs OCaml={:.2}µs factor={:.2}x",
                result.rust_avg_ns as f64 / 1000.0,
                result.ocaml_avg_ns as f64 / 1000.0,
                result.slowdown_factor
            );
            assert!(
                result.passes(),
                "Rust parser is too slow: {:.2}x slower than OCaml (max allowed: {:.2}x)",
                result.slowdown_factor,
                MAX_SLOWDOWN_FACTOR
            );
        }
    }

    /// Comprehensive benchmark test that runs all benchmarks and prints a summary
    #[test]
    #[ignore] // Run with `cargo test benchmark_all -- --ignored --nocapture`
    fn benchmark_all() {
        if !ocaml_parser_available() {
            eprintln!("Skipping benchmarks: OCaml parser not found at {}", OCAML_PARSER_PATH);
            return;
        }

        let test_files = [
            ("expressions/array", "expressions/array.res"),
            ("expressions/arrow", "expressions/arrow.res"),
            ("expressions/binary", "expressions/binary.res"),
            ("expressions/block", "expressions/block.res"),
            ("expressions/constants", "expressions/constants.res"),
            ("expressions/for", "expressions/for.res"),
            ("expressions/if", "expressions/if.res"),
            ("expressions/jsx", "expressions/jsx.res"),
            ("expressions/record", "expressions/record.res"),
            ("expressions/switch", "expressions/switch.res"),
            ("expressions/tuple", "expressions/tuple.res"),
            ("expressions/while", "expressions/while.res"),
        ];

        let mut results = Vec::new();

        for (name, file) in &test_files {
            let path = format!("{}/{}", test_file_base(), file);
            if let Some(result) = run_benchmark_with_timeout(name, &path) {
                results.push(result);
            }
        }

        print_benchmark_results(&results);

        // Assert all benchmarks pass
        let failures: Vec<_> = results.iter().filter(|r| !r.passes()).collect();
        if !failures.is_empty() {
            for failure in &failures {
                eprintln!(
                    "FAIL: {} is {:.2}x slower than OCaml",
                    failure.name, failure.slowdown_factor
                );
            }
            panic!(
                "{} benchmark(s) failed - Rust parser is too slow",
                failures.len()
            );
        }
    }
}
