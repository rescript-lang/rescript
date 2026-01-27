/**
 * Simple OTLP HTTP receiver for testing.
 *
 * Creates an HTTP server that implements the OpenTelemetry OTLP/HTTP protocol.
 * Supports both JSON and protobuf content types.
 * Collects spans in memory for test assertions.
 *
 * Usage:
 *   const receiver = await createOtelReceiver();
 *   // ... run tests that emit spans to receiver.endpoint ...
 *   const spans = receiver.getSpans();
 *   await receiver.stop();
 */

import http from "node:http";
import protobuf from "protobufjs";

/**
 * Convert a proto AnyValue to a JavaScript value
 */
function anyValueToJs(anyValue) {
  if (!anyValue) return null;
  if (anyValue.stringValue !== undefined) return anyValue.stringValue;
  if (anyValue.boolValue !== undefined) return anyValue.boolValue;
  if (anyValue.intValue !== undefined) return Number(anyValue.intValue);
  if (anyValue.doubleValue !== undefined) return anyValue.doubleValue;
  if (anyValue.bytesValue !== undefined) return anyValue.bytesValue;
  if (anyValue.arrayValue?.values) {
    return anyValue.arrayValue.values.map(anyValueToJs);
  }
  if (anyValue.kvlistValue?.values) {
    const obj = {};
    for (const kv of anyValue.kvlistValue.values) {
      obj[kv.key] = anyValueToJs(kv.value);
    }
    return obj;
  }
  return null;
}

/**
 * Convert proto KeyValue array to a plain object
 */
function attributesToObject(attributes) {
  const obj = {};
  if (!attributes) return obj;
  for (const attr of attributes) {
    obj[attr.key] = anyValueToJs(attr.value);
  }
  return obj;
}

/**
 * Convert a proto Span to a simplified JavaScript object
 */
function bufferToHex(buf) {
  if (!buf || buf.length === 0) return null;
  return Buffer.from(buf).toString("hex");
}

function spanToJs(span, resource, scope) {
  return {
    traceId: bufferToHex(span.traceId),
    spanId: bufferToHex(span.spanId),
    parentSpanId: bufferToHex(span.parentSpanId),
    name: span.name,
    kind: span.kind,
    startTimeUnixNano: span.startTimeUnixNano,
    endTimeUnixNano: span.endTimeUnixNano,
    attributes: attributesToObject(span.attributes),
    status: span.status
      ? { code: span.status.code, message: span.status.message }
      : null,
    events: (span.events || []).map(e => ({
      name: e.name,
      timeUnixNano: e.timeUnixNano,
      attributes: attributesToObject(e.attributes),
    })),
    links: (span.links || []).map(l => ({
      traceId: bufferToHex(l.traceId),
      spanId: bufferToHex(l.spanId),
      attributes: attributesToObject(l.attributes),
    })),
    resource: attributesToObject(resource?.attributes),
    scope: scope ? { name: scope.name, version: scope.version } : null,
  };
}

/**
 * Create an OTLP HTTP receiver for testing.
 *
 * @returns {Promise<{
 *   endpoint: string,
 *   port: number,
 *   getSpans: () => Array,
 *   getSpansByName: (name: string) => Array,
 *   waitForSpan: (predicate: (span) => boolean, timeoutMs?: number) => Promise<object>,
 *   clearSpans: () => void,
 *   stop: () => Promise<void>
 * }>}
 */
// OTLP proto definition for parsing protobuf messages
const OTLP_PROTO = `
syntax = "proto3";
package opentelemetry.proto.collector.trace.v1;

message ExportTraceServiceRequest {
  repeated ResourceSpans resource_spans = 1;
}
message ResourceSpans {
  Resource resource = 1;
  repeated ScopeSpans scope_spans = 2;
  string schema_url = 3;
}
message Resource {
  repeated KeyValue attributes = 1;
  uint32 dropped_attributes_count = 2;
}
message ScopeSpans {
  InstrumentationScope scope = 1;
  repeated Span spans = 2;
  string schema_url = 3;
}
message InstrumentationScope {
  string name = 1;
  string version = 2;
  repeated KeyValue attributes = 3;
  uint32 dropped_attributes_count = 4;
}
message Span {
  bytes trace_id = 1;
  bytes span_id = 2;
  string trace_state = 3;
  bytes parent_span_id = 4;
  string name = 5;
  int32 kind = 6;
  fixed64 start_time_unix_nano = 7;
  fixed64 end_time_unix_nano = 8;
  repeated KeyValue attributes = 9;
  uint32 dropped_attributes_count = 10;
  repeated Event events = 11;
  uint32 dropped_events_count = 12;
  repeated Link links = 13;
  uint32 dropped_links_count = 14;
  Status status = 15;
}
message Status {
  string message = 2;
  int32 code = 3;
}
message Event {
  fixed64 time_unix_nano = 1;
  string name = 2;
  repeated KeyValue attributes = 3;
  uint32 dropped_attributes_count = 4;
}
message Link {
  bytes trace_id = 1;
  bytes span_id = 2;
  string trace_state = 3;
  repeated KeyValue attributes = 4;
  uint32 dropped_attributes_count = 5;
}
message KeyValue {
  string key = 1;
  AnyValue value = 2;
}
message AnyValue {
  oneof value {
    string string_value = 1;
    bool bool_value = 2;
    int64 int_value = 3;
    double double_value = 4;
    ArrayValue array_value = 5;
    KeyValueList kvlist_value = 6;
    bytes bytes_value = 7;
  }
}
message ArrayValue {
  repeated AnyValue values = 1;
}
message KeyValueList {
  repeated KeyValue values = 1;
}
message ExportTraceServiceResponse {
  PartialSuccess partial_success = 1;
}
message PartialSuccess {
  int64 rejected_spans = 1;
  string error_message = 2;
}
`;

export async function createOtelReceiver() {
  const spans = [];

  // Parse the proto definition
  const root = protobuf.parse(OTLP_PROTO, { keepCase: false }).root;
  const ExportTraceServiceRequest = root.lookupType(
    "opentelemetry.proto.collector.trace.v1.ExportTraceServiceRequest",
  );
  const ExportTraceServiceResponse = root.lookupType(
    "opentelemetry.proto.collector.trace.v1.ExportTraceServiceResponse",
  );

  const server = http.createServer((req, res) => {
    if (req.method !== "POST") {
      res.writeHead(405);
      res.end();
      return;
    }

    // Accept traces endpoint
    if (req.url !== "/v1/traces") {
      res.writeHead(404);
      res.end();
      return;
    }

    const chunks = [];
    req.on("data", chunk => {
      chunks.push(chunk);
    });

    req.on("end", () => {
      try {
        const body = Buffer.concat(chunks);
        const contentType = req.headers["content-type"] || "";
        let request;

        if (process.env.DEBUG_OTEL) {
          console.log("[otel-receiver] Content-Type:", contentType);
          console.log("[otel-receiver] Body length:", body.length);
        }

        // Parse based on content type
        if (contentType.includes("application/x-protobuf")) {
          // Protobuf format
          request = ExportTraceServiceRequest.decode(body);
        } else {
          // JSON format (default)
          request = JSON.parse(body.toString());
        }

        if (process.env.DEBUG_OTEL) {
          console.log("[otel-receiver] Received traces request");
        }

        // Extract spans from the request
        if (request.resourceSpans) {
          for (const resourceSpans of request.resourceSpans) {
            const resource = resourceSpans.resource;
            if (resourceSpans.scopeSpans) {
              for (const scopeSpans of resourceSpans.scopeSpans) {
                const scope = scopeSpans.scope;
                if (scopeSpans.spans) {
                  for (const span of scopeSpans.spans) {
                    const jsSpan = spanToJs(span, resource, scope);
                    if (process.env.DEBUG_OTEL) {
                      console.log(
                        "[otel-receiver] Received span:",
                        jsSpan.name,
                      );
                    }
                    spans.push(jsSpan);
                  }
                }
              }
            }
          }
        }

        // Respond with protobuf or JSON based on request content type
        if (contentType.includes("application/x-protobuf")) {
          const response = ExportTraceServiceResponse.create({});
          const responseBuffer =
            ExportTraceServiceResponse.encode(response).finish();
          res.writeHead(200, { "Content-Type": "application/x-protobuf" });
          res.end(Buffer.from(responseBuffer));
        } else {
          res.writeHead(200, { "Content-Type": "application/json" });
          res.end(JSON.stringify({}));
        }
      } catch (err) {
        console.error("[otel-receiver] Error parsing request:", err.message);
        res.writeHead(400);
        res.end(err.message);
      }
    });
  });

  // Bind to random available port
  const port = await new Promise((resolve, reject) => {
    server.listen(0, "127.0.0.1", () => {
      const addr = server.address();
      resolve(addr.port);
    });
    server.on("error", reject);
  });

  console.log(`[otel-receiver] OTLP HTTP receiver listening on port ${port}`);

  return {
    endpoint: `http://127.0.0.1:${port}`,
    port,

    getSpans() {
      return [...spans];
    },

    getSpansByName(name) {
      return spans.filter(s => s.name === name);
    },

    async waitForSpan(predicate, timeoutMs = 5000) {
      const startTime = Date.now();
      while (Date.now() - startTime < timeoutMs) {
        const found = spans.find(predicate);
        if (found) return found;
        await new Promise(r => setTimeout(r, 50));
      }
      throw new Error(
        `Timeout waiting for span. Current spans: ${JSON.stringify(spans.map(s => s.name))}`,
      );
    },

    /**
     * Wait until at least `count` spans matching the predicate are received.
     * @param {string} name - Span name to match
     * @param {number} count - Minimum number of spans to wait for
     * @param {number} timeoutMs - Timeout in milliseconds
     * @returns {Promise<Array>} - The matching spans
     */
    async waitForSpans(name, count = 1, timeoutMs = 5000) {
      const startTime = Date.now();
      while (Date.now() - startTime < timeoutMs) {
        const matching = spans.filter(s => s.name === name);
        if (matching.length >= count) return matching;
        await new Promise(r => setTimeout(r, 50));
      }
      const matching = spans.filter(s => s.name === name);
      throw new Error(
        `Timeout waiting for ${count} spans named "${name}". Got ${matching.length}. All spans: ${JSON.stringify(spans.map(s => s.name))}`,
      );
    },

    clearSpans() {
      spans.length = 0;
    },

    async stop() {
      return new Promise(resolve => {
        server.close(() => resolve());
      });
    },
  };
}

/**
 * Assert that spans appear in order (with possible gaps).
 * Similar to assertEventsInOrder but for spans.
 *
 * @param {Array} actualSpans - Array of collected spans
 * @param {Array} expectedSpans - Array of partial span objects to match
 */
export function assertSpansInOrder(actualSpans, expectedSpans) {
  let actualIndex = 0;

  for (let i = 0; i < expectedSpans.length; i++) {
    const expected = expectedSpans[i];
    let found = false;

    while (actualIndex < actualSpans.length) {
      const actual = actualSpans[actualIndex];
      actualIndex++;

      if (spanMatches(actual, expected)) {
        found = true;
        break;
      }
    }

    if (!found) {
      const actualNames = actualSpans.map(s => s.name);
      throw new Error(
        `Expected span not found: ${JSON.stringify(expected)}\n` +
          `Actual spans: ${JSON.stringify(actualNames)}`,
      );
    }
  }
}

/**
 * Check if an actual span matches an expected partial span.
 */
function spanMatches(actual, expected) {
  // Check name if specified
  if (expected.name !== undefined && actual.name !== expected.name) {
    return false;
  }

  // Check attributes if specified
  if (expected.attributes) {
    for (const [key, value] of Object.entries(expected.attributes)) {
      if (actual.attributes[key] !== value) {
        return false;
      }
    }
  }

  // Check status if specified
  if (expected.status !== undefined) {
    if (typeof expected.status === "string") {
      // Allow matching by status code name
      const codeMap = { OK: 1, ERROR: 2, UNSET: 0 };
      if (actual.status?.code !== codeMap[expected.status]) {
        return false;
      }
    } else if (expected.status?.code !== undefined) {
      if (actual.status?.code !== expected.status.code) {
        return false;
      }
    }
  }

  return true;
}

/**
 * CLI mode: Run as a standalone OTLP file collector.
 *
 * Usage:
 *   node otel-receiver.mjs [--output <file>] [--port <port>]
 *
 * Options:
 *   --output, -o  Output file path (default: stdout on Ctrl+C)
 *   --port, -p    Port to listen on (default: 4318)
 *
 * Examples:
 *   # Start collector, write to file on exit
 *   node otel-receiver.mjs --output traces.json
 *
 *   # Start on specific port
 *   node otel-receiver.mjs --port 4318 --output traces.json
 *
 *   # Use with daemon
 *   OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 rescript daemon
 */
async function main() {
  const args = process.argv.slice(2);
  let outputFile = null;
  let port = 4318;

  // Parse arguments
  for (let i = 0; i < args.length; i++) {
    if (args[i] === "--output" || args[i] === "-o") {
      outputFile = args[++i];
    } else if (args[i] === "--port" || args[i] === "-p") {
      port = parseInt(args[++i], 10);
    } else if (args[i] === "--help" || args[i] === "-h") {
      console.log(`OTLP File Collector - Collects OpenTelemetry spans and writes to file

Usage:
  node otel-receiver.mjs [options]

Options:
  --output, -o <file>  Output file path (writes on exit)
  --port, -p <port>    Port to listen on (default: 4318)
  --help, -h           Show this help message

Examples:
  # Start collector on default port, write to file on Ctrl+C
  node otel-receiver.mjs --output traces.json

  # Start on specific port
  node otel-receiver.mjs --port 4318 --output traces.json

  # Use with the daemon
  OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 rescript daemon

The collector accepts OTLP/HTTP traces at /v1/traces and stores them in memory.
On SIGINT (Ctrl+C), it writes all collected spans to the output file as JSON.`);
      process.exit(0);
    }
  }

  // Create a custom receiver that binds to the specified port
  const { writeFileSync } = await import("node:fs");

  // We need to create a receiver with a specific port
  // Modify the createOtelReceiver to accept port option
  const receiver = await createOtelReceiverWithPort(port);

  console.log(`OTLP collector listening on ${receiver.endpoint}`);
  console.log(
    `Set OTEL_EXPORTER_OTLP_ENDPOINT=${receiver.endpoint} in your daemon`,
  );
  if (outputFile) {
    console.log(`Spans will be written to: ${outputFile}`);
  }
  console.log(`Press Ctrl+C to stop and write spans...`);

  // Handle graceful shutdown
  const shutdown = async () => {
    console.log(
      `\nReceived shutdown signal, writing ${receiver.getSpans().length} spans...`,
    );

    const spans = receiver.getSpans();
    const output = JSON.stringify(spans, null, 2);

    if (outputFile) {
      writeFileSync(outputFile, output);
      console.log(`Wrote ${spans.length} spans to ${outputFile}`);
    } else {
      console.log(output);
    }

    await receiver.stop();
    process.exit(0);
  };

  process.on("SIGINT", shutdown);
  process.on("SIGTERM", shutdown);
}

/**
 * Create an OTLP receiver bound to a specific port.
 * Used by the CLI mode.
 */
async function createOtelReceiverWithPort(requestedPort) {
  const spans = [];

  // Parse the proto definition
  const root = protobuf.parse(OTLP_PROTO, { keepCase: false }).root;
  const ExportTraceServiceRequest = root.lookupType(
    "opentelemetry.proto.collector.trace.v1.ExportTraceServiceRequest",
  );
  const ExportTraceServiceResponse = root.lookupType(
    "opentelemetry.proto.collector.trace.v1.ExportTraceServiceResponse",
  );

  const server = http.createServer((req, res) => {
    if (req.method !== "POST") {
      res.writeHead(405);
      res.end();
      return;
    }

    if (req.url !== "/v1/traces") {
      res.writeHead(404);
      res.end();
      return;
    }

    const chunks = [];
    req.on("data", chunk => {
      chunks.push(chunk);
    });

    req.on("end", () => {
      try {
        const body = Buffer.concat(chunks);
        const contentType = req.headers["content-type"] || "";
        let request;

        if (contentType.includes("application/x-protobuf")) {
          request = ExportTraceServiceRequest.decode(body);
        } else {
          request = JSON.parse(body.toString());
        }

        if (request.resourceSpans) {
          for (const resourceSpans of request.resourceSpans) {
            const resource = resourceSpans.resource;
            if (resourceSpans.scopeSpans) {
              for (const scopeSpans of resourceSpans.scopeSpans) {
                const scope = scopeSpans.scope;
                if (scopeSpans.spans) {
                  for (const span of scopeSpans.spans) {
                    const jsSpan = spanToJs(span, resource, scope);
                    spans.push(jsSpan);
                    console.log(`  [span] ${jsSpan.name}`);
                  }
                }
              }
            }
          }
        }

        if (contentType.includes("application/x-protobuf")) {
          const response = ExportTraceServiceResponse.create({});
          const responseBuffer =
            ExportTraceServiceResponse.encode(response).finish();
          res.writeHead(200, { "Content-Type": "application/x-protobuf" });
          res.end(Buffer.from(responseBuffer));
        } else {
          res.writeHead(200, { "Content-Type": "application/json" });
          res.end(JSON.stringify({}));
        }
      } catch (err) {
        console.error("[otel-receiver] Error parsing request:", err.message);
        res.writeHead(400);
        res.end(err.message);
      }
    });
  });

  const port = await new Promise((resolve, reject) => {
    server.listen(requestedPort, "127.0.0.1", () => {
      const addr = server.address();
      resolve(addr.port);
    });
    server.on("error", reject);
  });

  return {
    endpoint: `http://127.0.0.1:${port}`,
    port,
    getSpans: () => [...spans],
    getSpansByName: name => spans.filter(s => s.name === name),
    clearSpans: () => {
      spans.length = 0;
    },
    async stop() {
      return new Promise(resolve => {
        server.close(() => resolve());
      });
    },
  };
}

// Run CLI if this is the main module
const isMainModule = process.argv[1]?.endsWith("otel-receiver.mjs");
if (isMainModule) {
  main().catch(err => {
    console.error(err);
    process.exit(1);
  });
}
