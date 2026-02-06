/**
 * Simple OTLP HTTP receiver for testing.
 *
 * Creates an HTTP server that implements the OpenTelemetry OTLP/HTTP protocol.
 * Supports both JSON and protobuf content types.
 * Collects spans in memory for test assertions.
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
    resource: attributesToObject(resource?.attributes),
    scope: scope ? { name: scope.name, version: scope.version } : null,
  };
}

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

  if (process.env.DEBUG_OTEL) {
    console.log(`[otel-receiver] OTLP HTTP receiver listening on port ${port}`);
  }

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
