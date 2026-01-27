import path from "node:path";
import { fileURLToPath } from "node:url";
import grpc from "@grpc/grpc-js";
import protoLoader from "@grpc/proto-loader";

const PROTO_PATH = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../../../rewatch/proto/rescript.proto",
);

let _proto;

function loadProto() {
  if (!_proto) {
    const packageDef = protoLoader.loadSync(PROTO_PATH, {
      keepCase: true,
      longs: Number,
      enums: String,
      defaults: true,
      oneofs: true,
    });
    _proto = grpc.loadPackageDefinition(packageDef).rescript.daemon;
  }
  return _proto;
}

/**
 * Create a gRPC client connected to the daemon at the given socket path.
 * Unary RPCs are promisified so you can `await client.Ping({})` etc.
 * Streaming RPCs (Build, Clean, Watch, Format, Debug) remain unchanged.
 * @param {string} socketPath
 */
export function createClient(socketPath) {
  const proto = loadProto();
  const raw = new proto.RescriptDaemon(
    `unix://${socketPath}`,
    grpc.credentials.createInsecure(),
  );
  return promisifyClient(raw);
}

const STREAMING_METHODS = new Set([
  "Build",
  "Clean",
  "Watch",
  "Format",
  "Debug",
]);

/**
 * Wrap a gRPC client so that unary RPCs return promises.
 * Streaming RPCs are left as-is.
 */
function promisifyClient(raw) {
  return new Proxy(raw, {
    get(target, prop, receiver) {
      const value = Reflect.get(target, prop, receiver);
      if (typeof value !== "function") return value;
      if (STREAMING_METHODS.has(prop)) return value.bind(target);
      return request =>
        new Promise((resolve, reject) => {
          value.call(target, request, (err, res) =>
            err ? reject(err) : resolve(res),
          );
        });
    },
  });
}

/**
 * Connect as a debug client that receives ALL daemon events.
 * Returns an object with:
 * - `events`: live array that accumulates DaemonEvents
 * - `stream`: the underlying gRPC stream
 * - `client`: the gRPC client instance
 * - `close()`: cancel the stream
 *
 * @param {string} socketPath
 */
export async function createDebugClient(socketPath) {
  const client = createClient(socketPath);
  const events = [];
  const stream = client.Debug({});
  stream.on("data", event => events.push(event));
  stream.on("error", err => {
    if (err.code !== grpc.status.CANCELLED) {
      throw err;
    }
  });

  // Wait for stream to establish
  await new Promise(r => setTimeout(r, 200));

  return {
    events,
    stream,
    client,
    close() {
      stream.cancel();
    },
  };
}

/**
 * Collect all events from a streaming RPC call until the stream ends.
 * @param {grpc.ClientReadableStream} stream
 * @returns {Promise<object[]>}
 */
export function collectStream(stream) {
  return new Promise((resolve, reject) => {
    const events = [];
    stream.on("data", event => events.push(event));
    stream.on("end", () => resolve(events));
    stream.on("error", err => {
      // CANCELLED is expected when we close the stream
      if (err.code === grpc.status.CANCELLED) {
        resolve(events);
      } else {
        reject(err);
      }
    });
  });
}
