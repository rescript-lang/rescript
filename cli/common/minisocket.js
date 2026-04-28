import * as crypto from "node:crypto";

const KEY_SUFFIX = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";

function hashWebSocketKey(key) {
  const sha1 = crypto.createHash("sha1");
  sha1.update(key + KEY_SUFFIX, "ascii");
  return sha1.digest("base64");
}

/**
 * Limitations: the current implementation does not
 * care about what the client send:
 * - we don't know `ws.close` only know `socket.closed` which is later so it has a latency
 * - ping pong protocol
 */
// http://tools.ietf.org/html/rfc6455#section-5.2
const opcodes = {
  TEXT: 1,
  BINARY: 2,
  CLOSE: 8,
  PING: 9,
  PONG: 10,
};

/**
 *
 * @param {number} opcode
 * @param {Buffer} payload
 */
function encodeMessage(opcode, payload) {
  let buf; // Buffer type
  const b1 = 0x80 | opcode;
  // always send message as one frame (fin)

  // second byte: maks and length part 1
  // followed by 0, 2, or 8 additional bytes of continued length
  let b2 = 0;
  // server does not mask frames
  const length = payload.length;
  if (length < 126) {
    buf = Buffer.allocUnsafe(payload.length + 2 + 0);
    // zero extra bytes
    b2 |= length;
    buf.writeUInt8(b1, 0);
    buf.writeUInt8(b2, 1);
    payload.copy(buf, 2);
  } else if (length < 1 << 16) {
    buf = Buffer.allocUnsafe(payload.length + 2 + 2);
    // two bytes extra
    b2 |= 126;
    buf.writeUInt8(b1, 0);
    buf.writeUInt8(b2, 1);
    // add two byte length
    buf.writeUInt16BE(length, 2);
    payload.copy(buf, 4);
  } else {
    buf = Buffer.allocUnsafe(payload.length + 2 + 8);
    // eight bytes extra
    b2 |= 127;
    buf.writeUInt8(b1, 0);
    buf.writeUInt8(b2, 1);
    // add eight byte length
    // note: this implementation cannot handle lengths greater than 2^32
    // the 32 bit length is prefixed with 0x0000
    buf.writeUInt32BE(0, 2);
    buf.writeUInt32BE(length, 6);
    payload.copy(buf, 10);
  }
  return buf;
}

const upgradeHeader =
  "HTTP/1.1 101 Web Socket Protocol Handshake\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\nsec-websocket-accept: ";

export class WebSocket {
  constructor(req, socket, _upgradeHead) {
    this.socket = socket;
    this.closed = false;
    const key = hashWebSocketKey(req.headers["sec-websocket-key"]);

    // http://tools.ietf.org/html/rfc6455#section-4.2.2
    socket.write(`${upgradeHeader + key}\r\n\r\n`);
    socket.on("close", _hadError => {
      if (!this.closed) {
        this.closed = true;
      }
    });
  }
  sendText(obj) {
    this.socket.write(encodeMessage(opcodes.TEXT, Buffer.from(obj, "utf8")));
  }
}
