#!/usr/bin/env node
// Generator that supports both v2 batch protocol and v1 single protocol (supports input.data)
const readStdin = async () => {
  const chunks = [];
  for await (const chunk of process.stdin) chunks.push(chunk);
  return Buffer.concat(chunks).toString('utf8');
};

(async () => {
  try {
    const input = JSON.parse(await readStdin());
    const handle = (req) => {
      const d = req.data;
      const s = typeof d === 'string' ? d : (d && typeof d === 'object' ? String(d.query || d.id || '') : '');
      let suffix = '1';
      const m = /@name\s+([A-Za-z0-9_]+)/.exec(s);
      if (m) suffix = m[1];
      const code = 'let default = "generated-from: ' + suffix + '"\n';
      return { status: 'ok', code };
    };
    if (input && Array.isArray(input.requests)) {
      const results = input.requests.map(handle);
      process.stdout.write(JSON.stringify({ results }));
    } else {
      const out = handle(input);
      process.stdout.write(JSON.stringify(out));
    }
  } catch (err) {
    process.stdout.write(JSON.stringify({ results: [{ status: 'error', errors: [{ message: String(err) }] }] }));
    process.exitCode = 0;
  }
})();
