#!/usr/bin/env node
// Generator that supports both v2 batch protocol and v1 single protocol.
const readStdin = async () => {
  const chunks = [];
  for await (const chunk of process.stdin) chunks.push(chunk);
  return Buffer.concat(chunks).toString('utf8');
};

// Helper that works in both CJS and ESM contexts
const appendRunLog = async (tag, suffix) => {
  try {
    let fs;
    if (typeof require !== 'undefined') {
      // CommonJS
      fs = require('node:fs');
    } else {
      // ESM
      const mod = await import('node:fs');
      fs = mod.default || mod;
    }
    fs.appendFileSync('gen-runs.log', `${new Date().toISOString()} ${tag} ${suffix}\n`);
  } catch {}
};

(async () => {
  try {
    const input = JSON.parse(await readStdin());
    const handle = async (req) => {
      const d = req.data;
      const s = typeof d === 'string' ? d : (d && typeof d === 'object' ? String(d.query || d.id || '') : '');
      let suffix = '1';
      const m = /@name\s+([A-Za-z0-9_]+)/.exec(s);
      if (m) suffix = m[1];
      const code = 'let default = "generated-from: ' + suffix + '"\n';
      // record a side-effect so tests can assert cache hits/misses
      await appendRunLog(req.tag, suffix);
      return { status: 'ok', code };
    };
    if (input && Array.isArray(input.requests)) {
      const results = await Promise.all(input.requests.map(handle));
      process.stdout.write(JSON.stringify({ results }));
    } else {
      const out = await handle(input);
      process.stdout.write(JSON.stringify(out));
    }
  } catch (err) {
    process.stdout.write(JSON.stringify({ results: [{ status: 'error', errors: [{ message: String(err) }] }] }));
    process.exitCode = 0; // keep non-error status to simplify fixture
  }
})();
