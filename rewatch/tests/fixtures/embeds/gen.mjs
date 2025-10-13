#!/usr/bin/env node
// Minimal generator that reads a single JSON object from stdin and writes a JSON object to stdout.
/** Protocol v1 **/
const readStdin = async () => {
  const chunks = [];
  for await (const chunk of process.stdin) chunks.push(chunk);
  return Buffer.concat(chunks).toString('utf8');
};

(async () => {
  try {
    const input = JSON.parse(await readStdin());
    const s = String(input.embedString || '');
    let suffix = '_1';
    const m = /@name\s+([A-Za-z0-9_]+)/.exec(s);
    if (m) suffix = m[1];
    const code = 'let default = "generated-from: ' + suffix + '"\n';
    // record a side-effect so tests can assert cache hits/misses
    try {
      const fs = await import('node:fs');
      fs.appendFileSync('gen-runs.log', `${new Date().toISOString()} ${input.tag} ${suffix}\n`);
    } catch {}
    process.stdout.write(JSON.stringify({ status: 'ok', code, suffix }));
  } catch (err) {
    process.stdout.write(JSON.stringify({ status: 'error', errors: [{ message: String(err) }] }));
    process.exitCode = 0; // keep non-error status to simplify fixture
  }
})();
