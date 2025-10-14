#!/usr/bin/env node
// Reuse the simple generator from embeds fixture (supports input.data)
const readStdin = async () => {
  const chunks = [];
  for await (const chunk of process.stdin) chunks.push(chunk);
  return Buffer.concat(chunks).toString('utf8');
};

(async () => {
  try {
    const input = JSON.parse(await readStdin());
    const d = input.data;
    const s = typeof d === 'string' ? d : (d && typeof d === 'object' ? String(d.query || d.id || '') : '');
    let suffix = '1';
    const m = /@name\s+([A-Za-z0-9_]+)/.exec(s);
    if (m) suffix = m[1];
    const code = 'let default = "generated-from: ' + suffix + '"\n';
    process.stdout.write(JSON.stringify({ status: 'ok', code }));
  } catch (err) {
    process.stdout.write(JSON.stringify({ status: 'error', errors: [{ message: String(err) }] }));
    process.exitCode = 0;
  }
})();

