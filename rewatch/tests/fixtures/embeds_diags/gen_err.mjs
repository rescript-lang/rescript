#!/usr/bin/env node
// Emits a structured error with positions relative to the embedded string
const readStdin = async () => {
  const chunks = [];
  for await (const c of process.stdin) chunks.push(c);
  return Buffer.concat(chunks).toString('utf8');
};
(async () => {
  try {
    const input = JSON.parse(await readStdin());
    // Report a single error at line 1, col 10-14 of the embed literal
    const out = {
      status: 'error',
      errors: [
        {
          message: 'Example error from generator',
          severity: 'error',
          code: 'GEN001',
          start: { line: 1, column: 10 },
          end: { line: 1, column: 14 }
        }
      ]
    };
    process.stdout.write(JSON.stringify(out));
  } catch (err) {
    process.stdout.write(JSON.stringify({ status: 'error', errors: [{ message: String(err) }] }));
  }
})();

