#!/usr/bin/env node
// Emits a structured error with positions relative to the embedded string; supports v2 batch and v1 single
const readStdin = async () => {
  const chunks = [];
  for await (const c of process.stdin) chunks.push(c);
  return Buffer.concat(chunks).toString('utf8');
};
(async () => {
  try {
    const input = JSON.parse(await readStdin());
    const makeErr = () => ({
      status: 'error',
      errors: [{
        message: 'Example error from generator',
        severity: 'error',
        code: 'GEN001',
        start: { line: 1, column: 10 },
        end: { line: 1, column: 14 }
      }]
    });
    if (input && Array.isArray(input.requests)) {
      const results = input.requests.map(() => makeErr());
      process.stdout.write(JSON.stringify({ results }));
    } else {
      process.stdout.write(JSON.stringify(makeErr()));
    }
  } catch (err) {
    process.stdout.write(JSON.stringify({ results: [{ status: 'error', errors: [{ message: String(err) }] }] }));
  }
})();
