#!/usr/bin/env node

import fs from "node:fs";

if (process.argv.length !== 3) {
  console.error("Usage: analyze_typecheck_trace.js TRACE.tsv");
  process.exit(2);
}

const requests = new Map();
const imports = new Map();
const phaseTotals = new Map();
for (const [index, line] of fs.readFileSync(process.argv[2], "utf8").trim().split("\n").entries()) {
  const fields = line.split("\t");
  if (fields.length !== 12) throw new Error(`Invalid row ${index + 1}`);
  const [cwd, input, rawPhase, msText, bytesText, callsText, totalText,
    totalBytesText, minorText, majorText, compactText, heapText] = fields;
  const values = [msText, bytesText, callsText, totalText, totalBytesText,
    minorText, majorText, compactText, heapText].map(Number);
  if (values.some((value) => !Number.isFinite(value))) {
    throw new Error(`Invalid number on row ${index + 1}`);
  }
  const [seconds, bytes, calls, total, totalBytes, minor, major, compactions, heap] = values;
  const kind = input.endsWith(".iast") ? "interface"
    : input.endsWith(".ast") ? "implementation"
    : input.endsWith(".mlmap") ? "namespace"
    : "parse";
  const key = `${cwd}\0${input}`;
  const request = requests.get(key) ?? {
    kind, total, totalBytes, minor, major, compactions, heap, accounted: 0,
  };
  if (Math.abs(request.total - total) > 0.000001 || request.kind !== kind) {
    throw new Error(`Inconsistent request on row ${index + 1}`);
  }
  request.accounted += seconds;
  requests.set(key, request);
  const phase = rawPhase.startsWith("dependency.search_open:")
    ? "dependency.search_open" : rawPhase;
  if (phase !== rawPhase) {
    const name = rawPhase.slice("dependency.search_open:".length);
    const entry = imports.get(name) ?? {calls: 0, seconds: 0};
    entry.calls += calls;
    entry.seconds += seconds;
    imports.set(name, entry);
  }
  const entry = phaseTotals.get(`${kind}\0${phase}`) ?? {calls: 0, seconds: 0, bytes: 0};
  entry.calls += calls;
  entry.seconds += seconds;
  entry.bytes += bytes;
  phaseTotals.set(`${kind}\0${phase}`, entry);
}

for (const [key, request] of requests) {
  if (Math.abs(request.total - request.accounted) > 0.00005) {
    throw new Error(`Unaccounted request time for ${key.replace("\0", "/")}`);
  }
}

for (const kind of ["parse", "interface", "implementation", "namespace"]) {
  const group = [...requests.values()].filter((request) => request.kind === kind);
  if (group.length === 0) continue;
  const sum = (field) => group.reduce((value, request) => value + request[field], 0);
  console.log(`\n${kind}: ${group.length} requests, ${(sum("total") * 1000).toFixed(1)} summed request ms, ${(sum("totalBytes") / 1e6).toFixed(1)} allocated MB`);
  console.log(`GC collections during requests: ${sum("minor")} minor, ${sum("major")} major, ${sum("compactions")} compactions; largest sampled heap ${Math.max(...group.map((request) => request.heap))} words`);
  console.log("phase                             worker_ms  alloc_MB  calls");
  for (const [key, entry] of [...phaseTotals].sort(([a], [b]) => a.localeCompare(b))) {
    const [entryKind, phase] = key.split("\0");
    if (entryKind !== kind) continue;
    console.log(`${phase.padEnd(32)} ${String((entry.seconds * 1000).toFixed(1)).padStart(9)} ${String((entry.bytes / 1e6).toFixed(1)).padStart(9)} ${String(entry.calls).padStart(6)}`);
  }
  const mismatch = group.reduce((value, request) => value + Math.abs(request.total - request.accounted), 0) * 1000;
  console.log(`Exclusive-accounting rounding difference: ${mismatch.toFixed(2)} ms`);
}

console.log("\nMost repeated CMI lookups:");
for (const [name, entry] of [...imports].sort((a, b) => b[1].calls - a[1].calls).slice(0, 12)) {
  console.log(`${String(entry.calls).padStart(4)} calls ${String((entry.seconds * 1000).toFixed(1)).padStart(7)} search/open ms  ${name}`);
}
