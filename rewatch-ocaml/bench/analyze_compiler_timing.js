#!/usr/bin/env node

import fs from "node:fs";

if (process.argv.length !== 3) {
  console.error("Usage: analyze_compiler_timing.js TIMING_LOG");
  process.exit(2);
}

const rows = fs
  .readFileSync(process.argv[2], "utf8")
  .trim()
  .split("\n")
  .filter(Boolean)
  .map((line, index) => {
    const [phase, cwd, input, startText, endText, ...extra] = line.split("\t");
    const start = Number(startText);
    const end = Number(endText);
    if (
      extra.length > 0 ||
      !["parse", "namespace", "interface", "implementation"].includes(phase) ||
      !cwd ||
      !input ||
      !Number.isFinite(start) ||
      !Number.isFinite(end) ||
      end <= start
    ) {
      throw new Error(`Invalid timing row ${index + 1}`);
    }
    return { phase, cwd, input, start, end };
  });

function summarize(name, requests) {
  if (requests.length === 0) return;
  const durations = requests
    .map(({ start, end }) => (end - start) * 1000)
    .sort((a, b) => a - b);
  const events = requests
    .flatMap(({ start, end }) => [
      { time: start, delta: 1 },
      { time: end, delta: -1 },
    ])
    .sort((a, b) => a.time - b.time || a.delta - b.delta);
  let active = 0;
  let peak = 0;
  let zeroActive = 0;
  let previous = events[0].time;
  for (const { time, delta } of events) {
    if (active === 0) zeroActive += time - previous;
    active += delta;
    peak = Math.max(peak, active);
    previous = time;
  }
  const span = (events.at(-1).time - events[0].time) * 1000;
  const summed = durations.reduce((total, duration) => total + duration, 0);
  console.log(
    [
      name,
      requests.length,
      span.toFixed(1),
      summed.toFixed(1),
      (summed / span).toFixed(2),
      peak,
      (zeroActive * 1000).toFixed(1),
      durations[Math.ceil(0.95 * durations.length) - 1].toFixed(2),
    ].join(","),
  );
}

console.log(
  "phase,requests,span_ms,summed_job_ms,mean_active,peak_active,zero_active_ms,p95_job_ms",
);
summarize(
  "parse",
  rows.filter(({ phase }) => phase === "parse"),
);
summarize(
  "namespace",
  rows.filter(({ phase }) => phase === "namespace"),
);
summarize(
  "compile",
  rows.filter(({ phase }) => phase === "interface" || phase === "implementation"),
);

console.log("longest compile jobs:");
rows
  .filter(({ phase }) => phase === "interface" || phase === "implementation")
  .sort((a, b) => (b.end - b.start) - (a.end - a.start))
  .slice(0, 5)
  .forEach(({ phase, cwd, input, start, end }) => {
    console.log(`${((end - start) * 1000).toFixed(1)} ms ${phase} ${cwd}/${input}`);
  });
