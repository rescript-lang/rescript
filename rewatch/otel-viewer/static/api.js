export async function fetchTraces() {
  const res = await fetch("/api/traces");
  return res.json();
}

export async function fetchRootSpans(traceId) {
  const res = await fetch(`/api/traces/${traceId}/roots`);
  return res.json();
}

export async function fetchChildren(spanId) {
  const res = await fetch(`/api/spans/${spanId}/children`);
  return res.json();
}

export async function fetchSpanDetail(spanId) {
  const res = await fetch(`/api/spans/${spanId}`);
  return res.json();
}

export async function fetchExport(spanIds) {
  const res = await fetch("/api/export", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ span_ids: spanIds }),
  });
  return res.json();
}
