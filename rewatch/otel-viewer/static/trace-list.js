import { fetchTraces } from "./api.js";
import { esc, formatDuration } from "./utils.js";

export async function renderTraceList(onSelectTrace) {
  const traces = await fetchTraces();
  const tbody = document.getElementById("trace-table-body");
  const empty = document.getElementById("trace-empty");

  if (traces.length === 0) {
    empty.classList.remove("hidden");
    tbody.innerHTML = "";
    return;
  }
  empty.classList.add("hidden");

  tbody.innerHTML = "";
  for (const t of traces) {
    const tr = document.createElement("tr");
    const errorBorder = t.has_error ? "border-l-4 border-l-red-500" : "";
    tr.className = `border-b border-gray-200 hover:bg-gray-50 cursor-pointer ${errorBorder}`;
    tr.dataset.traceId = t.trace_id;
    tr.onclick = () => onSelectTrace(t.trace_id);

    const time = new Date(t.start_time_ns / 1e6);
    const timeStr = time.toLocaleString();

    tr.innerHTML = `
      <td class="py-2 px-3 text-gray-500">${timeStr}</td>
      <td class="py-2 px-3 text-gray-900">${esc(t.root_span_name || "(unknown)")}</td>
      <td class="py-2 px-3 text-right">${formatDuration(t.duration_ms)}</td>
      <td class="py-2 px-3 text-right text-gray-500">${t.span_count}</td>
    `;
    tbody.appendChild(tr);
  }
}
