import { fetchRootSpans, fetchChildren, fetchSpanDetail } from "./api.js";
import { state, resetTraceState } from "./state.js";
import { esc, formatDuration } from "./utils.js";
import { updateExportBar } from "./export.js";

export async function openTrace(traceId) {
  state.currentTraceId = traceId;
  state.view = "detail";
  resetTraceState();
  state.currentTraceId = traceId;
  updateExportBar();

  document.getElementById("trace-list-view").classList.add("hidden");
  document.getElementById("trace-detail-view").classList.remove("hidden");
  document.getElementById("back-btn").classList.remove("hidden");

  const tree = document.getElementById("span-tree");
  tree.innerHTML = '<p class="text-gray-400 text-sm">Loading...</p>';

  const roots = await fetchRootSpans(traceId);
  tree.innerHTML = "";
  for (const span of roots) {
    tree.appendChild(createSpanRow(span, 0));
  }

  // Auto-expand the first root span
  if (roots.length > 0 && roots[0].has_children) {
    await toggleExpand(roots[0].span_id, 0);
  }
}

function createSpanRow(span, depth) {
  const container = document.createElement("div");
  container.dataset.spanId = span.span_id;
  container.dataset.depth = depth;

  const isSelected = state.selectedSpanIds.has(span.span_id);

  const row = document.createElement("div");
  row.className = `flex items-center py-1.5 px-2 hover:bg-gray-50 border-b border-gray-200 ${isSelected ? "bg-blue-50" : ""}`;
  row.style.paddingLeft = `${depth * 24 + 8}px`;

  const errorDot = span.has_error
    ? `<span class="w-2 h-2 rounded-full bg-red-500 shrink-0 mr-1.5" title="Contains errors"></span>`
    : "";

  row.innerHTML = `
    <input type="checkbox" ${isSelected ? "checked" : ""}
      class="mr-2 accent-blue-600 cursor-pointer"
      data-select-span="${span.span_id}"
    />
    <button class="w-5 h-5 flex items-center justify-center text-gray-400 hover:text-gray-700 mr-1 cursor-pointer shrink-0 ${span.has_children ? "" : "invisible"}"
      data-expand-btn="${span.span_id}" data-depth="${depth}">
      ${state.expandedSpans.has(span.span_id) ? "&#9660;" : "&#9654;"}
    </button>
    ${errorDot}
    <span class="${span.has_error ? "text-red-700" : "text-gray-900"} truncate">${esc(span.name)}</span>
    <button class="ml-2 text-xs text-gray-400 hover:text-blue-600 cursor-pointer shrink-0"
      data-detail-btn="${span.span_id}"
      title="Show attributes and events">
      info
    </button>
    <span class="ml-auto text-xs text-gray-400 shrink-0 pl-4">${formatDuration(span.duration_ms)}</span>
  `;

  // Event listeners
  const checkbox = row.querySelector(`[data-select-span="${span.span_id}"]`);
  checkbox.addEventListener("change", () =>
    toggleSelect(span.span_id, checkbox.checked),
  );

  const expandBtn = row.querySelector(`[data-expand-btn="${span.span_id}"]`);
  if (span.has_children) {
    expandBtn.addEventListener("click", () =>
      toggleExpand(span.span_id, depth),
    );
  }

  const detailBtn = row.querySelector(`[data-detail-btn="${span.span_id}"]`);
  detailBtn.addEventListener("click", () => toggleDetail(span.span_id));

  container.appendChild(row);

  // Detail panel (hidden by default)
  const detail = document.createElement("div");
  detail.dataset.detailFor = span.span_id;
  detail.className = "hidden";
  detail.style.paddingLeft = `${depth * 24 + 36}px`;
  detail.innerHTML = `<p class="text-xs text-gray-400 py-1">Loading...</p>`;
  container.appendChild(detail);

  return container;
}

async function toggleExpand(spanId, depth) {
  const btn = document.querySelector(`[data-expand-btn="${spanId}"]`);

  if (state.expandedSpans.has(spanId)) {
    state.expandedSpans.delete(spanId);
    btn.innerHTML = "&#9654;";
    removeDescendants(spanId);
  } else {
    state.expandedSpans.add(spanId);
    btn.innerHTML = "&#9660;";

    let children = state.loadedChildren[spanId];
    if (!children) {
      children = await fetchChildren(spanId);
      state.loadedChildren[spanId] = children;
    }

    const parentContainer = document.querySelector(
      `[data-span-id="${spanId}"]`,
    );
    const childDepth = depth + 1;
    let insertAfter = parentContainer;
    for (const child of children) {
      const childRow = createSpanRow(child, childDepth);
      insertAfter.after(childRow);
      insertAfter = childRow;
    }
  }
}

function removeDescendants(spanId) {
  const parentContainer = document.querySelector(`[data-span-id="${spanId}"]`);
  const parentDepth = parseInt(parentContainer.dataset.depth);

  let next = parentContainer.nextElementSibling;
  while (next && parseInt(next.dataset.depth) > parentDepth) {
    const toRemove = next;
    const childId = toRemove.dataset.spanId;
    state.expandedSpans.delete(childId);
    state.detailOpen.delete(childId);
    next = next.nextElementSibling;
    toRemove.remove();
  }
}

async function toggleDetail(spanId) {
  const panel = document.querySelector(`[data-detail-for="${spanId}"]`);
  if (!panel) return;

  if (state.detailOpen.has(spanId)) {
    state.detailOpen.delete(spanId);
    panel.classList.add("hidden");
    return;
  }

  state.detailOpen.add(spanId);
  panel.classList.remove("hidden");

  const span = await fetchSpanDetail(spanId);

  let html = "";

  // Attributes
  const attrs = span.attributes || {};
  const attrKeys = Object.keys(attrs);
  if (attrKeys.length > 0) {
    html += `<div class="py-1.5 px-3 bg-gray-50 rounded mb-1 text-xs border border-gray-200">`;
    html += `<div class="font-semibold text-gray-600 mb-1">Attributes</div>`;
    for (const key of attrKeys) {
      html += `<div class="flex gap-2"><span class="text-gray-500">${esc(key)}:</span> <span class="text-gray-800">${esc(attrs[key])}</span></div>`;
    }
    html += `</div>`;
  }

  // Events
  const events = span.events || [];
  if (events.length > 0) {
    html += `<div class="py-1.5 px-3 bg-amber-50 rounded mb-1 text-xs border border-amber-200">`;
    html += `<div class="font-semibold text-amber-700 mb-1">Events (${events.length})</div>`;
    for (const ev of events) {
      html += `<div class="mb-1">`;
      html += `<span class="text-amber-800 font-medium">${esc(ev.name)}</span>`;
      const evAttrs = ev.attributes || {};
      const evAttrKeys = Object.keys(evAttrs);
      if (evAttrKeys.length > 0) {
        for (const key of evAttrKeys) {
          html += `<div class="ml-3 text-gray-600"><span class="text-gray-500">${esc(key)}:</span> ${esc(evAttrs[key])}</div>`;
        }
      }
      html += `</div>`;
    }
    html += `</div>`;
  }

  if (!html) {
    html = `<div class="py-1 px-3 text-xs text-gray-400 italic">No attributes or events</div>`;
  }

  panel.innerHTML = html;
}

function toggleSelect(spanId, checked) {
  if (checked) {
    state.selectedSpanIds.add(spanId);
  } else {
    state.selectedSpanIds.delete(spanId);
  }
  const container = document.querySelector(`[data-span-id="${spanId}"]`);
  if (container) {
    const row = container.firstElementChild;
    row.classList.toggle("bg-blue-50", checked);
  }
  updateExportBar();
}

export function clearSelection() {
  state.selectedSpanIds.clear();
  document
    .querySelectorAll('#span-tree input[type="checkbox"]')
    .forEach((cb) => {
      cb.checked = false;
    });
  document
    .querySelectorAll("#span-tree [data-span-id]")
    .forEach((container) => {
      container.firstElementChild.classList.remove("bg-blue-50");
    });
  updateExportBar();
}
