import { state } from "./state.js";
import { renderTraceList } from "./trace-list.js";
import { openTrace, clearSelection } from "./span-tree.js";
import { exportSelected } from "./export.js";

let pollInterval = null;

function onSelectTrace(traceId) {
  navigation.navigate(`/traces/${traceId}`);
}

function updateLastPolled() {
  const el = document.getElementById("last-polled");
  el.textContent = `Last updated ${new Date().toLocaleTimeString()}`;
  el.classList.remove("hidden");
}

function startPolling() {
  stopPolling();
  pollInterval = setInterval(async () => {
    await renderTraceList(onSelectTrace);
    updateLastPolled();
  }, 30_000);
}

function stopPolling() {
  if (pollInterval !== null) {
    clearInterval(pollInterval);
    pollInterval = null;
  }
}

function showTraceList() {
  state.view = "traces";
  state.currentTraceId = null;
  document.getElementById("trace-list-view").classList.remove("hidden");
  document.getElementById("trace-detail-view").classList.add("hidden");
  document.getElementById("back-btn").classList.add("hidden");
  document.getElementById("export-bar").classList.add("hidden");
  updateLastPolled();
  renderTraceList(onSelectTrace).then(updateLastPolled);
  startPolling();
}

function showTrace(traceId) {
  stopPolling();
  document.getElementById("last-polled").classList.add("hidden");
  openTrace(traceId);
}

// Use the Navigation API for client-side routing
navigation.addEventListener("navigate", (event) => {
  const url = new URL(event.destination.url);
  const traceMatch = url.pathname.match(/^\/traces\/([a-f0-9]+)$/);

  if (url.pathname === "/") {
    event.intercept({
      handler() {
        showTraceList();
      },
    });
  } else if (traceMatch) {
    event.intercept({
      handler() {
        showTrace(traceMatch[1]);
      },
    });
  }
});

// Wire up static buttons
document.getElementById("back-btn").addEventListener("click", () => {
  navigation.navigate("/");
});
document.getElementById("export-btn").addEventListener("click", exportSelected);
document.getElementById("clear-btn").addEventListener("click", clearSelection);

// Initial load based on current URL
const initialMatch = location.pathname.match(/^\/traces\/([a-f0-9]+)$/);
if (initialMatch) {
  showTrace(initialMatch[1]);
} else {
  showTraceList();
}
