import { state } from "./state.js";
import { renderTraceList } from "./trace-list.js";
import { openTrace, clearSelection } from "./span-tree.js";
import { exportSelected } from "./export.js";

function showTraceList() {
  state.view = "traces";
  state.currentTraceId = null;
  document.getElementById("trace-list-view").classList.remove("hidden");
  document.getElementById("trace-detail-view").classList.add("hidden");
  document.getElementById("back-btn").classList.add("hidden");
  document.getElementById("export-bar").classList.add("hidden");
  renderTraceList((traceId) => {
    navigation.navigate(`/traces/${traceId}`);
  });
}

function showTrace(traceId) {
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
