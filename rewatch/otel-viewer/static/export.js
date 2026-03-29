import { fetchExport } from "./api.js";
import { state } from "./state.js";
import { showToast } from "./utils.js";

export function updateExportBar() {
  const bar = document.getElementById("export-bar");
  const count = document.getElementById("selected-count");
  if (state.selectedSpanIds.size > 0) {
    bar.classList.remove("hidden");
    count.textContent = `${state.selectedSpanIds.size} span${state.selectedSpanIds.size > 1 ? "s" : ""} selected`;
  } else {
    bar.classList.add("hidden");
  }
}

export async function exportSelected() {
  const spanIds = Array.from(state.selectedSpanIds);
  const data = await fetchExport(spanIds);
  const text = JSON.stringify(data);
  await navigator.clipboard.writeText(text);
  showToast("Copied to clipboard");
}
