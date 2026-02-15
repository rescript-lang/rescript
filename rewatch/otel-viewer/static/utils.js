export function formatDuration(ms) {
  if (ms < 1) return `${(ms * 1000).toFixed(0)}us`;
  if (ms < 1000) return `${ms.toFixed(1)}ms`;
  return `${(ms / 1000).toFixed(2)}s`;
}

export function esc(str) {
  const d = document.createElement("div");
  d.textContent = str;
  return d.innerHTML;
}

export function showToast(message) {
  const container = document.getElementById("toast-container");
  const toast = document.createElement("div");
  toast.className =
    "toast bg-gray-800 text-white px-4 py-2 rounded shadow-lg text-sm";
  toast.textContent = message;
  container.appendChild(toast);
  setTimeout(() => toast.remove(), 2200);
}
