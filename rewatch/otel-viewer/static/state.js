export const state = {
  view: "traces", // "traces" | "detail"
  currentTraceId: null,
  selectedSpanIds: new Set(),
  loadedChildren: {}, // spanId -> [child spans] cache
  expandedSpans: new Set(),
  detailOpen: new Set(),
};

export function resetTraceState() {
  state.currentTraceId = null;
  state.selectedSpanIds.clear();
  state.loadedChildren = {};
  state.expandedSpans.clear();
  state.detailOpen.clear();
}
