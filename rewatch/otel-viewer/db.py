import json
import sqlite3
from pathlib import Path

DB_PATH = Path(__file__).parent / "traces.db"

SCHEMA = """
CREATE TABLE IF NOT EXISTS traces (
    trace_id TEXT PRIMARY KEY,
    root_span_name TEXT,
    start_time_ns INTEGER,
    duration_ms REAL,
    span_count INTEGER,
    has_error INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS spans (
    span_id TEXT PRIMARY KEY,
    trace_id TEXT NOT NULL,
    parent_span_id TEXT,
    name TEXT NOT NULL,
    service_name TEXT,
    start_time_ns INTEGER NOT NULL,
    end_time_ns INTEGER NOT NULL,
    duration_ms REAL NOT NULL,
    status TEXT,
    kind TEXT,
    has_error INTEGER NOT NULL DEFAULT 0
);

CREATE TABLE IF NOT EXISTS span_attributes (
    span_id TEXT NOT NULL,
    key TEXT NOT NULL,
    value TEXT,
    FOREIGN KEY (span_id) REFERENCES spans(span_id)
);

CREATE TABLE IF NOT EXISTS span_events (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    span_id TEXT NOT NULL,
    name TEXT NOT NULL,
    timestamp_ns INTEGER,
    attributes_json TEXT,
    FOREIGN KEY (span_id) REFERENCES spans(span_id)
);

CREATE INDEX IF NOT EXISTS idx_spans_trace_id ON spans(trace_id);
CREATE INDEX IF NOT EXISTS idx_spans_parent ON spans(parent_span_id);
CREATE INDEX IF NOT EXISTS idx_span_attrs_span ON span_attributes(span_id);
CREATE INDEX IF NOT EXISTS idx_span_events_span ON span_events(span_id);
"""

SPAN_KIND_MAP = {
    0: "UNSPECIFIED",
    1: "INTERNAL",
    2: "SERVER",
    3: "CLIENT",
    4: "PRODUCER",
    5: "CONSUMER",
}

STATUS_CODE_MAP = {
    0: "UNSET",
    1: "OK",
    2: "ERROR",
}


def get_conn() -> sqlite3.Connection:
    conn = sqlite3.connect(str(DB_PATH))
    conn.row_factory = sqlite3.Row
    conn.execute("PRAGMA journal_mode=WAL")
    conn.execute("PRAGMA foreign_keys=ON")
    return conn


def init_db():
    conn = get_conn()
    conn.executescript(SCHEMA)
    conn.close()


def _span_has_own_error(s: dict) -> bool:
    """Check if a span itself indicates an error (status, error_count, or error attribute)."""
    if s.get("status") == "ERROR":
        return True
    attrs = s.get("attributes", {})
    # Spans like build.compile_error carry an "error" attribute with the message
    if "error" in attrs:
        return True
    error_count = attrs.get("error_count", "0")
    try:
        return int(error_count) > 0
    except (ValueError, TypeError):
        return False


def insert_spans(spans: list[dict]):
    conn = get_conn()
    try:
        for s in spans:
            has_error = 1 if _span_has_own_error(s) else 0
            conn.execute(
                """INSERT OR REPLACE INTO spans
                   (span_id, trace_id, parent_span_id, name, service_name,
                    start_time_ns, end_time_ns, duration_ms, status, kind, has_error)
                   VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)""",
                (
                    s["span_id"],
                    s["trace_id"],
                    s.get("parent_span_id"),
                    s["name"],
                    s.get("service_name"),
                    s["start_time_ns"],
                    s["end_time_ns"],
                    s["duration_ms"],
                    s.get("status"),
                    s.get("kind"),
                    has_error,
                ),
            )
            # Attributes
            if s.get("attributes"):
                conn.execute(
                    "DELETE FROM span_attributes WHERE span_id = ?", (s["span_id"],)
                )
                for key, value in s["attributes"].items():
                    conn.execute(
                        "INSERT INTO span_attributes (span_id, key, value) VALUES (?, ?, ?)",
                        (s["span_id"], key, value),
                    )
            # Events
            if s.get("events"):
                conn.execute(
                    "DELETE FROM span_events WHERE span_id = ?", (s["span_id"],)
                )
                for ev in s["events"]:
                    conn.execute(
                        """INSERT INTO span_events (span_id, name, timestamp_ns, attributes_json)
                           VALUES (?, ?, ?, ?)""",
                        (
                            s["span_id"],
                            ev["name"],
                            ev.get("timestamp_ns"),
                            json.dumps(ev.get("attributes", {})),
                        ),
                    )
        conn.commit()
    finally:
        conn.close()


def propagate_errors():
    """Walk error spans upward, marking all ancestors with has_error=1."""
    conn = get_conn()
    try:
        # Find all span_ids that have has_error=1
        error_spans = conn.execute(
            "SELECT span_id, parent_span_id FROM spans WHERE has_error = 1"
        ).fetchall()
        # Walk each error span upward to mark ancestors
        to_mark: set[str] = set()
        for row in error_spans:
            parent_id = row["parent_span_id"]
            while parent_id:
                if parent_id in to_mark:
                    break  # already processed this ancestor chain
                to_mark.add(parent_id)
                parent_row = conn.execute(
                    "SELECT parent_span_id FROM spans WHERE span_id = ?",
                    (parent_id,),
                ).fetchone()
                parent_id = parent_row["parent_span_id"] if parent_row else None
        if to_mark:
            # Batch update — SQLite handles IN with reasonable set sizes fine
            placeholders = ",".join("?" for _ in to_mark)
            conn.execute(
                f"UPDATE spans SET has_error = 1 WHERE span_id IN ({placeholders})",  # noqa: S608
                list(to_mark),
            )
            conn.commit()
    finally:
        conn.close()


def upsert_traces_from_spans():
    """Recompute trace summary rows from the spans table."""
    conn = get_conn()
    try:
        conn.execute(
            """INSERT OR REPLACE INTO traces
               (trace_id, root_span_name, start_time_ns, duration_ms, span_count, has_error)
               SELECT
                   s.trace_id,
                   (SELECT name FROM spans WHERE trace_id = s.trace_id
                    AND (parent_span_id IS NULL OR parent_span_id = '')
                    ORDER BY start_time_ns LIMIT 1),
                   MIN(s.start_time_ns),
                   (MAX(s.end_time_ns) - MIN(s.start_time_ns)) / 1e6,
                   COUNT(*),
                   MAX(s.has_error)
               FROM spans s
               GROUP BY s.trace_id"""
        )
        conn.commit()
    finally:
        conn.close()


def get_traces() -> list[dict]:
    """Return only finished traces (those that have a true root span with no parent)."""
    conn = get_conn()
    rows = conn.execute(
        """SELECT t.* FROM traces t
           WHERE EXISTS (
               SELECT 1 FROM spans s
               WHERE s.trace_id = t.trace_id
               AND (s.parent_span_id IS NULL OR s.parent_span_id = '')
           )
           ORDER BY t.start_time_ns DESC"""
    ).fetchall()
    conn.close()
    return [dict(r) for r in rows]


def get_root_spans(trace_id: str) -> list[dict]:
    conn = get_conn()
    rows = conn.execute(
        """SELECT * FROM spans
           WHERE trace_id = ? AND (parent_span_id IS NULL OR parent_span_id = '')
           ORDER BY start_time_ns""",
        (trace_id,),
    ).fetchall()
    conn.close()
    result = []
    for r in rows:
        d = dict(r)
        d["has_children"] = _has_children_check(trace_id, d["span_id"])
        result.append(d)
    return result


def _has_children_check(trace_id: str, span_id: str) -> bool:
    conn = get_conn()
    row = conn.execute(
        "SELECT 1 FROM spans WHERE parent_span_id = ? LIMIT 1", (span_id,)
    ).fetchone()
    conn.close()
    return row is not None


def get_children(span_id: str) -> list[dict]:
    conn = get_conn()
    rows = conn.execute(
        "SELECT * FROM spans WHERE parent_span_id = ? ORDER BY start_time_ns",
        (span_id,),
    ).fetchall()
    conn.close()
    result = []
    for r in rows:
        d = dict(r)
        d["has_children"] = _has_children_check(d["trace_id"], d["span_id"])
        result.append(d)
    return result


def get_span_detail(span_id: str) -> dict | None:
    conn = get_conn()
    row = conn.execute("SELECT * FROM spans WHERE span_id = ?", (span_id,)).fetchone()
    if row is None:
        conn.close()
        return None
    d = dict(row)
    attrs = conn.execute(
        "SELECT key, value FROM span_attributes WHERE span_id = ?", (span_id,)
    ).fetchall()
    d["attributes"] = {a["key"]: a["value"] for a in attrs}
    events = conn.execute(
        "SELECT name, timestamp_ns, attributes_json FROM span_events WHERE span_id = ? ORDER BY timestamp_ns",
        (span_id,),
    ).fetchall()
    d["events"] = [
        {
            "name": e["name"],
            "timestamp_ns": e["timestamp_ns"],
            "attributes": json.loads(e["attributes_json"])
            if e["attributes_json"]
            else {},
        }
        for e in events
    ]
    conn.close()
    return d


def get_subtree(span_id: str) -> dict | None:
    """Recursively get a span and all its descendants as a nested tree."""
    detail = get_span_detail(span_id)
    if detail is None:
        return None
    children_rows = get_children(span_id)
    detail["children"] = [get_subtree(c["span_id"]) for c in children_rows]
    detail["children"] = [c for c in detail["children"] if c is not None]
    return detail


# --- Clean export (LLM-friendly) ---

# Attribute keys that are tracing/instrumentation internals, not domain info
_NOISE_ATTR_KEYS = {
    "code.filepath",
    "code.namespace",
    "code.lineno",
    "thread.id",
    "thread.name",
    "busy_ns",
    "idle_ns",
    "level",
    "target",
}


def _clean_attributes(attrs: dict) -> dict:
    """Remove instrumentation noise from an attribute dict."""
    return {k: v for k, v in attrs.items() if k not in _NOISE_ATTR_KEYS}


def _clean_event(ev: dict) -> dict:
    """Clean a single span event for export."""
    cleaned = {"name": ev["name"]}
    if ev.get("attributes"):
        clean_attrs = _clean_attributes(ev["attributes"])
        if clean_attrs:
            cleaned["attributes"] = clean_attrs
    return cleaned


def _clean_span_for_export(span: dict) -> dict:
    """Strip a full span detail dict down to what's useful for an LLM."""
    out: dict = {"name": span["name"]}

    out["duration_ms"] = span["duration_ms"]

    # Only include error indicators when present
    if span.get("has_error"):
        out["has_error"] = True
    if span.get("status") and span["status"] not in ("UNSET", None):
        out["status"] = span["status"]

    # Attributes — strip instrumentation noise
    if span.get("attributes"):
        clean_attrs = _clean_attributes(span["attributes"])
        if clean_attrs:
            out["attributes"] = clean_attrs

    # Events — strip noise from each event, omit empty list
    if span.get("events"):
        clean_events = [_clean_event(ev) for ev in span["events"]]
        # Drop events that are just a name with no meaningful attributes
        if clean_events:
            out["events"] = clean_events

    # Children — recurse
    if span.get("children"):
        out["children"] = [_clean_span_for_export(c) for c in span["children"]]

    return out


def get_subtree_clean(span_id: str) -> dict | None:
    """Get a span subtree cleaned up for LLM export."""
    tree = get_subtree(span_id)
    if tree is None:
        return None
    return _clean_span_for_export(tree)
