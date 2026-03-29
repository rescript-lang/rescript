from contextlib import asynccontextmanager
from pathlib import Path

from fastapi import FastAPI, Request, Response
from fastapi.responses import HTMLResponse
from fastapi.staticfiles import StaticFiles
from fastapi.templating import Jinja2Templates
from opentelemetry.proto.collector.trace.v1.trace_service_pb2 import (
    ExportTraceServiceRequest,
    ExportTraceServiceResponse,
)

import db


@asynccontextmanager
async def lifespan(app: FastAPI):
    db.init_db()
    yield


app = FastAPI(lifespan=lifespan)
app.mount(
    "/static",
    StaticFiles(directory=str(Path(__file__).parent / "static")),
    name="static",
)
templates = Jinja2Templates(directory=str(Path(__file__).parent / "templates"))

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


def extract_attribute_value(value) -> str:
    """Extract a scalar value from an OTLP KeyValue's AnyValue."""
    if value.HasField("string_value"):
        return value.string_value
    if value.HasField("int_value"):
        return str(value.int_value)
    if value.HasField("double_value"):
        return str(value.double_value)
    if value.HasField("bool_value"):
        return str(value.bool_value)
    return str(value)


# --- OTLP Receiver ---


@app.post("/v1/traces")
async def receive_traces(request: Request):
    body = await request.body()
    req = ExportTraceServiceRequest()
    req.ParseFromString(body)

    spans_to_insert = []

    for resource_spans in req.resource_spans:
        # Extract service name from resource attributes
        service_name = None
        for attr in resource_spans.resource.attributes:
            if attr.key == "service.name":
                service_name = extract_attribute_value(attr.value)
                break

        for scope_spans in resource_spans.scope_spans:
            for span in scope_spans.spans:
                trace_id = span.trace_id.hex()
                span_id = span.span_id.hex()
                raw_parent = span.parent_span_id
                parent_span_id = (
                    raw_parent.hex()
                    if raw_parent and raw_parent != b"\x00" * 8
                    else None
                )

                # Attributes
                attributes = {}
                for attr in span.attributes:
                    attributes[attr.key] = extract_attribute_value(attr.value)

                # Events
                events = []
                for event in span.events:
                    ev_attrs = {}
                    for attr in event.attributes:
                        ev_attrs[attr.key] = extract_attribute_value(attr.value)
                    events.append(
                        {
                            "name": event.name,
                            "timestamp_ns": event.time_unix_nano,
                            "attributes": ev_attrs,
                        }
                    )

                duration_ms = (
                    span.end_time_unix_nano - span.start_time_unix_nano
                ) / 1e6

                spans_to_insert.append(
                    {
                        "span_id": span_id,
                        "trace_id": trace_id,
                        "parent_span_id": parent_span_id,
                        "name": span.name,
                        "service_name": service_name,
                        "start_time_ns": span.start_time_unix_nano,
                        "end_time_ns": span.end_time_unix_nano,
                        "duration_ms": duration_ms,
                        "status": STATUS_CODE_MAP.get(span.status.code, "UNSET"),
                        "kind": SPAN_KIND_MAP.get(span.kind, "UNSPECIFIED"),
                        "attributes": attributes,
                        "events": events,
                    }
                )

    if spans_to_insert:
        db.insert_spans(spans_to_insert)
        db.propagate_errors()
        db.upsert_traces_from_spans()

    # Return protobuf response
    resp = ExportTraceServiceResponse()
    return Response(
        content=resp.SerializeToString(),
        media_type="application/x-protobuf",
    )


# --- Web UI ---


@app.get("/", response_class=HTMLResponse)
async def index(request: Request):
    return templates.TemplateResponse("index.html", {"request": request})


@app.get("/traces/{trace_id}", response_class=HTMLResponse)
async def trace_detail(request: Request, trace_id: str):
    return templates.TemplateResponse("index.html", {"request": request})


# --- API ---


@app.get("/health")
async def health():
    return {"status": "ok"}


@app.get("/api/traces")
async def api_traces():
    return db.get_traces()


@app.get("/api/traces/{trace_id}/roots")
async def api_trace_roots(trace_id: str):
    return db.get_root_spans(trace_id)


@app.get("/api/spans/{span_id}/children")
async def api_span_children(span_id: str):
    return db.get_children(span_id)


@app.get("/api/spans/{span_id}")
async def api_span_detail(span_id: str):
    detail = db.get_span_detail(span_id)
    if detail is None:
        return Response(status_code=404)
    return detail


@app.post("/api/export")
async def api_export(request: Request):
    body = await request.json()
    span_ids = body.get("span_ids", [])
    exported = []
    trace_id = None
    for span_id in span_ids:
        tree = db.get_subtree_clean(span_id)
        if tree is not None:
            exported.append(tree)
        # Get trace_id from the span row directly
        if trace_id is None:
            detail = db.get_span_detail(span_id)
            if detail is not None:
                trace_id = detail["trace_id"]
    return {"trace_id": trace_id, "exported_spans": exported}


@app.get("/api/spans/{span_id}/context")
async def api_span_context(span_id: str):
    result = db.get_context_spans(span_id)
    if result is None:
        return Response(status_code=404)
    return result


@app.get("/api/spans/{span_id}/flush")
async def api_span_flush(span_id: str):
    result = db.get_flush_summary(span_id)
    if result is None:
        return Response(status_code=404)
    return result


if __name__ == "__main__":
    import sys

    import uvicorn

    reload = "--watch" in sys.argv
    uvicorn.run(
        "server:app",
        host="0.0.0.0",
        port=4707,
        reload=reload,
        reload_dirs=[str(Path(__file__).parent)],
    )
