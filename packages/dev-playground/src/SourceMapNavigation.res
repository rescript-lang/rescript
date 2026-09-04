type traceMap
type rawMapping

type position = {
  line: int,
  col: int,
}

type originalPosition = {
  source: string,
  position: position,
}

type mapping = {
  generated: position,
  original: option<originalPosition>,
}

@new @module("@jridgewell/trace-mapping")
external makeTraceMap: string => traceMap = "TraceMap"

@module("@jridgewell/trace-mapping")
external eachMapping: (traceMap, rawMapping => unit) => unit = "eachMapping"

module RawMapping = {
  @get external generatedLine: rawMapping => int = "generatedLine"
  @get external generatedColumn: rawMapping => int = "generatedColumn"
  @get @return(nullable) external source: rawMapping => option<string> = "source"
  @get @return(nullable) external originalLine: rawMapping => option<int> = "originalLine"
  @get @return(nullable) external originalColumn: rawMapping => option<int> = "originalColumn"
}

let decode = sourceMap => {
  let mappings: array<mapping> = []
  try {
    sourceMap
    ->makeTraceMap
    ->eachMapping(rawMapping => {
      let original = switch (
        rawMapping->RawMapping.source,
        rawMapping->RawMapping.originalLine,
        rawMapping->RawMapping.originalColumn,
      ) {
      | (Some(source), Some(line), Some(col)) => Some({source, position: {line, col}})
      | _ => None
      }
      mappings->Array.push({
        generated: {
          line: rawMapping->RawMapping.generatedLine,
          col: rawMapping->RawMapping.generatedColumn,
        },
        original,
      })
    })
    mappings
  } catch {
  | _ => []
  }
}

let isCurrentSource = (compiledSource, currentSource) => compiledSource === currentSource

let decodeForSource = (sourceMap, compiledSource, currentSource) =>
  isCurrentSource(compiledSource, currentSource) ? decode(sourceMap) : []

let distance = (left: position, right: position) => {
  let lineDistance = left.line - right.line
  let colDistance = left.col - right.col
  let lineDistance = lineDistance < 0 ? -lineDistance : lineDistance
  let colDistance = colDistance < 0 ? -colDistance : colDistance
  lineDistance * 1000000 + colDistance
}

let generatedForOriginal = (mappings, position) => {
  let closest: ref<option<(int, mapping)>> = ref(None)
  mappings->Array.forEach(mapping =>
    switch mapping.original {
    | Some(original) if original.position.line === position.line => {
        let nextDistance = distance(original.position, position)
        switch closest.contents {
        | None => closest := Some((nextDistance, mapping))
        | Some((currentDistance, _)) if nextDistance < currentDistance =>
          closest := Some((nextDistance, mapping))
        | Some(_) => ()
        }
      }
    | Some(_) | None => ()
    }
  )
  closest.contents->Option.map(((_, mapping)) => mapping)
}
