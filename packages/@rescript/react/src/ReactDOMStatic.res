type abortSignal // WebAPI.EventAPI.abortSignal

type nodeStream // NodeJs.Stream.stream

type readableStream // WebAPI.FileAPI.readableStream

type prerenderOptions<'error> = {
  bootstrapScriptContent?: string,
  bootstrapScripts?: array<string>,
  bootstrapModules?: array<string>,
  identifierPrefix?: string,
  namespaceURI?: string,
  onError?: 'error => unit,
  progressiveChunkSize?: int,
  signal?: abortSignal,
}

type staticResult = {prelude: readableStream}

@module("react-dom/static")
external prerender: (React.element, ~options: prerenderOptions<'error>=?) => promise<staticResult> =
  "prerender"

type staticResultNode = {prelude: nodeStream}

@module("react-dom/static")
external prerenderToNodeStream: (
  React.element,
  ~options: prerenderOptions<'error>=?,
) => promise<staticResultNode> = "prerenderToNodeStream"
