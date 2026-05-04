type abortSignal // WebAPI.EventAPI.abortSignal

type nodeStream // NodeJs.Stream.stream

type readableStream // WebAPI.FileAPI.readableStream

type postponedState

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

type staticResult = {prelude: readableStream, postponed: postponedState}

/**
prerender renders a React tree to a static HTML string using a Web Stream.
[Read more on the React Documentation](https://react.dev/reference/react-dom/static/prerender)
*/
@module("react-dom/static")
external prerender: (React.element, ~options: prerenderOptions<'error>=?) => promise<staticResult> =
  "prerender"

/**
resumeAndPrerender continues a prerendered React tree to a static HTML string using a Web Stream.
[Read more on the React Documentation](https://react.dev/reference/react-dom/static/resumeAndPrerender)
*/
@module("react-dom/static")
external resumeAndPrerender: (
  React.element,
  postponedState,
  ~options: prerenderOptions<'error>=?,
) => promise<staticResult> = "resumeAndPrerender"

type staticResultNode = {prelude: nodeStream, postponed: postponedState}

/**
prerenderToNodeStream renders a React tree to a static HTML string using a Node.js Stream.
[Read more on the React Documentation](https://react.dev/reference/react-dom/static/prerenderToNodeStream)
*/
@module("react-dom/static")
external prerenderToNodeStream: (
  React.element,
  ~options: prerenderOptions<'error>=?,
) => promise<staticResultNode> = "prerenderToNodeStream"

/**
resumeAndPrerenderToNodeStream continues a prerendered React tree to a static HTML string using a a Node.js Stream.
[Read more on the React Documentation](https://react.dev/reference/react-dom/static/resumeAndPrerenderToNodeStream)
*/
@module("react-dom/static")
external resumeAndPrerenderToNodeStream: (
  React.element,
  postponedState,
  ~options: prerenderOptions<'error>=?,
) => promise<staticResultNode> = "resumeAndPrerenderToNodeStream"
