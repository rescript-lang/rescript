@module("react-dom/server")
external renderToString: React.element => string = "renderToString"

@module("react-dom/server")
external renderToStaticMarkup: React.element => string = "renderToStaticMarkup"

type resumeOptions<'error> = {
  nonce?: string,
  signal?: ReactDOMStatic.abortSignal,
  onError?: 'error => unit,
}

/**
resume streams a pre-rendered React tree to a Readable Web Stream.
[Read more on the React Documentation](https://react.dev/reference/react-dom/server/resume)
*/
@module("react-dom/server")
external resume: (
  React.element,
  ReactDOMStatic.postponedState,
  ~options: resumeOptions<'error>=?,
) => promise<ReactDOMStatic.readableStream> = "resume"

/**
resumeToPipeableStream streams a pre-rendered React tree  to a pipeable Node.js Stream.
[Read more on the React Documentation](https://react.dev/reference/react-dom/server/resumeToPipeableStream)
*/
@module("react-dom/server")
external resumeToPipeableStream: (
  React.element,
  ReactDOMStatic.postponedState,
  ~options: resumeOptions<'error>=?,
) => promise<ReactDOMStatic.nodeStream> = "resumeToPipeableStream"
