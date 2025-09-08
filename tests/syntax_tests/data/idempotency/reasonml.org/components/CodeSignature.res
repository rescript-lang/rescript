open Util.ReactStuff

@react.component
let make = (~code, ~lang) => {
  let highlighted = {
    open HighlightJs
    highlight(~lang, ~value=code)->valueGet
  }

  ReactDOMRe.createElementVariadic(
    "code",
    ~props=ReactDOMRe.objToDOMProps({
      "className": "text-lg font-bold md:texxt-xl whitespace-pre-line text-night-dark hljs sig lang-" ++
      lang,
      "dangerouslySetInnerHTML": {
        "__html": highlighted,
      },
    }),
    [],
  )
}
