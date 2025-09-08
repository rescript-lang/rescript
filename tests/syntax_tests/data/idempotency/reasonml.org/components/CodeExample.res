open Util.ReactStuff

@react.component
let make = (~code: string, ~lang="text") => {
  let highlighted = {
    open HighlightJs
    highlight(~lang, ~value=code)->valueGet
  }

  let children = ReactDOMRe.createElementVariadic(
    "code",
    ~props=ReactDOMRe.objToDOMProps({
      "className": "wrap hljs lang-" ++ lang,
      "dangerouslySetInnerHTML": {
        "__html": highlighted,
      },
    }),
    [],
  )

  let langShortname = switch lang {
  | "ocaml" => "ml"
  | "reason" => "re"
  | "bash" => "sh"
  | "text" => ""
  | rest => rest
  }

  <div
    className="flex flex-col -mx-8 xs:mx-0 rounded-none xs:rounded border border-snow-dark bg-snow-light px-5 py-2 text-night-dark">
    <div className="flex self-end font-sans mb-4 text-sm font-bold text-night-light">
      {Js.String2.toUpperCase(langShortname)->s}
    </div>
    <div className="px-5 text-base pb-6 overflow-x-auto"> children </div>
  </div>
}
