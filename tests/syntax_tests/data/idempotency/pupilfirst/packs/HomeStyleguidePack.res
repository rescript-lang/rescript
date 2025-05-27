ReactDOMRe.renderToElementWithId(
  <HomeStyleguide__MarkdownSyntaxHighlightingPreview />,
  "styleguide__markdown-syntax-highlighting-root",
)

let stringRepeat = (n, s) => s->Array.make(n)->Array.to_list->String.concat("")

ReactDOMRe.renderToElementWithId(
  <DisablingCover disabled=true message="This element is disabled, and this is a custom message.">
    <div className="m-2 p-2 border-2 border-red-500">
      {"This is the element being disabled. "->stringRepeat(10)->React.string}
    </div>
  </DisablingCover>,
  "styleguide__disabling-cover-root",
)
