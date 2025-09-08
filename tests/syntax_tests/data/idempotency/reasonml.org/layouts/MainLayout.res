module Link = Next.Link

@react.component
let make = (~children, ~components=Markdown.default) => {
  let minWidth = ReactDOMRe.Style.make(~minWidth="20rem", ())
  let overlayState = React.useState(() => false)

  <>
    <Meta />
    <div className="mb-32 mt-16">
      <div className="text-night text-lg">
        <Navigation overlayState />
        <div className="flex justify-center overflow-hidden">
          <main
            style=minWidth
            className="mt-32 lg:align-center w-full px-4 max-w-xl " /* ++ (isOpen ? " hidden" : "") */>
            <Mdx.Provider components>
              <div className="w-full max-w-lg"> children </div>
            </Mdx.Provider>
          </main>
        </div>
      </div>
    </div>
  </>
}
